struct CutSet
    balance_nodes::Dict{Int, Number}
    succ::Dict{Int, BitSet}
end

function sample_subgraph(graph::BaseGraph,
                         partition::Partition,
                         rng::AbstractRNG)
    """ Randomly sample two adjacent districts and returns them and their
        induced edges and nodes.
    """
    D₁, D₂ = sample_adjacent_districts_randomly(partition, rng)

    # take all their nodes
    nodes = union(partition.dist_nodes[D₁],
                  partition.dist_nodes[D₂])

    # get a subgraph of these two districts
    edges = induced_subgraph_edges(graph, collect(nodes))

    return D₁, D₂, edges, nodes
end


function bfs_traverse(mst::Dict{Int, Array{Int, 1}}, root::Int)::Tuple{Dict{Int, Int}, Dict{Int, BitSet}}
    """ Perform a BFS traversal of an MST from a specified root node and return
        dictionary of predecessors and successors.
    """
    q = Queue{Int}()
    visited = BitSet()
    enqueue!(q, root)
    predecessors = Dict{Int, Int}()
    successors = Dict{Int, BitSet}()

    while !isempty(q)
        n = dequeue!(q)
        push!(visited, n)
        successors[n] = BitSet()
        for neighbor in mst[n]
            if !(neighbor in visited)
                enqueue!(q, neighbor)
                push!(successors[n], neighbor)
                predecessors[neighbor] = n
            end
        end
    end
    return predecessors, successors
end


function memoized_balance_edges(graph::BaseGraph,
                                mst::Dict{Int, Vector{Int}},
                                mst_nodes::BitSet,
                                pop_constraint::PopulationConstraint,
                                D₁::Int,
                                D₂::Int,
                                subgraph_pop::Number)::CutSet
    node_pops = Dict(n => graph.populations[n] for n in mst_nodes)
    degrees = Dict(n => length(mst[n]) for n in mst_nodes)
    non_leaves = BitSet(filter(n -> degrees[n] > 1, mst_nodes))
    # if we can't find a root that has > degree 1, we'll start over
    if isempty(non_leaves) return Set() end
    root = rand(non_leaves)
    pred, succ = bfs_traverse(mst, root)
    tree_pops = Dict{Int, Number}()
    stack = Stack{Int}()
    push!(stack, root)
    while !isempty(stack)
        next_node = pop!(stack)
        if !(next_node in keys(tree_pops))
            if next_node in non_leaves
                if all(c in keys(tree_pops) for c in succ[next_node])
                    # The populations of all the node's children are known,
                    # so we can compute the total population of the tree rooted
                    # at the node.
                    tree_pops[next_node] = sum(tree_pops[c] for c in succ[next_node])
                    tree_pops[next_node] += node_pops[next_node]
                else
                    # Some children have still not been traversed. Come back later.
                    push!(stack, next_node)
                    for c in succ[next_node]
                        if !(c in keys(tree_pops))
                            push!(stack, c)
                        end
                    end
                end
            else
                # The current node is a leaf.
                # Compute its subtree population non-recursively.
                tree_pops[next_node] = node_pops[next_node]
            end
        end
    end

    balance_nodes = Dict{Int, Number}()
    for (node, pop) in tree_pops
        if pop_constraint.min_pop <= pop <= pop_constraint.max_pop
            balance_nodes[node] = pop
        end
    end
    return CutSet(balance_nodes, succ)
end


function attempt_leaf_contraction(graph::BaseGraph,
                                  mst::Dict{Int, Vector{Int}},
                                  mst_nodes::BitSet,
                                  pop_constraint::PopulationConstraint,
                                  D₁::Int,
                                  D₂::Int,
                                  subgraph_pop::Number)::AbstractProposal
    """ Tries to find a balanced cut on the subgraph induced by `mst_edges` and
        `mst_nodes` such that the population is balanced accoriding to
        `pop_constraint`.
        This subgraph was formed by the combination of districts `D₁` and `D₂`.
    """
    # used to agglomerate nodes
    subsets = Dict(n => BitSet(n) for n in mst_nodes)
    # keep track of populations of groups of nodes
    pops = Dict(n => graph.populations[n] for n in mst_nodes)
    # keep track of the degree of nodes as leaves are contracted
    degrees = Dict(n => length(mst[n]) for n in mst_nodes)

    # choose a root that has > degree 1
    non_leaves = filter(n -> degrees[n] > 1, mst_nodes)
    # if we can't find a root that has > degree 1, we'll start over
    if isempty(non_leaves) return DummyProposal("No non-leaves.") end
    root = rand(non_leaves)

    # perform bfs traversal and get predecessors
    preds, _ = bfs_traverse(mst, root)
    leaves = Deque{Int}()
    # add each leaf to deque
    foreach(l -> push!(leaves, l), setdiff(mst_nodes, non_leaves))

    while !isempty(leaves)
        leaf = popfirst!(leaves)
        parent = preds[leaf]
        # return group of nodes iff it satisfies the ideal population constraint
        # note that we only have to check one population to see if it's within the ideal
        # range - if so, then the other population must also be in the ideal range!
        if pops[leaf] >= pop_constraint.min_pop && pops[leaf] <= pop_constraint.max_pop
            component₁, component₂ = subsets[leaf], setdiff(mst_nodes, subsets[leaf])
            population₁, population₂ = pops[leaf], subgraph_pop - pops[leaf]
            return RecomProposal(D₁, D₂, population₁, population₂, component₁, component₂)
        end
        # contract leaf into parent
        union!(subsets[parent], subsets[leaf])
        parent_degree = (degrees[parent] -= 1)
        pops[parent] += pops[leaf]
        if parent_degree == 1 && parent != root
            push!(leaves, parent)
        end
    end
    return DummyProposal("Could not find balanced cut.")
end

function get_all_balanced_proposals(graph::BaseGraph,
                                    mst_edges::BitSet,
                                    mst_nodes::BitSet,
                                    partition::Partition,
                                    pop_constraint::PopulationConstraint,
                                    D₁::Int,
                                    D₂::Int)::Set{AbstractProposal}
    """ Tries to find the set of balanced cuts on the subgraph induced by
       `mst_edges` and `mst_nodes` such that the population is balanced
        according to `pop_constraint`.

        This subgraph was formed by the combination of districts `D₁` and `D₂`.
    """
    mst = build_mst(graph, mst_nodes, mst_edges)
    subgraph_pop = partition.dist_populations[D₁] + partition.dist_populations[D₂]
    #println("subgraph pop: ", subgraph_pop)

    proposals = Set{AbstractProposal}()

    for edge in mst_edges
        component₁ = traverse_mst(mst,
                                  graph.edge_src[edge],
                                  graph.edge_dst[edge])

        population₁ = get_subgraph_population(graph, component₁)
        population₂ = subgraph_pop - population₁

        if satisfy_constraint(pop_constraint, population₁, population₂)
            component₂ = setdiff(mst_nodes, component₁)
            proposal = RecomProposal(D₁, D₂, population₁, population₂,
                                     component₁, component₂)
            #println("edge: ", edge, "\tpop1: ", population₁, "\tpop2:", population₂)
            push!(proposals, proposal)
        end
    end
    return proposals
end


function get_valid_proposal_contraction(graph::BaseGraph,
                                        partition::Partition,
                                        pop_constraint::PopulationConstraint,
                                        rng::AbstractRNG,
                                        node_repeats::Int=1)
    """ Returns a population balanced proposal.

        Arguments:
            graph:          BaseGraph
            partition:      Partition
            pop_constraint: PopulationConstraint to adhere to
            tree_fn:        Function used to generate spanning trees
            num_tries:      num times to try getting a balanced cut from a subgraph
                            before giving up
            node_repeats:   How many times to try to sample a balanced cut from
                            a subgraph before generating a new subgraph
    """
    D₁, D₂, sg_edges, sg_nodes = sample_subgraph(graph, partition, rng)
    subgraph_pop = partition.dist_populations[D₁] + partition.dist_populations[D₂]
    while true
        weights = rand(rng, length(sg_edges))
        mst = weighted_kruskal_mst(graph, sg_edges, sg_nodes, weights)
        for _ in 1:node_repeats
            # see if we can get a population-balanced cut in this mst
            proposal = attempt_leaf_contraction(graph, mst, sg_nodes,
                                                pop_constraint, D₁, D₂, subgraph_pop)
            if proposal isa RecomProposal return proposal end
        end
    end
end


function get_valid_proposal_memoization(graph::BaseGraph,
                                        partition::Partition,
                                        pop_constraint::PopulationConstraint,
                                        rng::AbstractRNG,
                                        node_repeats::Int=1)
    """ Returns a population balanced proposal.

        Arguments:
            graph:          BaseGraph
            partition:      Partition
            pop_constraint: PopulationConstraint to adhere to
            num_tries:      num times to try getting a balanced cut from a subgraph
                            before giving up
            node_repeats:   How many times to try to sample a balanced cut from
                            a subgraph before generating a new subgraph
    """
    D₁, D₂, sg_edges, sg_nodes = sample_subgraph(graph, partition, rng)
    subgraph_pop = partition.dist_populations[D₁] + partition.dist_populations[D₂]
    while true
        weights = rand(rng, length(sg_edges))
        mst = weighted_kruskal_mst(graph, sg_edges, sg_nodes, weights)
        for _ in 1:node_repeats
            # see if we can get a population-balanced cut in this mst
            cut_set = memoized_balance_edges(graph, mst, sg_nodes,
                                             pop_constraint, D₁, D₂, subgraph_pop)
            if length(cut_set.balance_nodes) > 0
                # Choose a random balance node.
                component₁ = BitSet()
                root = rand(rng, keys(cut_set.balance_nodes))
                queue = Queue{Int}()
                enqueue!(queue, root)
                while !isempty(queue)
                    next_node = dequeue!(queue)
                    push!(component₁, next_node)
                    if next_node in keys(cut_set.succ)
                        for c in cut_set.succ[next_node]
                            enqueue!(queue, c)
                        end
                    end
                end
                pop₁ = cut_set.balance_nodes[root]
                component₂ = setdiff(sg_nodes, component₁)
                pop₂ = subgraph_pop - pop₁ 
                return RecomProposal(D₁, D₂, pop₁, pop₂, component₁, component₂)
            end
        end
    end
end



function update_partition!(partition::Partition,
                           graph::BaseGraph,
                           proposal::RecomProposal,
                           copy_parent::Bool=false,
                           weight::Float64=1.0)
    """ Updates the Partition with the RecomProposal
    """
    if copy_parent
        partition.parent = nothing
        old_partition = deepcopy(partition)
        partition.parent = old_partition
    end

    partition.dist_populations[proposal.D₁] = proposal.D₁_pop
    partition.dist_populations[proposal.D₂] = proposal.D₂_pop

    for node in proposal.D₁_nodes
        partition.assignments[node] = proposal.D₁
    end
    for node in proposal.D₂_nodes
        partition.assignments[node] = proposal.D₂
    end

    partition.dist_nodes[proposal.D₁] = proposal.D₁_nodes
    partition.dist_nodes[proposal.D₂] = proposal.D₂_nodes

    update_partition_adjacency!(partition, graph)
    update_partition_weight!(partition, weight)
end

function recom_chain(graph::BaseGraph,
                     partition::Partition,
                     pop_constraint::PopulationConstraint,
                     num_steps::Int,
                     scores::Array{AbstractScore, 1};
                     num_tries::Int=3,
                     acceptance_fn::Function=always_accept,
                     tree_fn::Function=random_kruskal_mst,
                     proposal_fn::Function=get_valid_proposal_contraction,
                     rng::AbstractRNG=Random.default_rng(),
                     no_self_loops::Bool=false)::ChainScoreData
        """Runs a Markov chain for `num_steps` steps using ReCom. Returns
        a ChainScoreData object which can be queried to retrieve the values of
        every score at each step of the chain.

        Arguments:
            graph:          BaseGraph
            partition:      Partition with the plan information
            pop_constraint: PopulationConstraint
            num_steps:      Number of steps to run the chain for
            scores:         Array of AbstractScores to capture at each step
            num_tries:      num times to try getting a balanced cut from a subgraph
                            before giving up
            acceptance_fn:  A function generating a probability in [0, 1]
                            representing the likelihood of accepting the
                            proposal. Should accept a Partition as input.
            tree_fn:        A function used to generate random spanning trees.
                            The GerryChain version of ReCom uses random minimum
                            spanning trees (fast, but not from the uniform
                            distribution); the reversible ReCom chain demands
                            spanning trees drawn from the uniform distribution.
            rng:            Random number generator. The user can pass in their
                            own; otherwise, we use the default RNG from Random.
            no_self_loops:  If this is true, then a failure to accept a new state
                            is not considered a self-loop; rather, the chain
                            simply generates new proposals until the acceptance
                            function is satisfied. BEWARE - this can create
                            infinite loops if the acceptance function is never
                            satisfied!
    """
    steps_taken = 0
    first_scores = score_initial_partition(graph, partition, scores)
    chain_scores = ChainScoreData(deepcopy(scores), [first_scores])

    while steps_taken < num_steps
        proposal = proposal_fn(graph, partition, pop_constraint, rng, num_tries)
        custom_acceptance = acceptance_fn !== always_accept
        update_partition!(partition, graph, proposal, custom_acceptance)
        if custom_acceptance && !satisfies_acceptance_fn(partition, acceptance_fn)
            # go back to the previous partition
            partition = partition.parent
            # if user specifies this behavior, we do not increment the steps
            # taken if the acceptance function fails.
            if no_self_loops continue end
        end
        score_vals = score_partition_from_proposal(graph, partition, proposal, scores)
        push!(chain_scores.step_values, score_vals)
        steps_taken += 1
    end
    return chain_scores
end

function reversible_recom_chain(
    graph::BaseGraph,
    partition::Partition,
    pop_constraint::PopulationConstraint,
    M::Int,
    num_steps::Int, 
    scores::Array{S, 1}; 
    num_tries::Int=3,
    tree_fn::Function=wilson_ust,
    acceptance_fn::F=always_accept, 
    rng::AbstractRNG=Random.default_rng())::ChainScoreData where
    {F<:Function, S<:AbstractScore}
    """Runs a Markov chain for `num_steps` steps using reversible ReCom.
       (See Cannon, Duchin, Randall, Rule 2020; forthcoming.)  Returns
        a ChainScoreData object which can be queried to retrieve the values of
        every score at each step of the chain.

        The reversible ReCom chain is slower than the "normal" (i.e. GerryChain-
        flavored) ReCom chain for the same number of _unique_ plans.
        This is for multiple reasons:
            * We draw spanning trees from the uniform distribution using
              a self-avoiding random walk, as described in Wilson 1996.
              This processs is asymptotically slower than drawing a random
              minimum spanning tree (that is, generating random edge weights
              and finding the minimum MST using Kruskal's or Prim's algorithm).
            * The chain has a high rejection probability compared to an otherwise
              equivalent ReCom chain. For the reversible chain, we count self-loops
              using the `weight` field in `partition`.

        `n_steps` specifies the number to steps to take _including_ self-loops;
        the number of plans in the returned data will usually be substantially
        smaller than `n_steps`.

        Arguments:
            graph:          BaseGraph
            partition:      Partition with the plan information
            pop_constraint: PopulationConstraint
            M:              An upper bound on the number of balance edges
                            in a proposal.
            num_steps:      Number of steps to run the chain for
            scores:         Array of AbstractScores to capture at each step
            num_tries:      num times to try getting a balanced cut from a subgraph
                            before giving up
            acceptance_fn:  A function generating a probability in [0, 1]
                            representing the likelihood of accepting the
                            proposal. Should accept a Partition as input.
            rng:            Random number generator. The user can pass in their
                            own; otherwise, we use the default RNG from Random.
    """
    steps_taken = 0
    self_loops = 0
    reasons = DefaultDict{String, Int}(0)
    first_scores = score_initial_partition(graph, partition, scores)
    chain_scores = ChainScoreData(deepcopy(scores), [first_scores])
    district_pairs = Set{Tuple}()
    for i in 1:graph.num_dists
        for j in (i + 1):graph.num_dists
            push!(district_pairs, (i, j))
        end
    end

    while steps_taken < num_steps
        #println(reasons)
        #println(partition.dist_populations)
        # Choose two random districts, not necessarily adjacent.
        D₁, D₂ = rand(district_pairs)
        if partition.dist_adj[D₁, D₂] == 0
            self_loops += 1
            reasons["non-adjacent district pair"]  += 1
        else
            sg_nodes = union(partition.dist_nodes[D₁], partition.dist_nodes[D₂])
            sg_edges = induced_subgraph_edges(graph, collect(sg_nodes))
            mst_edges = tree_fn(graph, sg_edges, collect(sg_nodes), rng)
            # TODO: would this be faster if proposals were generated lazily?
            proposals = get_all_balanced_proposals(graph, mst_edges, sg_nodes,
                                                   partition, pop_constraint, D₁, D₂) 
            n_proposals = length(proposals)
            if n_proposals > M
                throw("Number of balance edges ($n_proposals) exceeds upper bound ($M).")
            end
            if n_proposals > 0
                proposal = rand(proposals)
                seam_length = partition.dist_adj[D₁, D₂]
                if rand() < 1 / (M * seam_length)
                    custom_acceptance = acceptance_fn !== always_accept
                    update_partition!(partition, graph, proposal, custom_acceptance)
                    if custom_acceptance && !satisfies_acceptance_fn(partition, acceptance_fn)
                        # go back to the previous partition
                        partition = partition.parent
                        # case: self-loop on non-reversible ReCom acceptance function
                        self_loops += 1
                        reasons["custom acceptance function"] += 1
                    else
                        partition.weight = self_loops + 1
                        partition.chain_meta = Dict("reasons" => reasons)
                        println(reasons)
                        score_vals = score_partition_from_proposal(graph, partition,
                                                                   proposal, scores)
                        push!(chain_scores.step_values, score_vals)
                        # case: reset self-loops
                        self_loops = 0
                        reasons = DefaultDict{String, Int}(0)
                    end
                else
                    # case: self-loop on reversible ReCom rejection
                    self_loops += 1
                    reasons["seam length"] += 1
                end
            else
                # case: self-loop on lack of proposals
                self_loops += 1
                reasons["no proposals"] += 1
            end
        end
        steps_taken += 1
    end
    return chain_scores
end
