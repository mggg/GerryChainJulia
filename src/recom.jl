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


function bfs_traverse(mst::Dict{Int, Array{Int, 1}},
                      root::Int)::Tuple{Dict{Int, Int}, Dict{Int, BitSet}}
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


function component_nodes(cut_set::CutSet, root::Int)::BitSet
    if !(root in keys(cut_set.balance_nodes))
        throw("Node $(root) not a balance node.")
    end

    component = BitSet()
    queue = Queue{Int}()
    enqueue!(queue, root)
    while !isempty(queue)
        next_node = dequeue!(queue)
        push!(component, next_node)
        if next_node in keys(cut_set.succ)
            for c in cut_set.succ[next_node]
                enqueue!(queue, c)
            end
        end
    end
    return component
end


function get_recom_proposal(graph::BaseGraph,
                            partition::Partition,
                            pop_constraint::PopulationConstraint,
                            rng::AbstractRNG,
                            tree_fn::Function=weighted_kruskal_mst,
                            balance_edge_fn::Function=memoized_balance_edges,
                            node_repeats::Int=1)::RecomProposal
    """ Returns a population-balanced Recom proposal.

        Arguments:
            graph:            BaseGraph
            partition:        Partition
            pop_constraint:   PopulationConstraint to adhere to
            tree_fn:          A function used to generate random spanning trees.
                              The GerryChain version of ReCom uses random minimum
                              spanning trees (fast, but not from the uniform
                              distribution); the reversible ReCom chain demands
                              spanning trees drawn from the uniform distribution.
            balance_edge_fn:  A function used to find population-balanced edges
                              in a spanning tree.
            num_tries:        num times to try getting a balanced cut from a subgraph
                              before giving up
            node_repeats:     How many times to try to sample a balanced cut from
                              a subgraph before generating a new subgraph
    """
    D₁, D₂, sg_edges, sg_nodes = sample_subgraph(graph, partition, rng)
    subgraph_pop = partition.dist_populations[D₁] + partition.dist_populations[D₂]

    while true
        mst = tree_fn(graph, sg_edges, sg_nodes, rng)
        for _ in 1:node_repeats
            # see if we can get a population-balanced cut in this mst
            cut_set = balance_edge_fn(graph, mst, sg_nodes,
                                      pop_constraint, D₁, D₂, subgraph_pop)
            if length(cut_set.balance_nodes) > 0
                # Choose a random balance node.
                root = rand(rng, keys(cut_set.balance_nodes))
                component₁ - component_nodes(cut_set, root)
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
                     proposal_fn::Function=get_recom_proposal,
                     rng::AbstractRNG=Random.default_rng(),
                     no_self_loops::Bool=false)::ChainScoreData
        """Runs a Markov chain for `num_steps` steps using ReCom. Returns
        a ChainScoreData object which can be queried to retrieve the values of
        every score at each step of the chain.

        Arguments:
            graph:           BaseGraph
            partition:       Partition with the plan information
            pop_constraint:  PopulationConstraint
            num_steps:       Number of steps to run the chain for
            scores:          Array of AbstractScores to capture at each step
            num_tries:       num times to try getting a balanced cut from a subgraph
                             before giving up
            acceptance_fn:   A function generating a probability in [0, 1]
                             representing the likelihood of accepting the
                             proposal. Should accept a Partition as input.
            proposal_fn:     A function used to generate a single ReCom proposal.
                             This function will typically be a parameterized 
                             variant of `get_recom_proposal`.
            rng:             Random number generator. The user can pass in their
                             own; otherwise, we use the default RNG from Random.
            no_self_loops:   If this is true, then a failure to accept a new state
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


function get_reversible_recom_proposal(
    graph::BaseGraph,
    partition::Partition,
    pop_constraint::PopulationConstraint,
    M::Int,
    rng::AbstractRNG,
    tree_fn::Function=wilson_ust,
    balance_edge_fn::Function=memoized_balance_edges,
    node_repeats::Int=1)::Union{RecomProposal, String}
    """TODO"""
    D₁, D₂ = rand(1:partition.num_dists, 2)
    if D₁ == D₂ || partition.dist_adj[D₁, D₂] == 0
        return "non-adjacent district pair"
    end

    subgraph_pop = partition.dist_populations[D₁] + partition.dist_populations[D₂]
    sg_nodes = union(partition.dist_nodes[D₁], partition.dist_nodes[D₂])
    sg_edges = induced_subgraph_edges(graph, collect(sg_nodes))
    mst = tree_fn(graph, sg_edges, collect(sg_nodes), rng)
    cut_set = balance_edge_fn(graph, mst, sg_nodes,
                              pop_constraint, D₁, D₂, subgraph_pop)
    n_proposals = length(cut_set.balance_nodes)
    if n_proposals > M
        throw("Number of balance edges ($n_proposals) exceeds upper bound ($M).")
    end

    if n_proposals > 0
        root = rand(rng, keys(cut_set.balance_nodes))
        component₁ = component_nodes(cut_set, root)
        pop₁ = cut_set.balance_nodes[root]
        component₂ = setdiff(sg_nodes, component₁)
        pop₂ = subgraph_pop - pop₁ 
        proposal = RecomProposal(D₁, D₂, pop₁, pop₂, component₁, component₂)
        seam_length = 0
        for node in proposal.D₁_nodes
            for neighbor in graph.neighbors[node]
                if neighbor in proposal.D₂_nodes
                    seam_length += 1
                end
            end
        end

        # We have a valid proposal; (maybe) accept.
        if rand() < 1 / (M * seam_length)
            return proposal
        else
            # case: self-loop on reversible ReCom rejection
            return "seam length"
        end
    end
    # case: self-loop on lack of proposals
    return "no proposals"
end

function reversible_recom_chain(
    graph::BaseGraph,
    partition::Partition,
    pop_constraint::PopulationConstraint,
    M::Int,
    num_steps::Int, 
    scores::Array{S, 1}; 
    num_tries::Int=3,
    proposal_fn::Function=get_reversible_recom_proposal,
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
            graph:            BaseGraph
            partition:        Partition with the plan information
            pop_constraint:   PopulationConstraint
            M:                An upper bound on the number of balance edges
                              in a proposal.
            num_steps:        Number of steps to run the chain for
            scores:           Array of AbstractScores to capture at each step
            num_tries:        num times to try getting a balanced cut from a subgraph
                              before giving up
            proposal_fn:      A function used to generate a single reversible ReCom
                              proposal or self-loop. This function will typically be
                              a parameterized variant of `get_reversible_recom_proposal`.
            acceptance_fn:    A function generating a probability in [0, 1]
                              representing the likelihood of accepting the
                              proposal. Should accept a Partition as input.
            rng:              Random number generator. The user can pass in their
                              own; otherwise, we use the default RNG from Random.
    """
    steps_taken = 0
    self_loops = 0
    reasons = DefaultDict{String, Int}(0)
    first_scores = score_initial_partition(graph, partition, scores)
    chain_scores = ChainScoreData(deepcopy(scores), [first_scores])
    custom_acceptance = acceptance_fn !== always_accept

    while steps_taken < num_steps
        proposal = proposal_fn(graph, partition, pop_constraint, M, rng)
        if proposal isa RecomProposal
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
                #println(reasons)
                score_vals = score_partition_from_proposal(graph, partition,
                                                        proposal, scores)
                push!(chain_scores.step_values, score_vals)
                # case: reset self-loops
                self_loops = 0
                reasons = DefaultDict{String, Int}(0)
            end
        else
            self_loops += 1
            reasons[proposal] += 1
        end
        steps_taken += 1
    end
    return chain_scores
end