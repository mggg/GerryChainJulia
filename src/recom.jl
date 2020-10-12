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
                           copy_parent::Bool=false)
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

    update_partition_adjacency(partition, graph)
end


function recom_chain(graph::BaseGraph,
                     partition::Partition,
                     pop_constraint::PopulationConstraint,
                     num_steps::Int,
                     scores::Array{AbstractScore, 1};
                     num_tries::Int=3,
                     acceptance_fn::Function=always_accept,
                     proposal_fn::Function=get_valid_proposal_contraction,
                     rng::AbstractRNG=Random.default_rng(),
                     no_self_loops::Bool=false)::ChainScoreData
    """ Runs a Markov Chain for `num_steps` steps using ReCom. Returns
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
