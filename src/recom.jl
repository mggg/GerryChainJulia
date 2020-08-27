

function sample_subgraph(graph::BaseGraph,
                         partition::Partition,
                         rng::AbstractRNG)
    """ Randomly sample two adjacent districts and returns them and their
        induced edges and nodes.
    """
    D₁, D₂ = sample_adjacent_districts_randomly(partition, graph.num_dists, rng)

    # take all their nodes
    nodes = union(partition.dist_nodes[D₁],
                  partition.dist_nodes[D₂])

    # get a subgraph of these two districts
    edges = induced_subgraph_edges(graph, collect(nodes))

    return D₁, D₂, edges, nodes
end


function bfs_predecessors(mst::Dict{Int, Array{Int, 1}}, root)
    """ Perform a BFS traversal of an MST from a specified root node and return
        a dictionary of predecessors, which maps every visited node to its "parent"
        in the BFS tree.
    """
    q = Queue{Int}()
    visited = BitSet()
    enqueue!(q, root)
    predecessors = Dict{Int, Int}()

    while !isempty(q)
        n = dequeue!(q)
        push!(visited, n)

        for neighbor in mst[n]
            if !(neighbor in visited)
                enqueue!(q, neighbor)
                predecessors[neighbor] = n
            end
        end
    end
    return predecessors
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
    preds = bfs_predecessors(mst, root)
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


function get_valid_proposal(graph::BaseGraph,
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
                     scores::Array{S, 1};
                     num_tries::Int=3,
                     acceptance_fn::F=always_accept,
                     rng::AbstractRNG=Random.default_rng())::ChainScoreData where
                     {F<:Function, S<:AbstractScore}
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
    """
    steps_taken = 0
    first_scores = score_initial_partition(graph, partition, scores)
    chain_scores = ChainScoreData(deepcopy(scores), [first_scores])

    while steps_taken < num_steps
        proposal = get_valid_proposal(graph, partition, pop_constraint, rng, num_tries)
        custom_acceptance = acceptance_fn !== always_accept
        update_partition!(partition, graph, proposal, custom_acceptance)
        if custom_acceptance && !satisfies_acceptance_fn(partition, acceptance_fn)
            # go back to the previous partition
            partition = partition.parent
        end
        score_vals = score_partition_from_proposal(graph, partition, proposal, scores)
        push!(chain_scores.step_values, score_vals)
        steps_taken += 1
    end
    return chain_scores
end
