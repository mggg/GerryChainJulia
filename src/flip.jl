function propose_random_flip(graph::BaseGraph,
                             partition::Partition)
    """ Proposes a random boundary flip from the partition.

    Arguments:
        partition:  Partition
        graph:      BaseGraph
    """
    if partition.num_cut_edges == 0
        return partition
    end
    cut_edge_idx = rand(1:partition.num_cut_edges)
    cut_edge_tracker = 0
    edge_idx = 0
    for i in 1:graph.num_edges
        cut_edge_tracker += partition.cut_edges[i]
        if cut_edge_tracker == cut_edge_idx
            edge_idx = i
            break
        end
    end

    edge = (graph.edge_src[edge_idx], graph.edge_dst[edge_idx])
    index = rand((0, 1))
    flipped_node, other_node = edge[index + 1], edge[2 - index]
    node_pop = graph.populations[flipped_node]
    old_dist = partition.assignments[flipped_node]
    new_dist = partition.assignments[other_node]
    flip = FlipProposal(flipped_node,
                        old_dist,
                        new_dist,
                        partition.dist_populations[new_dist] + node_pop,
                        partition.dist_populations[old_dist] - node_pop,
                        union(partition.dist_nodes[new_dist], flipped_node),
                        setdiff(partition.dist_nodes[old_dist], flipped_node))
    return flip
end


function get_valid_proposal(graph::BaseGraph,
                            partition::Partition,
                            pop_constraint::PopulationConstraint)
    """ Returns a population balanced proposal.

        Arguments:
            graph:          BaseGraph
            partition:      Partition
            pop_constraint: PopulationConstraint to adhere to
            num_tries:      num times to try getting a balanced cut from a subgraph
                            before giving up
    """
    proposal = propose_random_flip(graph, partition)
    while satisfy_constraint(pop_constraint,
                             proposal.D₂_pop,
                             proposal.D₁_pop)
        proposal = propose_random_flip(graph, partition)
    end
    return proposal
end


function update_partition!(partition::Partition,
                           graph::BaseGraph,
                           proposal::FlipProposal)
    """ Updates the Partition with the FlipProposal
    """
    orig_dist = partition.assignments[proposal.Node]
    new_dist = proposal.D₂
    partition.dist_populations[orig_dist] = proposal.D₁_pop
    partition.dist_populations[new_dist] = proposal.D₂_pop

    partition.assignments[proposal.Node] = new_dist

    pop!(partition.dist_nodes[orig_dist], proposal.Node)
    push!(partition.dist_nodes[new_dist], proposal.Node)

    update_partition_adjacency(partition, graph)
end


function flip_chain(graph::BaseGraph,
                    partition::Partition,
                    pop_constraint::PopulationConstraint,
                    num_steps::Int,
                    score_keys::Array{String, 1},
                    scores_save_dir::AbstractString="./scores.json",
                    num_tries::Int=3)
    """ Runs a Markov Chain for `num_steps` steps using Flip proposals.

        Arguments:
            graph:          BaseGraph
            partition:      Partition with the plan information
            pop_constraint: PopulationConstraint
            num_steps:      Number of steps to run the chain for
    """
    steps_taken = 0
    all_scores = Array{Dict{String, Any}, 1}()

    while steps_taken < num_steps
        steps_taken += 1

        proposal = get_valid_proposal(graph, partition, pop_constraint)
        update_partition!(partition, graph, proposal)

        scores = get_scores(graph, partition, score_keys, steps_taken, proposal)
        push!(all_scores, scores)
    end
end
