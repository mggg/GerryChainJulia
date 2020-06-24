function propose_random_flip(graph::BaseGraph,
                             partition::Partition)
    """ Proposes a random boundary flip from the partition.

    Arguments:
        partition:  Partition
        graph:      BaseGraph
    """
    if partition.num_cut_edges == 0
        throw(ArgumentError("No cut edges in the districting plan"))
    end
    # select a random cut edge
    cut_edge_idx = rand(1:partition.num_cut_edges)
    cut_edge_tracker = 0
    edge_idx = 0
    # iterate through array of bools indicating cut edge, stop at the
    # randomly chosen index-th edge
    for i in 1:graph.num_edges
        cut_edge_tracker += partition.cut_edges[i]
        if cut_edge_tracker == cut_edge_idx
            edge_idx = i
            break
        end
    end
    # randomly choose which of the nodes from the edge get flipped
    edge = (graph.edge_src[edge_idx], graph.edge_dst[edge_idx])
    index = rand((0, 1))
    flipped_node, other_node = edge[index + 1], edge[2 - index]
    node_pop = graph.populations[flipped_node]
    # old district
    D₁ = partition.assignments[flipped_node]
    D₁_pop = partition.dist_populations[D₁] - node_pop
    D₁_n = setdiff(partition.dist_nodes[D₁], flipped_node)
    # new district
    D₂ = partition.assignments[other_node]
    D₂_pop = partition.dist_populations[D₂] + node_pop
    D₂_n = union(partition.dist_nodes[D₂], flipped_node)
    return FlipProposal(flipped_node, D₁, D₂, D₁_pop, D₂_pop, D₁_n, D₂_n)
end


function is_valid(graph::BaseGraph,
                  partition::Partition,
                  pop_constraint::PopulationConstraint,
                  cont_constraint::ContiguityConstraint,
                  proposal::FlipProposal)
    """ Helper function that checks whether a proposal both (a) is population
        balanced and (b) does not break contiguity.
    """
    return satisfy_constraint(pop_constraint,
                              proposal.D₂_pop,
                              proposal.D₁_pop) &&
           satisfy_constraint(cont_constraint,
                              graph,
                              partition,
                              proposal)
end


function get_valid_proposal(graph::BaseGraph,
                            partition::Partition,
                            pop_constraint::PopulationConstraint,
                            cont_constraint::ContiguityConstraint)
    """ Returns a population balanced FlipProposal subject to a contiguity
        constraint.
    """
    proposal = propose_random_flip(graph, partition)
    # continuously generate new proposals until one satisfies our constraints
    while !is_valid(graph, partition, pop_constraint, cont_constraint, proposal)
        proposal = propose_random_flip(graph, partition)
    end
    return proposal
end


function update_partition!(partition::Partition,
                           graph::BaseGraph,
                           proposal::FlipProposal)
    """ Updates the Partition with the FlipProposal
    """
    # update district population counts
    partition.dist_populations[proposal.D₁] = proposal.D₁_pop
    partition.dist_populations[proposal.D₂] = proposal.D₂_pop

    # relabel node with new district
    partition.assignments[proposal.node] = proposal.D₂

    pop!(partition.dist_nodes[proposal.D₁], proposal.node)
    push!(partition.dist_nodes[proposal.D₂], proposal.node)

    update_partition_adjacency(partition, graph)
end


function flip_chain(graph::BaseGraph,
                    partition::Partition,
                    pop_constraint::PopulationConstraint,
                    cont_constraint::ContiguityConstraint,
                    num_steps::Int,
                    score_keys::Array{String, 1},
                    scores_save_dir::AbstractString="./scores.json",
                    num_tries::Int=3)
    """ Runs a Markov Chain for `num_steps` steps using Flip proposals.

        Arguments:
            graph:              BaseGraph
            partition:          Partition with the plan information
            pop_constraint:     PopulationConstraint
            cont_constraint:    ContiguityConstraint
            score_keys:         list of scores to evaluate plans with
            score_save_dir:     directory of where to store the scores
            num_steps:          Number of steps to run the chain for
    """
    steps_taken = 0
    all_scores = Array{Dict{String, Any}, 1}()

    while steps_taken < num_steps
        steps_taken += 1

        proposal = get_valid_proposal(graph, partition, pop_constraint,
                                      cont_constraint)
        update_partition!(partition, graph, proposal)

        scores = get_scores(graph, partition, score_keys, steps_taken, proposal)
        push!(all_scores, scores)
    end
end
