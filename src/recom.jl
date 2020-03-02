

function sample_subgraph(graph::BaseGraph, partition::Partition)
    """ Randomly sample two adjacent districts and returns them and their
        induced edges and nodes.
    """
    D₁, D₂ = sample_adjacent_districts_randomly(partition)

    # take all their nodes
    nodes = union(partition.dist_nodes[D₁],
                  partition.dist_nodes[D₂])

    # get a subgraph of these two districts
    subgraph_edges = induced_subgraph(graph, collect(nodes))

    return D₁, D₂, subgraph_edges, nodes
end
