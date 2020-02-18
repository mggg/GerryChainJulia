# general utility functions

function get_graph_and_partition(filepath::AbstractString,
                                 population_col::AbstractString,
                                 assignment_col::AbstractString,
                                 nodes_str::AbstractString = "nodes",
                                 adjacency_str::AbstractString = "adjacency",
                                 edge_id_str::AbstractString = "id")
    raw_graph = JSON.parsefile(filepath)
    graph = Graph(raw_graph)
    partition = Partition(graph, raw_graph, population_col, assignment_col)

    return graph, partition
end
