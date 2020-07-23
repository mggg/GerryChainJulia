mutable struct Partition
    num_cut_edges::Int
    assignments::Array{Int, 1}                  # of length(num_nodes)
    dist_populations::Array{Int, 1}             # of length(num_districts)
    cut_edges::Array{Int, 1}                    # of length(num_edges)
    dist_adj::SparseMatrixCSC{Int, Int}
    dist_nodes::Array{BitSet}
    parent::Union{Partition, Nothing}           # optional parent partition
end

function Partition(filepath::AbstractString,
                   graph::BaseGraph,
                   pop_col::AbstractString,
                   assignment_col::AbstractString)::Partition

    """
        Partition represents a partition of the nodes of the graph.
        It contains plan-specific information that will change each time we change
        our plan. We assume that the JSON file contains a key "nodes" that
        refers to an array of nodes.

        Arguments:
            filepath:  Filepath to the .json graph file
            graph:     Graph object that has the underlying network structure of the plan.
            pop_col:   the key denoting the population attribute at the node level
            assignment_col: the key denoting the district assignment at the node level
    """
    raw_graph = JSON.parsefile(filepath)

    populations, assignments = get_populations_and_assignments(raw_graph["nodes"], pop_col, assignment_col)

    # get cut_edges, district_adjacencies
    dist_adj, cut_edges = get_district_adj_and_cut_edges(graph, assignments, graph.num_dists)
    num_cut_edges = sum(cut_edges)

    # get district populations
    dist_populations = get_district_populations(assignments, populations, graph.num_nodes, graph.num_dists)

    # get district_nodes
    dist_nodes = get_district_nodes(assignments, graph.num_nodes, graph.num_dists)

    # return Partition with no parent by default
    return Partition(num_cut_edges, assignments, dist_populations, cut_edges,
                     dist_adj, dist_nodes, nothing)
end

function get_district_nodes(assignments::Array{Int, 1},
                            num_nodes::Int,
                            num_districts::Int)::Array{Set{Int}, 1}
    """ Returns an array of sets where the nodes of the i'th district will
        be at district_nodes[i] as a set.
    """
    district_nodes = [BitSet([]) for _ in 1:num_districts]
    for i in 1:num_nodes
        push!(district_nodes[assignments[i]], i)
    end
    return district_nodes
end

function get_district_populations(assignments::Array{Int, 1},
                                  populations::Array{Int, 1},
                                  num_nodes::Int,
                                  num_districts::Int)::Array{Int, 1}
    """ Returns an array of populations where the population of the i'th district
        is at district_populations[i].
    """
    district_populations = zeros(Int, num_districts)
    for i in 1:num_nodes
        district_populations[assignments[i]] += populations[i]
    end
    return district_populations
end

function get_district_adj_and_cut_edges(graph::BaseGraph,
                                        assignments::Array{Int, 1},
                                        num_districts::Int)
    """ Returns:
            district_adj: a num_districts x num_districts matrix where the
                          elements are the number of cut_edges between the districts
            cut_edges:    an array of size(num_edges) where i'th element is 1
                          if edge `i` is a cut_edge, 0 otherwise
    """
    district_adj = zeros(Int, num_districts, num_districts)
    num_edges = ne(graph.simple_graph)
    cut_edges = zeros(Int, graph.num_edges)

    for edge in edges(graph.simple_graph)
        src_assignment = assignments[src(edge)]
        dst_assignment = assignments[dst(edge)]

        if src_assignment != dst_assignment
            # look up the index for this edge
            index = graph.adj_matrix[src(edge), dst(edge)]
            cut_edges[index] = 1

            district_adj[src_assignment, dst_assignment] += 1
            district_adj[dst_assignment, src_assignment] += 1
        end
    end
    return district_adj, cut_edges
end

function sample_adjacent_districts_randomly(partition::Partition,
                                            num_dists::Int,
                                            rng::AbstractRNG)
    """ Randomly sample two adjacent districts and return them.
    """
    while true
        D₁ = rand(rng, 1:num_dists)
        D₂ = rand(rng, 1:num_dists)
        if partition.dist_adj[D₁, D₂] != 0
            return D₁, D₂
        end
    end
end

function get_populations_and_assignments(nodes::Array{Any, 1},
                                         pop_col::AbstractString,
                                         assignment_col::AbstractString)
    """ Returns the arrays of populations and assignments of the graph, where
        i'th node's population is populations[i] and assignment is assignments[i].
    """
    num_nodes = length(nodes)
    populations = zeros(Int, num_nodes)
    assignments = zeros(Int, num_nodes)
    for i in 1:num_nodes
        populations[i] = nodes[i][pop_col]
        if nodes[i][assignment_col] isa String
            assignments[i] = parse(Int, nodes[i][assignment_col])
        elseif nodes[i][assignment_col] isa Int
            assignments[i] = nodes[i][assignment_col]
        end
    end
    return populations, assignments
end


function update_partition_adjacency(partition::Partition,
                                    graph::BaseGraph)
    """ Updates the district adjacency matrix and cut edges
        to reflect the partition's assignments for each node.
    """
    partition.dist_adj = spzeros(Int, graph.num_dists, graph.num_dists)
    partition.num_cut_edges = 0

    for i=1:graph.num_edges
        src_assignment = partition.assignments[graph.edge_src[i]]
        dst_assignment = partition.assignments[graph.edge_dst[i]]

        if src_assignment != dst_assignment
            partition.cut_edges[i] = 1
            partition.num_cut_edges += 1

            partition.dist_adj[src_assignment, dst_assignment] += 1
            partition.dist_adj[dst_assignment, src_assignment] += 1
        else
            partition.cut_edges[i] = 0
        end
    end
end
