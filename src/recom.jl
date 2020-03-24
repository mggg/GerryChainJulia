

function sample_subgraph(graph::BaseGraph, partition::Partition)
    """ Randomly sample two adjacent districts and returns them and their
        induced edges and nodes.
    """
    D₁, D₂ = sample_adjacent_districts_randomly(partition, graph.num_dists)

    # take all their nodes
    nodes = union(partition.dist_nodes[D₁],
                  partition.dist_nodes[D₂])

    # get a subgraph of these two districts
    edges = induced_subgraph_edges(graph, collect(nodes))

    return D₁, D₂, edges, BitSet(nodes)
end

function build_mst(graph::BaseGraph, nodes::BitSet,
                   edges::BitSet)::Dict{Int, Array{Int, 1}}
    """ Builds a graph as an adjacency list from the `mst_nodes` and `mst_edges`.
    """
    mst = Dict{Int, Array{Int, 1}}()
    for node in nodes
        mst[node] = Array{Int, 1}()
    end
    for edge in edges
        add_edge_to_mst!(graph, mst, edge)
    end
    return mst
end

function remove_edge_from_mst!(graph::BaseGraph, mst::Dict{Int, Array{Int,1}}, edge::Int)
    """ Removes an edge from the graph built by `build_mst`.
    """
    filter!(e -> e != graph.edge_dst[edge], mst[graph.edge_src[edge]])
    filter!(e -> e != graph.edge_src[edge], mst[graph.edge_dst[edge]])
end

function add_edge_to_mst!(graph::BaseGraph, mst::Dict{Int, Array{Int,1}}, edge::Int)
    """ Adds an edge to the graph built by `build_mst`.
    """
    push!(mst[graph.edge_src[edge]], graph.edge_dst[edge])
    push!(mst[graph.edge_dst[edge]], graph.edge_src[edge])
end

function traverse_mst(mst::Dict{Int, Array{Int, 1}},
                      start_node::Int,
                      avoid_node::Int,
                      stack::Stack{Int},
                      traversed_nodes::BitSet)::BitSet
    """ Returns the component of the MST `mst` that contains the vertex
        `start_node`.

        Arguments:
            mst:        mst to traverse
            start_node: the node to start traversing from
            avoid_node: the node to avoid adn which seperates the mst into
                        two components
            stack:      an empty Stack
            traversed_nodes: an empty BitSet that is to be populated.

        `stack` and `traversed_nodes` are are pre-allocated and passed in to
        reduce the number of memory allocations and consequently, time taken.
        In the course of calling this function multiple times, it is intended that
        we pass in the same (empty) objects repeatedly.
    """
    @assert isempty(stack)
    empty!(traversed_nodes)

    push!(stack, start_node)

    while !isempty(stack)
        new_node = pop!(stack)
        push!(traversed_nodes, new_node)

        for neighbor in mst[new_node]
            if !(neighbor in traversed_nodes) && neighbor != avoid_node
                push!(stack, neighbor)
            end
        end
    end
    return traversed_nodes
end

# function get_a_component(graph::BaseGraph,
#                          start_node::Int,
#                          avoid_node::Int,
#                          mst_edges::BitSet,
#                          traversed_nodes::BitSet,
#                          stack::Stack{Int})::BitSet
#     empty!(traversed_nodes)
#     push!(stack, start_node)
#
#     while !isempty(stack)
#         next_node = pop!(stack)
#         push!(traversed_nodes, next_node)
#
#         for neighbor in graph.neighbors[next_node]
#             neighbor_edge = graph.adj_matrix[neighbor, next_node]
#             if (neighbor_edge in mst_edges && !(neighbor in traversed_nodes) && neighbor != avoid_node)
#                 push!(stack, neighbor)
#             end
#         end
#     end
#     return traversed_nodes
# end

function get_balanced_proposal(graph::BaseGraph,
                               mst_edges::BitSet,
                               mst_nodes::BitSet,
                               partition::Partition,
                               pop_constraint::PopulationConstraint,
                               D₁::Int,
                               D₂::Int)
    """ Tries to find a balanced cut on the subgraph induced by `mst_edges` and
        `mst_nodes` such that the population is balanced accoriding to
        `pop_constraint`.
        This subgraph was formed by the combination of districts `D₁` and `D₂`.
    """
    mst = build_mst(graph, mst_nodes, mst_edges)
    subgraph_pop = partition.dist_populations[D₁] + partition.dist_populations[D₂]

    # pre-allocated reusable data structures to reduce number of memory allocations
    stack = Stack{Int}()
    component_container = BitSet([])

    for edge in mst_edges
        component₁ = traverse_mst(mst,
                                  graph.edge_src[edge],
                                  graph.edge_dst[edge],
                                  stack,
                                  component_container)

        population₁ = get_subgraph_population(graph, component₁)
        population₂ = subgraph_pop - population₁

        if satisfy_constraint(pop_constraint, population₁, population₂)
            component₂ = setdiff(mst_nodes, component₁)
            proposal = RecomProposal(D₁, D₂, population₁, population₂, component₁, component₂)
            return proposal
        end
    end
    return DummyProposal("Could not find balanced cut.")
end


function get_valid_proposal(graph::BaseGraph,
                            partition::Partition,
                            pop_constraint::PopulationConstraint,
                            num_tries::Int=3)
    """ Returns a population balanced proposal.

        Arguments:
            graph:          BaseGraph
            partition:      Partition
            pop_constraint: PopulationConstraint to adhere to
            num_tries:      num times to try getting a balanced cut from a subgraph
                            before giving up
    """
    while true
        D₁, D₂, sg_edges, sg_nodes = sample_subgraph(graph, partition)

        for _ in 1:num_tries
            weights = rand(rng, length(sg_edges))
            mst_edges = weighted_kruskal_mst(graph, sg_edges, collect(sg_nodes), weights)

            # see if we can get a population-balanced cut in this mst
            proposal = get_balanced_proposal(graph, mst_edges, sg_nodes,
                                             partition, pop_constraint,
                                             D₁, D₂)

            if proposal isa RecomProposal return proposal end
        end
    end
end


function update!(graph::BaseGraph,
                 partition::Partition,
                 proposal::RecomProposal)
    """ Updates the Partition with the RecomProposal
    """
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

function recom_chain(graph::BaseGraph,
                     partition::Partition,
                     pop_constraint::PopulationConstraint,
                     num_steps::Int,
                     elections::Array{Election, 1},
                     racial_pops::Array{RacePopulations, 1},
                     measures_save_dir::AbstractString="measures.json",
                     num_tries::Int=3)
    """ Runs a Markov Chain for `num_steps` steps using ReCom.

        Arguments:
            graph:          BaseGraph
            partition:      Partition with the plan information
            pop_constraint: PopulationConstraint
            num_steps:      Number of steps to run the chain for
            num_tries:      num times to try getting a balanced cut from a subgraph
                            before giving up
    """
    all_measures = Array{Dict{String, Any}, 1}()
    steps_taken = 0
    while steps_taken < num_steps
        steps_taken += 1
        proposal = get_valid_proposal(graph, partition, pop_constraint, num_tries)
        update!(graph, partition, proposal)
        if steps_taken == 1
            measures = get_measures(graph, partition, elections, racial_pops)
        else
            measures = get_measures(graph, partition, elections, racial_pops, proposal)
        end
        push!(all_measures, measures)
    end

    # measures = parse_measure_at_index(all_measures, 7)
    # println(measures)
    # println(length(keys(measures)))
    # println(all_measures)
    json_string = JSON.json(all_measures)
    open(measures_save_dir, "w") do f
        write(f, json_string)
    end
end
