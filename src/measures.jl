abstract type AbstractMOI end # MOI : "Measures of Interest"

struct Election <: AbstractMOI
    name::AbstractString
    P₁::AbstractString
    P₂::AbstractString
    P₁_col::AbstractString
    P₂_col::AbstractString
end

struct RacePopulations <: AbstractMOI # should be singluar # TODO
    name::AbstractString
    col_name::AbstractString
end

function initialize_counter(elections, racial_pops)
    counter = Dict{String, Int}()
    for election in elections
        counter[string(election.name, "_", election.P₁)] = 0
        counter[string(election.name, "_", election.P₂)] = 0
    end
    for racial_pop in racial_pops
        counter[racial_pop.name] = 0
    end
    return counter
end

function fill_counter!(counter, graph, partition, dist, elections, racial_pops)
    for node in partition.dist_nodes[dist]
        for election in elections
            counter[string(election.name, "_", election.P₁)] += graph.attributes[node][election.P₁_col]
            counter[string(election.name, "_", election.P₂)] += graph.attributes[node][election.P₂_col]
        end
        for racial_pop in racial_pops
            counter[racial_pop.name] += graph.attributes[node][racial_pop.col_name]
        end
    end
end

function update_measures!(measures, counter, elections, racial_pops)
    # put it in Measures dict
    for election in elections
        push!(measures[string(election.name, "_", election.P₁)], counter[string(election.name, "_", election.P₁)])
        push!(measures[string(election.name, "_", election.P₂)], counter[string(election.name, "_", election.P₂)])
    end
    for racial_pop in racial_pops
        push!(measures[racial_pop.name],  counter[racial_pop.name])
    end
end

function initialize_measures(partition, elections, racial_pops)
    measures = Dict{String, Any}()
    measures["num_cut_edges"] = partition.num_cut_edges
    for election in elections
        measures[string(election.name, "_", election.P₁)] = Array{Int, 1}()
        measures[string(election.name, "_", election.P₂)] = Array{Int, 1}()
    end
    for racial_pop in racial_pops
        measures[racial_pop.name] = Array{Int, 1}()
    end
    return measures
end

function get_measures(graph, partition, elections, racial_pops)
    measures = initialize_measures(partition, elections, racial_pops)

    for dist in 1:graph.num_dists
        counter = initialize_counter(elections, racial_pops)
        fill_counter!(counter, graph, partition, dist, elections, racial_pops)
        update_measures!(measures, counter, elections, racial_pops)
    end

    return measures
end

function update_measures!(measures, nodes, graph, elections, racial_pops)
    for node in nodes
        for election in elections
            measures[string(election.name, "_", election.P₁)][1] += graph.attributes[node][election.P₁_col]
            measures[string(election.name, "_", election.P₂)][1] += graph.attributes[node][election.P₂_col]
        end
        for racial_pop in racial_pops
            measures[string(racial_pop.name)][1] += graph.attributes[node][racial_pop.col_name]
        end
    end
end

function initialize_Δ_measures(partition, elections, racial_pops)
    Δ_measures = Dict{String, Any}()
    Δ_measures["num_cut_edges"] = partition.num_cut_edges
    Δ_measures["dists"] = (proposal.D₁, proposal.D₂)

    for election in elections
        Δ_measures[string(election.name, "_", election.P₁)] = [0, 0]
        Δ_measures[string(election.name, "_", election.P₂)] = [0, 0]
    end
    for racial_pop in racial_pops
        Δ_measures[string(racial_pop.name)] = [0, 0]
    end
    return Δ_measures
end

function get_measures(graph, partition, elections, racial_pops, proposal)
    Δ_measures = initialize_Δ_measures(partition, elections, racial_pops)
    update_measures!(Δ_measures, proposal.D₁_nodes, graph, partition, elections, racial_pops)
    update_measures!(Δ_measures, proposal.D₂_nodes, graph, partition, elections, racial_pops)
    return Δ_measures
end

function parse_measure_at_index(all_measures, index)
    if index == 1
        return all_measures[1]
    end

    measures = deepcopy(all_measures[1])

    for i in 2:index
        curr_measures = all_measures[index]
        (D₁, D₂) = all_measures[index]["dists"]

        for key in keys(measures)
            if key in ("num_cut_edges", "dists")
                continue
            end
            measures[key][D₁] = curr_measures[key][1]
            measures[key][D₂] = curr_measures[key][2]
        end
    end

    return measures
end
