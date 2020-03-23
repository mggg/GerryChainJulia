abstract type AbstractMOI end # MOI : "Measures of Interest"

struct Election <: AbstractMOI
    name::AbstractString
    P₁::AbstractString
    P₂::AbstractString
    P₁_col::AbstractString
    P₂_col::AbstractString
end

struct RacePopulations <: AbstractMOI
    name::AbstractString
    col_name::AbstractString
end

function get_measures(graph, partition, elections, racial_pops)
    measures = Dict{String, Any}()
    measures["num_cut_edges"] = partition.num_cut_edges
    for election in elections
        measures[string(election.name, "_", election.P₁)] = Array{Int, 1}()
        measures[string(election.name, "_", election.P₂)] = Array{Int, 1}()
    end
    for racial_pop in racial_pops
        measures[racial_pop.name] = Array{Int, 1}()
    end

    for i in 1:graph.num_dists
        counter = Dict{String, Int}()
        for election in elections
            counter[string(election.name, "_", election.P₁)] = 0
            counter[string(election.name, "_", election.P₂)] = 0
        end
        for racial_pop in racial_pops
            counter[racial_pop.name] = 0
        end

        for node in partition.dist_nodes[i]
            for election in elections
                counter[string(election.name, "_", election.P₁)] += graph.attributes[node][election.P₁_col]
                counter[string(election.name, "_", election.P₂)] += graph.attributes[node][election.P₂_col]
            end
            for racial_pop in racial_pops
                counter[racial_pop.name] += graph.attributes[node][racial_pop.col_name]
            end
        end

        # put it in Measures dict
        for election in elections
            push!(measures[string(election.name, "_", election.P₁)], counter[string(election.name, "_", election.P₁)])
            push!(measures[string(election.name, "_", election.P₂)], counter[string(election.name, "_", election.P₂)])
        end
        for racial_pop in racial_pops
            push!(measures[racial_pop.name],  counter[racial_pop.name])
        end
    end

    return measures
end

function get_measures(graph, partition, elections, racial_pops, proposal)
    Δ_measures = Dict{String, Any}()
    Δ_measures["num_cut_edges"] = partition.num_cut_edges
    Δ_measures["D₁"] = proposal.D₁
    Δ_measures["D₂"] = proposal.D₂

    # initialize
    for election in elections
        Δ_measures[string(election.name, "_", election.P₁, "_D₁")] = 0
        Δ_measures[string(election.name, "_", election.P₁, "_D₂")] = 0
        Δ_measures[string(election.name, "_", election.P₂, "_D₁")] = 0
        Δ_measures[string(election.name, "_", election.P₂, "_D₂")] = 0
    end
    for racial_pop in racial_pops
        Δ_measures[string(racial_pop.name, "_D₁")] = 0
        Δ_measures[string(racial_pop.name, "_D₂")] = 0
    end

    # calculate
    for node in proposal.D₁_nodes
        for election in elections
            Δ_measures[string(election.name, "_", election.P₁, "_D₁")] += graph.attributes[node][election.P₁_col]
            Δ_measures[string(election.name, "_", election.P₂, "_D₁")] += graph.attributes[node][election.P₂_col]
        end
        for racial_pop in racial_pops
            Δ_measures[string(racial_pop.name, "_D₁")] += graph.attributes[node][racial_pop.col_name]
        end
    end
    for node in proposal.D₂_nodes
        for election in elections
            Δ_measures[string(election.name, "_", election.P₁, "_D₂")] += graph.attributes[node][election.P₁_col]
            Δ_measures[string(election.name, "_", election.P₂, "_D₂")] += graph.attributes[node][election.P₂_col]
        end
        for racial_pop in racial_pops
            Δ_measures[string(racial_pop.name, "_D₂")] += graph.attributes[node][racial_pop.col_name]
        end
    end

    return Δ_measures
end
