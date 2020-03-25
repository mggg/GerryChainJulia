abstract type AbstractMeasure end

struct Election <: AbstractMeasure
    name::AbstractString
    P₁::AbstractString
    P₂::AbstractString
    P₁_col::AbstractString
    P₂_col::AbstractString
end

struct RacePopulation <: AbstractMeasure
    name::AbstractString
    col_name::AbstractString
end

function initialize_dist_tallies(elections::Array{Election, 1},
                                 racial_pops::Array{RacePopulation, 1})
    """ Initializes a Dict of the form

            { ElectionName_PartyName : 0,
              RacePopulation         : 0
            }

        for each election in `elections` and race population in `racial_pops`.

        Eg: { SEN2010_Dem : 0,
              SEN2010_Rep : 0,
              Hispanic    : 0,
              White       : 0
            }
    """
    dist_tallies = Dict{String, Int}()
    for election in elections
        dist_tallies[string(election.name, "_", election.P₁)] = 0
        dist_tallies[string(election.name, "_", election.P₂)] = 0
    end
    for racial_pop in racial_pops
        dist_tallies[racial_pop.name] = 0
    end
    return dist_tallies
end

function tally_counts!(dist_tallies::Dict{String, Int},
                       graph::BaseGraph,
                       partition::Partition,
                       dist::Int,
                       elections::Array{Election, 1},
                       racial_pops::Array{RacePopulation, 1})
    """ Fill the `dist_tallies` with the counts in each election in `elections` and
        each RacePopulation in `racial_pops` for district `dist` in `partition`
    """
    for node in partition.dist_nodes[dist]
        for election in elections
            dist_tallies[string(election.name, "_", election.P₁)] += graph.attributes[node][election.P₁_col]
            dist_tallies[string(election.name, "_", election.P₂)] += graph.attributes[node][election.P₂_col]
        end
        for racial_pop in racial_pops
            dist_tallies[racial_pop.name] += graph.attributes[node][racial_pop.col_name]
        end
    end
end

function update_measures!(measures::Dict{String, Any},
                          dist_tallies::Dict{String, Int},
                          elections::Array{Election, 1},
                          racial_pops::Array{RacePopulation, 1})
    """ Update the `measures` Dictionary with the information in `dist_tallies`.
        The `measures` dictionary holds counts for the entire partition, and the
        `dist_tallies` dictionary only holds the counts for a single district.
    """
    for election in elections
        push!(measures[string(election.name, "_", election.P₁)], dist_tallies[string(election.name, "_", election.P₁)])
        push!(measures[string(election.name, "_", election.P₂)], dist_tallies[string(election.name, "_", election.P₂)])
    end
    for racial_pop in racial_pops
        push!(measures[racial_pop.name], dist_tallies[racial_pop.name])
    end
end

function initialize_measures(partition::Partition,
                             elections::Array{Election, 1},
                             racial_pops::Array{RacePopulation, 1})
    """ Initalizes a Dict of the form

            { num_cut_edges          : partition.num_cut_edges
              ElectionName_PartyName : Array{Int, 1}(),
              RacePopulation         : Array{Int, 1}()
            }

        for each election in `elections` and racial_pop in `racial_pops`.

        Eg: { num_cut_edges : 50
              SEN2010_Dem   : Array{Int, 1}(),
              SEN2010_Rep   : Array{Int, 1}(),
              Hispanic      : Array{Int, 1}(),
              White         : Array{Int, 1}(),
            }

    """
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

function update_measures!(measures::Dict{String, Any},
                          nodes::BitSet,
                          graph::BaseGraph,
                          elections::Array{Election, 1},
                          racial_pops::Array{RacePopulation, 1},
                          dist::Int)
    """ Update the `measures` Dict with the counts for each election in `elections`
        and each RacePopulation in `racial_pops` of district `dist`
    """
    for node in nodes
        for election in elections
            measures[string(election.name, "_", election.P₁)][dist] += graph.attributes[node][election.P₁_col]
            measures[string(election.name, "_", election.P₂)][dist] += graph.attributes[node][election.P₂_col]
        end
        for racial_pop in racial_pops
            measures[string(racial_pop.name)][dist] += graph.attributes[node][racial_pop.col_name]
        end
    end
end

function initialize_Δ_measures(partition::Partition,
                               elections::Array{Election, 1},
                               racial_pops::Array{RacePopulation, 1},
                               proposal::AbstractProposal)
    """ Returns a Dict of the form

        {
            "num_cut_edges" : partition.num_cut_edges,
            "dists"         : (D₁, D₂).
            Election_Party  : [0, 0],
            Race_Name       : [0, 0]
        }

    """
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

function get_detailed_measures(graph::BaseGraph,
                               partition::Partition,
                               elections::Array{Election, 1},
                               racial_pops::Array{RacePopulation, 1})
    """ Return all measures collected for `partition`

        Eg. {
              "num_cut_edges" : partition.num_cut_edges,
              Election_Party  : [x₁, x₂, ..., xᵤ]
              Race_Pop        : [y₁, y₂, ..., yᵤ]
            }

        where each value at index i of an array is the count at district i,
        for election in `elections` and racial_pop in `racial_pops`.
    """
    measures = initialize_measures(partition, elections, racial_pops)

    for dist in 1:graph.num_dists
        dist_tallies = initialize_dist_tallies(elections, racial_pops)
        tally_counts!(dist_tallies, graph, partition, dist, elections, racial_pops)
        update_measures!(measures, dist_tallies, elections, racial_pops)
    end

    return measures
end

function get_Δ_measures(graph::BaseGraph,
                        partition::Partition,
                        elections::Array{Election, 1},
                        racial_pops::Array{RacePopulation, 1},
                        proposal::AbstractProposal)
    """ Returns only the change in measures from the last partition after
        `proposal` was accepted.

        For example, suppose district 4's new White population is 43 and
            the new Sen2010_Dem population is 62, district 8's new White population
            is 22 and new Sen2010_Dem population is 66. The Δ measures would
            look like:
                {
                    "num_cut_edges" : partition.num_cut_edges,
                    "dists"         : (5, 8),
                    "White"         : [43, 22],
                    "Sen2010_Dem"   : [62, 66],
                }

    """
    Δ_measures = initialize_Δ_measures(partition, elections, racial_pops, proposal)
    update_measures!(Δ_measures, proposal.D₁_nodes, graph, elections, racial_pops, 1)
    update_measures!(Δ_measures, proposal.D₂_nodes, graph, elections, racial_pops, 2)
    return Δ_measures
end

function get_measures(graph::BaseGraph,
                      partition::Partition,
                      elections::Array{Election, 1},
                      racial_pops::Array{RacePopulation, 1},
                      steps_taken::Int=1,
                      proposal::AbstractProposal=DummyProposal("Optional argument used when you want Δ metrics."))
    """ Return all the measures of interest of `partition` for election in `elections`
        and RacePopulation in racial_pops.

        Other Arguments:
            steps_taken : Number of steps in the chain

        Important:
            The `proposal` is the step the chain just took. It is used to identify
            only the districts just modified, so only the change in information
            from those districts can be stored instead of all the measures.
            If steps_taken == 1, then it returns the detailed measures instead
            of just the Δ measures.
    """
    if steps_taken == 1
        measures = get_detailed_measures(graph, partition, elections, racial_pops)
    else
        measures = get_Δ_measures(graph, partition, elections, racial_pops, proposal)
    end
    return measures
end

function get_measures_at_step(all_measures::Array{}, step)
    """ Returns the detailed measures of the partition at step `step`.

        Arguments:
            all_measures : List of measures of partitions at each step of
                           the Markov Chain
            step         : The step of the chain at which measures are desired
    """
    if step == 1
        return all_measures[1]
    end

    # we don't want to alter the data in all_measures
    measures = deepcopy(all_measures[1])

    for i in 2:step
        curr_measures = all_measures[i]
        (D₁, D₂) = all_measures[i]["dists"]

        for key in keys(measures)
            # TODO: this handling for non-array singular values is poor
            if key == "num_cut_edges"
                measures[key] = curr_measures[key]
                continue
            end
            measures[key][D₁] = curr_measures[key][1]
            measures[key][D₂] = curr_measures[key][2]
        end
    end

    return measures
end
