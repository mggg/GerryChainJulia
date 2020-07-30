abstract type AbstractScore end


struct DistrictAggregate <: AbstractScore
    """ A DistrictAggregate score is a simple sum of a particular property
        over all nodes in a given district.
    """
    name::String
    key::String
end


struct DistrictScore <: AbstractScore
    """ A DistrictScore takes a user-supplied function that returns some
        quantity of interest given the nodes in a given district. The signature
        of `score_fn` should be as follows:
            score_fn(graph::BaseGraph, district_nodes::BitSet, district::int)
    """
    name::Union{String, Missing}
    score_fn::Function
    DistrictScore(score_fn::Function) = new(missing, score_fn)
    DistrictScore(name::String, score_fn::Function) = new(name, score_fn)
end


struct PlanScore <: AbstractScore
    """ A PlanScore takes a user-supplied function that returns some
        quantity of interest given a Graph and corresponding Partition object.
        The signature of `score_fn` should be as follows:
            score_fn(graph::BaseGraph, partition::Partition)
    """
    name::Union{String, Missing}
    score_fn::Function
    PlanScore(score_fn::Function) = new(missing, score_fn)
    PlanScore(name::String, score_fn::Function) = new(name, score_fn)
end


struct CompositeScore <: AbstractScore
    """ A CompositeScore is just a group of scores that are run in sequence.
        CompositeScores are especially useful when the score functions depend
        upon/modify some shared state.
    """
    name::String
    scores::Array{S,1} where {S<:AbstractScore} # should be other AbstractScores
end


struct ChainScoreData
    """ The ChainScoreData object stores the values returned by score functions
        at every step of the chain as well as the scores themselves.
    """
    scores::Array{S,1} where {S<:AbstractScore} # scores that were measured on a particular chain
    step_values::Array{Dict{String, Any}} # array of Dicts which map {score name: score value}
end


struct ChainScoreQuery
    """ Can be used to extract scores from a ChainScoreData object via an iterator
    """
    requested_scores::Array{String,1} # scores that were measured on a particular chain
    chain_data::ChainScoreData
end


function DistrictAggregate(key::String)
    """ Initializes a DistrictAggregate score where the name and key are
        the same.
    """
    return DistrictAggregate(key, key)
end


function Base.iterate(query::ChainScoreQuery)
    """ This is 1/2 of the functions needed to make a ChainScoreQuery iterable.
        This is the function that calls when the iterable is first run. It checks
        that the query is valid (i.e., the user is asking for sensible scores)
        and then returns the values of the relevant scores for the first state
        in the chain.
    """
    chain_data = query.chain_data
    if isempty(chain_data.step_values) # check that some data exists
        return nothing
    end
    # check that all requested scores are in the ChainScoreData object
    score_vals = Dict{String, Any}()
    nested_keys = Dict{String, String}() # maps scores to the name of parent CompositeScore, if any
    score_names = isempty(query.requested_scores) ? collect(keys(chain_data.step_values[1])) : query.requested_scores
    for score_name in score_names
        score, nested_key = get_score_by_name(chain_data, score_name)
        if isnothing(score)
            throw(KeyError("No score in the ChainScoreData object matches the name: $score_name"))
        end
        # write value to initial dictionary
        if isnothing(nested_key)
            score_vals[score_name] = chain_data.step_values[1][score_name]
        else
            if !(haskey(score_vals, nested_key))
                score_vals[nested_key] = Dict{String, Any}()
            end
            score_vals[nested_key][score_name] = chain_data.step_values[1][nested_key][score_name]
        end
    end
    # keep track of next index and current dictionary
    return score_vals, (2, deepcopy(score_vals))
end


function Base.iterate(query::ChainScoreQuery, state::Tuple)
    """ This is 2/2 of the functions needed to make a ChainScoreQuery iterable.
        This is the function that calls after the first time the iterable runs.
        The "state" of the iterable is always captured in the step that we are on
        and the current dictionary.
    """
    step, score_vals = state
    chain_data = query.chain_data
    if step > length(chain_data.step_values) # end iteration
        return nothing
    end
    # update the dictionary of values to reflect districts changed in
    # the next state
    curr_scores = chain_data.step_values[step]
    (D₁, D₂) = chain_data.step_values[step]["dists"]
    update_dictionary!(score_vals, curr_scores, D₁, D₂)
    return score_vals, (step + 1, deepcopy(score_vals))
end


function eval_score_on_district(graph::BaseGraph,
                                partition::Partition,
                                score::DistrictAggregate,
                                district::Int)::Number
    """ Evaluates a DistrictAggregate score on the nodes in a particular
        district.
    """
    sum = 0
    for node in partition.dist_nodes[district]
        sum += graph.attributes[node][score.key]
    end
    return sum
end


function eval_score_on_district(graph::BaseGraph,
                                partition::Partition,
                                score::DistrictScore,
                                district::Int)
    """ Evaluates a user-supplied DistrictScore function on the nodes in a
        particular district.
    """
    try
        return score.score_fn(graph, partition.dist_nodes[district], district)
    catch e # Check if the user-specified method was constructed incorrectly
        if isa(e, MethodError)
            error_msg = "DistrictScore function must accept graph, array of nodes, and district index."
            throw(ArgumentError(error_msg))
        end
        throw(e)
    end
end


function eval_score_on_districts(graph::BaseGraph,
                                 partition::Partition,
                                 score::Union{DistrictScore,DistrictAggregate},
                                 districts::Array{Int, 1})::Array
    """ Evaluates a user-supplied DistrictScore function or DistrictAggregate
        score repeatedly on districts specified by the districts array.

        Returns an array of the form [a₁, a₂, ..., aₙ], where aᵢ corresponds to
        the value of the score for the district indexed by i in the `districts`
        array and n is the length of `districts`.
    """
    return [eval_score_on_district(graph, partition, score, d) for d in districts]
end


function eval_score_on_partition(graph::BaseGraph,
                                 partition::Partition,
                                 composite::CompositeScore)
    """ Evaluates the user-supplied functions in the CompositeScore scores
        array on each of the districts in the partition.

        Returns an Dict of the form:
        {
            # District-level scores
            d_score₁.name :     [a₁, a₂, ..., aₙ]
                ...
            d_scoreᵤ.name :     [b₁, b₂, ..., bₙ]
            # Partition-level scores
            p_score₁.name :     c,
                ...
            p_scoreᵥ.name :     d,
        }
    """
    return score_initial_partition(graph, partition, composite.scores)
end


function eval_score_on_partition(graph::BaseGraph,
                                 partition::Partition,
                                 score::Union{DistrictScore,DistrictAggregate})::Array
    """ Evaluates a user-supplied DistrictScore function or DistrictAggregate
        score on all districts in an entire plan.

        Returns an array of the form [a₁, a₂, ..., aₘ], where m is the number
        of districts in the plan.
    """
    all_districts = Array(1:graph.num_dists)
    return eval_score_on_districts(graph, partition, score, all_districts)
end


function eval_score_on_partition(graph::BaseGraph,
                                 partition::Partition,
                                 score::PlanScore)
    """ Evaluates a user-supplied PlanScore function on the entire partition.
    """
    try
        return score.score_fn(graph, partition)
    catch e # Check if the user-specified method was constructed incorrectly
        if isa(e, MethodError)
            error_msg = "PlanScore function must accept graph and partition."
            throw(ArgumentError(error_msg))
        end
        throw(e)
    end
end


function score_initial_partition(graph::BaseGraph,
                                 partition::Partition,
                                 scores::Array{S, 1}) where {S<:AbstractScore}
    """ Returns a dictionary of scores for the initial partition. The dictionary
        has the following form (where n is the number of districts, u is the
        number of district-level scores, v is the number of partition-level
        scores, and t is the number of composite scores):
        {
            # District-level scores
            d_score₁.name :     [a₁, a₂, ..., aₙ]
                ...
            d_scoreᵤ.name :     [b₁, b₂, ..., bₙ]
            # Partition-level scores
            p_score₁.name :     c,
                ...
            p_scoreᵥ.name :     d,
            # Composite scores
            c_score₁.name :
                {
                    c_score₁.scores[1].name :     ...
                        ...
                }
                ...
            c_scoreₜ.name :
                {
                    ...
                }
        }
    """
    score_values = Dict{String, Any}()
    for s in scores
        value = eval_score_on_partition(graph, partition, s)
        if !ismissing(s.name) # nameless scores should not be stored
            score_values[s.name] = value
        end
    end
    return score_values
end

function score_partition_from_proposal(graph::BaseGraph,
                                       partition::Partition,
                                       proposal::AbstractProposal,
                                       scores::Array{S, 1}) where {S<:AbstractScore}
    """ Returns a Dictionary of (a) updated district-level scores for districts
        that were altered after `proposal` was accepted, (b) partition-level
        scores, and (c) composite scores, that may be comprised of scores from
        (a) or (b).

        For example, suppose district 4's new White population is 43 and
        the new Sen2010_Dem population is 62, district 8's new White population
        is 22 and new Sen2010_Dem population is 66. The Δ scores would
        look like:
            {
                "num_cut_edges" : partition.num_cut_edges,
                "dists"         : (5, 8),
                "White"         : [43, 22],
                "Sen2010_Dem"   : [62, 66],
            }

    """
    score_values = Dict{String, Any}()
    Δ_districts = [proposal.D₁, proposal.D₂]
    score_values["dists"] = Δ_districts
    for s in scores
        if s isa PlanScore
            value = eval_score_on_partition(graph, partition, s)
        elseif s isa CompositeScore
            # ensure that district-level scores in the CompositeScore are only
            # evaluated on changed districts
            value = score_partition_from_proposal(graph, partition, proposal, s.scores)
            delete!(value, "dists") # remove redundant dists key
        else # efficiently calculate & store scores only on changed districts
            value = eval_score_on_districts(graph, partition, s, Δ_districts)
        end
        if !ismissing(s.name)
            score_values[s.name] = value
        end
    end
    return score_values
end


function update_dictionary!(original::Dict{String, Any},
                            update::Dict{String, Any},
                            D₁::Int,
                            D₂::Int)
    """ Modifies a Dict in-place by merging it with another Dict
        that contains "updates" to the former Dict. Runs recursively when there
        are nested Dicts. Helper function for ChainScoreQuery iterator.
    """
    for key in keys(original)
        if update[key] isa Array # district-level score
            original[key][D₁] = update[key][1]
            original[key][D₂] = update[key][2]
        elseif update[key] isa Dict # composite score
            update_dictionary!(original[key], update[key], D₁, D₂)
        else
            original[key] = update[key]
        end
    end
end


function get_scores_at_step(chain_data::ChainScoreData,
                            step::Int;
                            score_names::Array{String,1}=String[])::Dict{String, Any}
    """ Returns the detailed scores of the partition at step `step`. If no
        scores are passed in, all scores are returned by default. Here, step=0
        represents the score of the original (initial) partition, so step=t
        will return the scores of the plan that was produced after taking
        t steps of the Markov chain.

        Arguments:
            chain_data   : ChainScoreData object containing scores of partitions
                           at each step of the Markov Chain
            step         : The step of the chain at which scores are desired
            score_names  : An optional array of Strings representing the scores
                           for which the user is requesting the values
    """
    # we don't want to alter the data in all_scores
    query = ChainScoreQuery(score_names, chain_data)

    score_vals = Dict{String, Any}()
    for (i, step_scores) in enumerate(query)
        if i > step
            score_vals = step_scores
            break
        end
    end
    return score_vals
end


function get_score_by_name(chain_data::ChainScoreData, score_name::String)
    """ Private helper function to extract the AbstractScore object whose name
        matches `score_name` from the ChainScoreData object. Returns two values:
        first, the AbstractScore object itself, and second, the name of the
        CompositeScore it is nested within (if it is nested within one at all.)
    """
    index = findfirst(s -> !ismissing(s.name) && s.name == score_name, chain_data.scores)
    if index == nothing
        # Check if score is nested inside a CompositeScore
        composite_scores = filter(s -> s isa CompositeScore, chain_data.scores)
        for c in composite_scores
            index = findfirst(s -> !ismissing(s.name) && s.name == score_name, c.scores)
            if index != nothing
                return c.scores[index], c.name # return score and nested key
            end
        end
        return nothing, nothing # no score, no nested key
    end
    return chain_data.scores[index], nothing # no nested key
end


function get_score_values(all_scores::Array{Dict{String, Any}, 1},
                          score::Union{DistrictAggregate, DistrictScore};
                          nested_key::Union{String,Nothing}=nothing)::Array
    """ Helper function that returns the value of specified
        DistrictScore/DistrictAggregate score at every step of the chain.

        Arguments:
            all_scores  : List of scores of partitions at each step of
                         the Markov Chain
            score       : DistrictScore of interest
            nested_key  : If the score is nested within a CompositeScore, this
                          argument provides the CompositeScore's name
    """
    # check if score is nested inside a CompositeScore
    nested = nested_key != nothing
    init_vals = nested ? all_scores[1][nested_key][score.name] : all_scores[1][score.name]
    num_districts = length(init_vals)
    # create a matrix that is (num states of chain, num districts) to record
    # values of score
    score_table = Array{typeof(init_vals[1]), 2}(undef, length(all_scores), num_districts)
    # we don't want to alter the data in all_scores
    score_table[1, :] = deepcopy(init_vals)

    for i in 2:length(all_scores)
        score_table[i, :] = deepcopy(score_table[i-1, :])
        (D₁, D₂) = all_scores[i]["dists"]
        curr_scores = nested ? all_scores[i][nested_key] : all_scores[i]
        score_table[i, D₁] = curr_scores[score.name][1]
        score_table[i, D₂] = curr_scores[score.name][2]
    end

    return score_table
end


function get_score_values(all_scores::Array{Dict{String, Any}, 1},
                          score::PlanScore;
                          nested_key::Union{String,Nothing}=nothing)::Array
    """ Helper function that returns the value of specified PlanScore at every
        step of the chain.

        Arguments:
            all_scores  : List of scores of partitions at each step of
                          the Markov Chain
            score       : PlanScore of interest
            nested_key  : If the score is nested within a CompositeScore, this
                          argument provides the CompositeScore's name
    """
    num_states = length(all_scores)
    if nested_key == nothing
        return deepcopy([all_scores[i][score.name] for i in 1:num_states])
    end
    return deepcopy([all_scores[i][nested_key][score.name] for i in 1:num_states])
end


function get_score_values(all_scores::Array{Dict{String, Any}, 1},
                          composite::CompositeScore)::Dict{String, Array}
    """ Helper function that returns the value of specified CompositeScore at
        every step of the chain.

        Arguments:
            all_scores  : List of scores of partitions at each step of
                          the Markov Chain
            composite   : CompositeScore of interest
    """
    fetch_vals = s -> get_score_values(all_scores, s, nested_key = composite.name)
    return Dict(s.name => fetch_vals(s) for s in composite.scores)
end


function get_score_values(chain_data::ChainScoreData, score_name::String)
    """ Returns the value of specified score at every step of the chain.

        Arguments:
            chain_data   : ChainScoreData object containing scores of partitions
                           at each step of the Markov Chain
            score_name   : Name of the score of interest
    """
    score, nested_key = get_score_by_name(chain_data, score_name)
    if score == nothing
        throw(ArgumentError("No score with requested name found."))
    elseif score isa CompositeScore
        return get_score_values(chain_data.step_values, score)
    end
    return get_score_values(chain_data.step_values, score, nested_key=nested_key)
end


function flattened_score_names(chain_data::ChainScoreData)::Array{String, 1}
    """ Simple helper function for `save_scores`. Extracts the names of all of
        the district/plan-level scores (including those nested within
        CompositeScores, hence the term "flattened").
    """
    score_names = String[]
    for score in chain_data.scores
        if score isa CompositeScore # add children score names
            named_scores = filter(s -> !ismissing(s.name), score.scores)
            append!(score_names, [s.name for s in named_scores])
        else
            push!(score_names, score.name)
        end
    end
    return score_names
end


function save_scores(filename::String,
                     chain_data::ChainScoreData,
                     score_names::Array{String,1}=String[])
    """ Save the `scores` in a CSV file named `filename`. We iterate through
        each state in the history of the chain and write the CSV row-by-row.
    """
    open(filename, "w") do f
        if isempty(score_names) # by default, export all scores from chain
            score_names = flattened_score_names(chain_data)
        end

        column_names = String[] # colum names of the CSV
        nested_keys = Dict{String, String}() # map score key to nested key, if any

        num_districts = nothing
        for score_name in score_names
            score, nested_key = get_score_by_name(chain_data, score_name)
            if isnothing(score)
                throw(KeyError("No score in the ChainScoreData object matches the name: $score_name"))
            elseif score isa DistrictScore || score isa DistrictAggregate
                if isnothing(num_districts)
                    initial_vals = chain_data.step_values[1]
                    if !isnothing(nested_key)
                        initial_vals = initial_vals[nested_key]
                    end
                    num_districts = length(initial_vals[score_name])
                end
                # add new column for each district
                district_columns = ["$(score_name)_$(i)" for i in 1:num_districts]
                append!(column_names, district_columns)
            elseif score isa CompositeScore
                throw(ArgumentError("Cannot automatically save a CompositeScore to CSV. You may save a CompositeScore's child score."))
            else # must be PlanScore
                push!(column_names, score_name)
            end
            if !isnothing(nested_key)
                nested_keys[score_name] = nested_key
            end
        end
        # write column names
        colname_row = hcat(column_names...) # turn into 1xn array
        writedlm(f, colname_row, ',') # write intial row of column names

        # iterate through all steps of chain
        query = ChainScoreQuery(score_names, chain_data)
        for step_values in query
            row_values = []
            for key in score_names
                if haskey(nested_keys, key)
                    nested_key = nested_keys[key]
                    append!(row_values, step_values[nested_key][key])
                else
                    append!(row_values, step_values[key])
                end
            end
            # write one row for every state of the chain
            writedlm(f, hcat(row_values...), ',')
        end
    end
end
