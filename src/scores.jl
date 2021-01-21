abstract type AbstractScore end

"""
    DistrictAggregate(name::String,
                      key::String)

A DistrictAggregate score is a simple sum of a particular property
over all nodes in a given district.
"""
struct DistrictAggregate <: AbstractScore
    name::String
    key::String
end

"""
    DistrictScore(name::Union{String, Missing}
                  score_fn::Function)

A `DistrictScore` takes a user-supplied function that returns some
quantity of interest given the nodes in a given district. The signature
of `score_fn` should be as follows:
    `score_fn(graph::BaseGraph, district_nodes::BitSet, district::int)`
"""
struct DistrictScore <: AbstractScore
    name::Union{String,Missing}
    score_fn::Function
    DistrictScore(score_fn::Function) = new(missing, score_fn)
    DistrictScore(name::String, score_fn::Function) = new(name, score_fn)
end

"""
    PlanScore(name::Union{String, Missing},
              score_fn::Function)

A `PlanScore` takes a user-supplied function that returns some quantity of
interest given a `BaseGraph` and corresponding `Partition` object.

The signature of `score_fn` should be as follows:
    `score_fn(graph::BaseGraph, partition::Partition)`
"""
struct PlanScore <: AbstractScore
    name::Union{String,Missing}
    score_fn::Function
    PlanScore(score_fn::Function) = new(missing, score_fn)
    PlanScore(name::String, score_fn::Function) = new(name, score_fn)
end

"""
    CompositeScore(name::String,
                   scores::Array{S,1}) where {S<:AbstractScore} # should be other AbstractScores

A `CompositeScore` is just a group of scores that are run in sequence.
`CompositeScore`s are especially useful when the score functions depend
upon/modify some shared state.
"""
struct CompositeScore <: AbstractScore
    name::String
    scores::Array{S,1} where {S<:AbstractScore} # should be other AbstractScores
end


"""
    ChainScoreData(scores::Array{S,1} where {S<:AbstractScore} # scores that were measured on a particular chain
                   step_values::Array{Dict{String, Any}}) # array of Dicts which map {score name: score value}

The `ChainScoreData` object stores the values returned by score functions
at every step of the chain as well as the scores themselves.
"""
struct ChainScoreData
    scores::Array{S,1} where {S<:AbstractScore} # scores that were measured on a particular chain
    step_values::Array{Dict{String,Any}} # array of Dicts which map {score name: score value}
end


"""
    ChainScoreQuery(requested_scores::Array{String, 1}, # scores that were measured on a particular chain
                    chain_data::ChainScoreData)

Can be used to extract scores from a `ChainScoreData` object via an iterator.
"""
struct ChainScoreQuery
    requested_scores::Array{String,1} # scores that were measured on a particular chain
    chain_data::ChainScoreData
end

"""
    DistrictAggregate(key::String)

Initializes a DistrictAggregate score where the name and key are the same.
"""
function DistrictAggregate(key::String)
    return DistrictAggregate(key, key)
end

"""
This is 1/2 of the functions needed to make a ChainScoreQuery iterable.
This is the function that calls when the iterable is first run. It checks
that the query is valid (i.e., the user is asking for sensible scores)
and then returns the values of the relevant scores for the first state
in the chain.
"""
function Base.iterate(query::ChainScoreQuery)
    chain_data = query.chain_data
    if isempty(chain_data.step_values) # check that some data exists
        return nothing
    end
    # check that all requested scores are in the ChainScoreData object
    score_vals = Dict{String,Any}()
    nested_keys = Dict{String,String}() # maps scores to the name of parent CompositeScore, if any
    score_names =
        isempty(query.requested_scores) ? collect(keys(chain_data.step_values[1])) :
        query.requested_scores
    for score_name in score_names
        score, nested_key = get_score_by_name(chain_data, score_name)
        if isnothing(score)
            throw(
                KeyError(
                    "No score in the ChainScoreData object matches the name: $score_name",
                ),
            )
        end
        # write value to initial dictionary
        if isnothing(nested_key)
            score_vals[score_name] = chain_data.step_values[1][score_name]
        else
            if !(haskey(score_vals, nested_key))
                score_vals[nested_key] = Dict{String,Any}()
            end
            score_vals[nested_key][score_name] =
                chain_data.step_values[1][nested_key][score_name]
        end
    end
    # keep track of next index and current dictionary
    return score_vals, (2, deepcopy(score_vals))
end

"""
This is 2/2 of the functions needed to make a ChainScoreQuery iterable.
This is the function that calls after the first time the iterable runs.
The "state" of the iterable is always captured in the step that we are on
and the current dictionary.
"""
function Base.iterate(query::ChainScoreQuery, state::Tuple)
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

"""
    eval_score_on_district(graph::BaseGraph,
                           partition::Partition,
                           score::DistrictAggregate,
                           district::Int)::Number

Evaluates a `DistrictAggregate` score on the nodes in a particular
`district`.
"""
function eval_score_on_district(
    graph::BaseGraph,
    partition::Partition,
    score::DistrictAggregate,
    district::Int,
)::Number
    try
        sum = 0
        for node in partition.dist_nodes[district]
            sum += graph.attributes[node][score.key]
        end
        return sum
    catch e
        if isa(e, MethodError)
            error_msg = string(
                "The DistrictAggregate Score ",
                score.name,
                " seems to be of type String when a Number was ",
                "expected. Try calling the function ",
                "coerce_aggregated_attributes!(graph, scores_array) ",
                "before running the chain.",
            )
            throw(ArgumentError(error_msg))
        end
        throw(e)
    end
end


"""
    eval_score_on_district(graph::BaseGraph,
                           partition::Partition,
                           score::DistrictScore,
                           district::Int)

Evaluates a user-supplied `DistrictScore` function on the nodes in a
particular `district`.
"""
function eval_score_on_district(
    graph::BaseGraph,
    partition::Partition,
    score::DistrictScore,
    district::Int,
)
    try
        return score.score_fn(graph, partition.dist_nodes[district], district)
    catch e # Check if the user-specified method was constructed incorrectly
        if !applicable(score.score_fn, graph, partition)
            error_msg = "DistrictScore function must accept graph, array of nodes, and district index."
            throw(ArgumentError(error_msg))
        end
        throw(e)
    end
end


"""
    eval_score_on_districts(graph::BaseGraph,
                            partition::Partition,
                            score::Union{DistrictScore,DistrictAggregate},
                            districts::Array{Int, 1})::Array

Evaluates a user-supplied `DistrictScore` function or `DistrictAggregate`
score repeatedly on districts specified by the districts array.

Returns an array of the form [a₁, a₂, ..., aᵢ], where aᵢ corresponds to
the value of the score for the district indexed by `i` in the `districts`
array and `n` is the length of `districts`.
"""
function eval_score_on_districts(
    graph::BaseGraph,
    partition::Partition,
    score::Union{DistrictScore,DistrictAggregate},
    districts::Array{Int,1},
)::Array
    return [eval_score_on_district(graph, partition, score, d) for d in districts]
end


"""
    eval_score_on_partition(graph::BaseGraph,
                            partition::Partition,
                            composite::CompositeScore)

Evaluates the user-supplied functions in the `CompositeScore` scores
array on each of the districts in the partition.

Returns an Dict of the form:

```json
{
    # District-level scores
    d\\_score₁.name :     [a₁, a₂, ..., aᵢ]
        ...
    d\\_scoreᵤ.name :     [b₁, b₂, ..., bᵢ]
    # Partition-level scores
    p\\_score₁.name :     c,
        ...
    p\\_scoreᵥ.name :     d,
}
```
"""
function eval_score_on_partition(
    graph::BaseGraph,
    partition::Partition,
    composite::CompositeScore,
)
    return score_initial_partition(graph, partition, composite.scores)
end


"""
Evaluates a user-supplied `DistrictScore` function or `DistrictAggregate` score
on  all districts in an entire plan.

*Returns* an array of the form [a₁, a₂, ..., aᵢ], where `i` is the number
of districts in the plan.
"""
function eval_score_on_partition(
    graph::BaseGraph,
    partition::Partition,
    score::Union{DistrictScore,DistrictAggregate},
)::Array
    all_districts = Array(1:partition.num_dists)
    return eval_score_on_districts(graph, partition, score, all_districts)
end


"""
    eval_score_on_partition(graph::BaseGraph,
                            partition::Partition,
                            score::PlanScore)

Evaluates a user-supplied `PlanScore` function on the entire partition.
"""
function eval_score_on_partition(graph::BaseGraph, partition::Partition, score::PlanScore)
    try
        return score.score_fn(graph, partition)
    catch e # Check if the user-specified method was constructed incorrectly
        if !applicable(score.score_fn, graph, partition)
            error_msg = "PlanScore function must accept graph and partition."
            throw(ArgumentError(error_msg))
        end
        throw(e)
    end
end


"""
    score_initial_partition(graph::BaseGraph,
                            partition::Partition,
                            scores::Array{S, 1}) where {S<:AbstractScore}

Returns a dictionary of scores for the initial partition. The dictionary
has the following form (where `n` is the number of districts, `u` is the
number of district-level scores, `v` is the number of partition-level
scores, and `i` is the number of composite scores):

```json
{
    # District-level scores
    d\\_score₁.name :     [a₁, a₂, ..., aᵤ]
        ...
    d\\_scoreᵤ.name :     [b₁, b₂, ..., bᵤ]
    # Partition-level scores
    p\\_score₁.name :     c,
        ...
    p\\_scoreᵥ.name :     d,
    # Composite scores
    c\\_score₁.name :
        {
            c\\_score₁.scores[1].name :     ...
                ...
        }
        ...
    c\\_scoreᵢ.name :
        {
            ...
        }
}
```
"""
function score_initial_partition(
    graph::BaseGraph,
    partition::Partition,
    scores::Array{S,1},
) where {S<:AbstractScore}
    score_values = Dict{String,Any}()
    for s in scores
        value = eval_score_on_partition(graph, partition, s)
        if !ismissing(s.name) # nameless scores should not be stored
            score_values[s.name] = value
        end
    end
    return score_values
end

"""
    score_partition_from_proposal(graph::BaseGraph,
                                  partition::Partition,
                                  proposal::AbstractProposal,
                                  scores::Array{S, 1}) where {S<:AbstractScore}

Returns a Dictionary of
- (a) updated district-level scores for districts that were altered
after `proposal` was accepted,
- (b) partition-level scores, and
(c) composite scores, that may be comprised of scores from (a) or (b).

For example, suppose district 4's new White population is 43 and
the new Sen2010\\_Dem population is 62, district 8's new White population
is 22 and new Sen2010\\_Dem population is 66. The Δ scores would
look like:
```json
    {
        "num_cut_edges" : partition.num\\_cut\\_edges,
        "dists"         : (5, 8),
        "White"         : [43, 22],
        "Sen2010_Dem"   : [62, 66],
    }
```
"""
function score_partition_from_proposal(
    graph::BaseGraph,
    partition::Partition,
    proposal::AbstractProposal,
    scores::Array{S,1},
) where {S<:AbstractScore}
    score_values = Dict{String,Any}()
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


"""
    update_dictionary!(original::Dict{String, Any},
                       update::Dict{String, Any},
                       D₁::Int,
                       D₂::Int)

Modifies a Dict in-place by merging it with another Dict that contains
`update` to the former Dict. Runs recursively when there are nested Dicts.
Helper function for ChainScoreQuery iterator.
"""
function update_dictionary!(
    original::Dict{String,Any},
    update::Dict{String,Any},
    D₁::Int,
    D₂::Int,
)
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

"""
    get_scores_at_step(chain_data::ChainScoreData,
                       step::Int;
                       score_names::Array{String,1}=String[])::Dict{String, Any}

Returns the detailed scores of the partition at step `step`. If no
scores are passed in, all scores are returned by default. Here, step=0
represents the score of the original (initial) partition, so step=t
will return the scores of the plan that was produced after taking
t steps of the Markov chain.

*Arguments:*
- chain_data   : ChainScoreData object containing scores of partitions
                 at each step of the Markov Chain
- step         : The step of the chain at which scores are desired
- score_names  : An optional array of Strings representing the scores
                 for which the user is requesting the values
"""
function get_scores_at_step(
    chain_data::ChainScoreData,
    step::Int;
    score_names::Array{String,1} = String[],
)::Dict{String,Any}
    # we don't want to alter the data in all_scores
    query = ChainScoreQuery(score_names, chain_data)

    score_vals = Dict{String,Any}()
    for (i, step_scores) in enumerate(query)
        if i > step
            score_vals = step_scores
            break
        end
    end
    return score_vals
end


"""
    get_score_by_name(chain_data::ChainScoreData,
                      score_name::String)

Private helper function to extract the `AbstractScore` object whose name
matches `score_name` from the `ChainScoreData` object.

*Returns* two values:
first, the `AbstractScore` object itself, and second, the name of the
`CompositeScore` it is nested within (if it is nested within one at all.)
"""
function get_score_by_name(chain_data::ChainScoreData, score_name::String)
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


"""
    get_score_values(all_scores::Array{Dict{String, Any}, 1},
                     score::Union{DistrictAggregate, DistrictScore};
                     nested_key::Union{String,Nothing}=nothing)::Array

Helper function that returns the value of specified
`DistrictScore`/`DistrictAggregate` score at every step of the chain.

*Arguments:*
- all_scores  : List of scores of partitions at each step of
                the Markov Chain
- score       : `DistrictScore` of interest
- nested_key  : If the score is nested within a `CompositeScore`, this
                argument provides the `CompositeScore`'s name
"""
function get_score_values(
    all_scores::Array{Dict{String,Any},1},
    score::Union{DistrictAggregate,DistrictScore};
    nested_key::Union{String,Nothing} = nothing,
)::Array
    # check if score is nested inside a CompositeScore
    nested = nested_key != nothing
    init_vals = nested ? all_scores[1][nested_key][score.name] : all_scores[1][score.name]
    num_districts = length(init_vals)
    # create a matrix that is (num states of chain, num districts) to record
    # values of score
    score_table = Array{typeof(init_vals[1]),2}(undef, length(all_scores), num_districts)
    # we don't want to alter the data in all_scores
    score_table[1, :] = deepcopy(init_vals)

    for i = 2:length(all_scores)
        score_table[i, :] = deepcopy(score_table[i-1, :])
        (D₁, D₂) = all_scores[i]["dists"]
        curr_scores = nested ? all_scores[i][nested_key] : all_scores[i]
        score_table[i, D₁] = curr_scores[score.name][1]
        score_table[i, D₂] = curr_scores[score.name][2]
    end

    return score_table
end


"""
    get_score_values(all_scores::Array{Dict{String, Any}, 1},
                     score::PlanScore;
                     nested_key::Union{String,Nothing}=nothing)::Array

Helper function that returns the value of specified `PlanScore` at every step of
the chain.

*Arguments:*
- all_scores  : List of scores of partitions at each step of
                the Markov Chain
- score       : `PlanScore` of interest
- nested_key  : If the score is nested within a `CompositeScore`, this
                argument provides the `CompositeScore`'s name
"""
function get_score_values(
    all_scores::Array{Dict{String,Any},1},
    score::PlanScore;
    nested_key::Union{String,Nothing} = nothing,
)::Array
    num_states = length(all_scores)
    if nested_key == nothing
        return deepcopy([all_scores[i][score.name] for i = 1:num_states])
    end
    return deepcopy([all_scores[i][nested_key][score.name] for i = 1:num_states])
end


"""
    get_score_values(all_scores::Array{Dict{String, Any}, 1},
                     composite::CompositeScore)::Dict{String, Array}

Helper function that returns the value of specified `CompositeScore` at
every step of the chain.

*Arguments:*
- all_scores  : List of scores of partitions at each step of
                the Markov Chain
- composite   : `CompositeScore` of interest
"""
function get_score_values(
    all_scores::Array{Dict{String,Any},1},
    composite::CompositeScore,
)::Dict{String,Array}
    fetch_vals = s -> get_score_values(all_scores, s, nested_key = composite.name)
    return Dict(s.name => fetch_vals(s) for s in composite.scores)
end


"""
    get_score_values(chain_data::ChainScoreData,
                     score_name::String)
Returns the value of specified score at every step of the chain.

*Arguments:*
- chain_data   : ChainScoreData object containing scores of partitions
                 at each step of the Markov Chain
- score_name   : Name of the score of interest
"""
function get_score_values(chain_data::ChainScoreData, score_name::String)
    score, nested_key = get_score_by_name(chain_data, score_name)
    if score == nothing
        throw(ArgumentError("No score with requested name found."))
    elseif score isa CompositeScore
        return get_score_values(chain_data.step_values, score)
    end
    return get_score_values(chain_data.step_values, score, nested_key = nested_key)
end


"""
    flattened_score_names(chain_data::ChainScoreData)::Array{String, 1}

Simple helper function for `save_scores`. Extracts the names of all of
the district/plan-level scores (including those nested within
CompositeScores, hence the term "flattened").
"""
function flattened_score_names(chain_data::ChainScoreData)::Array{String,1}
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


"""
    save_scores_to_csv(filename::String,
                       chain_data::ChainScoreData,
                       score_names::Array{String,1}=String[])

Save the `scores` in a CSV file named `filename`.
"""
function save_scores_to_csv(
    filename::String,
    chain_data::ChainScoreData,
    score_names::Array{String,1} = String[],
)
    open(filename, "w") do f
        if isempty(score_names) # by default, export all scores from chain
            score_names = flattened_score_names(chain_data)
        end

        column_names = String[] # column names of the CSV
        nested_keys = Dict{String,String}() # map score key to nested key, if any

        num_districts = nothing
        for score_name in score_names
            score, nested_key = get_score_by_name(chain_data, score_name)
            if score isa DistrictScore || score isa DistrictAggregate
                if isnothing(num_districts)
                    initial_vals = chain_data.step_values[1]
                    if !isnothing(nested_key)
                        initial_vals = initial_vals[nested_key]
                    end
                    num_districts = length(initial_vals[score_name])
                end
                # add new column for each district
                district_columns = ["$(score_name)_$(i)" for i = 1:num_districts]
                append!(column_names, district_columns)
            elseif score isa CompositeScore
                throw(
                    ArgumentError(
                        "Cannot automatically save a CompositeScore to CSV. You may save a CompositeScore's child score.",
                    ),
                )
            elseif score isa PlanScore
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


"""
    save_scores_to_json(filename::String,
                        chain_data::ChainScoreData,
                        score_names::Array{String,1}=String[])

Save the `scores` in a JSON file named `filename`.
"""
function save_scores_to_json(
    filename::String,
    chain_data::ChainScoreData,
    score_names::Array{String,1} = String[],
)
    open(filename, "w") do f
        if isempty(score_names) # by default, export all scores from chain
            score_names = flattened_score_names(chain_data)
        end

        # iterate through all steps of chain
        query = ChainScoreQuery(score_names, chain_data)
        first_entry = true
        # note that we manually write the brackets and commas because the point
        # of writing the scores to the file row-by-row is that we never hold
        # the entire array of scores in main memory. this may be brittle!
        println(f, "[")
        for step_values in query
            first_entry ? first_entry = false : print(f, ",\n")
            print(f, JSON.json(step_values))
        end
        print(f, "\n]")
    end
end


"""
    save_scores_to_hdf5(filename::String,
                        chain_data::ChainScoreData,
                        score_names::Array{String,1}=String[])

Save the `scores` in a hdf5 file named `filename`.
"""
function save_scores_to_hdf5(
    filename::String,
    chain_data::ChainScoreData,
    score_names::Array{String,1} = String[],
)
    h5open(filename, "w") do f
        if isempty(score_names) # by default, export all scores from chain
            score_names = flattened_score_names(chain_data)
        end

        nested_keys = Dict{String,String}() # map score key to nested key, if any
        # check for CompositeScores, which can't be saved to HDF5
        for score_name in score_names
            score, nested_key = get_score_by_name(chain_data, score_name)
            if score isa CompositeScore
                throw(
                    ArgumentError(
                        "Cannot automatically save a CompositeScore to HDF5. You may save a CompositeScore's child score.",
                    ),
                )
            end
            if !isnothing(nested_key)
                nested_keys[score_name] = nested_key
            end
        end

        query = ChainScoreQuery(score_names, chain_data)
        scores_datasets = Dict{String,Any}()
        num_steps = length(chain_data.step_values)
        for (idx, step_values) in enumerate(query)
            for key in score_names
                # fetch the data
                data = nothing
                if haskey(nested_keys, key)
                    nested_key = nested_keys[key]
                    data = step_values[nested_key][key]
                else
                    data = step_values[key]
                end
                # create the initial datasets
                if idx == 1
                    scores_datasets[key] = create_dataset(
                        f,
                        key,
                        datatype(data),
                        dataspace(num_steps, length(data)),
                    )
                end
                # write the data to the hdf5 file
                scores_datasets[key][idx, :] = data
            end
        end
    end
end


"""
    num_cut_edges(name::String)::PlanScore

Returns a `PlanScore` that tracks the number of cut edges in a particular plan.
"""
function num_cut_edges(name::String)::PlanScore
    function score_fn(graph::BaseGraph, partition::Partition)
        return partition.num_cut_edges
    end
    return PlanScore(name, score_fn)
end

"""
    coerce_attribute_type!(graph::BaseGraph,
                           key::String,
                           new_type::DataType)

Coerces attribute `key` in `graph` to be of type `new_type` if it is of
type String.
"""
function coerce_attribute_type!(graph::BaseGraph, key::String, new_type::DataType)
    for node = 1:graph.num_nodes
        if graph.attributes[node][key] isa String
            graph.attributes[node][key] = parse(new_type, graph.attributes[node][key])
            @info string(
                "Key ",
                key,
                " attribute was of type String, ",
                "but was converted to type ",
                new_type,
            ) maxlog = 1
        end
    end
end

"""
    coerce_aggregated_attributes!(graph::BaseGraph,
                                  scores::Array{S, 1}) where {S<:AbstractScore}

Coerces DistrictAggregate attributes in `scores` to be Floats if they are
Strings.
"""
function coerce_aggregated_attributes!(
    graph::BaseGraph,
    scores::Array{S,1},
) where {S<:AbstractScore}
    for score in scores
        if score isa DistrictAggregate
            coerce_attribute_type!(graph, score.key, Float64)
        end
    end
end
