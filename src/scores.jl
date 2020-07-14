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
    name::String
    score_fn::Function
end


struct PlanScore <: AbstractScore
    """ A PlanScore takes a user-supplied function that returns some
        quantity of interest given a Graph and corresponding Partition object.
        The signature of `score_fn` should be as follows:
            score_fn(graph::BaseGraph, partition::Partition)
    """
    name::String
    score_fn::Function
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
        at every step of the chain.
    """
    scores::Array{S,1} where {S<:AbstractScore} # should be other AbstractScores
    step_values::Array{Dict{String, Any}} # array of Dicts which map {score name: score value}
end


function DistrictAggregate(key::String)
    """ Initializes a DistrictAggregate score where the name and key are
        the same.
    """
    return DistrictAggregate(key, key)
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
            throw(MethodError(error_msg))
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
    composite_results = Dict{String, Any}()
    for s in composite.scores
        composite_results[s.name] = eval_score_on_partition(graph, partition, s)
    end
    return composite_results
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
            throw(MethodError(error_msg))
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
        score_values[s.name] = eval_score_on_partition(graph, partition, s)
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
            score_values[s.name] = eval_score_on_partition(graph, partition, s)
        elseif s isa CompositeScore
            # ensure that district-level scores in the CompositeScore are only
            # evaluated on changed districts
            score_values[s.name] = score_partition_from_proposal(graph, partition, proposal, s.scores)
            delete!(score_values[s.name], "dists") # remove redundant dists key
        else # efficiently calculate & store scores only on changed districts
            score_values[s.name] = eval_score_on_districts(graph, partition, s, Δ_districts)
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
        are nested Dicts. Helper function for `get_scores_at_step.`
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


function get_scores_at_step(all_scores::Array{Dict{String, Any}, 1},
                            step::Int;
                            scores::Array{S,1}=AbstractScore[]) where {S <: AbstractScore}
    """ Returns the detailed scores of the partition at step `step`. If no
        scores are passed in, all scores are returned by default. Here, step=0
        represents the score of the original (initial) partition, so step=t
        will return the scores of the plan that was produced after taking
        t steps of the Markov chain.

        Arguments:
            all_scores : List of scores of partitions at each step of
                         the Markov Chain
            step       : The step of the chain at which scores are desired
            scores     : An optional array of AbstractScores for which the user
                         is requesting the values
    """
    # we don't want to alter the data in all_scores
    score_vals = Dict{String, Any}()
    score_names = [s.name for s in scores]
    if isempty(score_names) # return all scores by default
        score_names = collect(keys(all_scores[1]))
    end
    foreach(name -> score_vals[name] = all_scores[1][name], score_names)

    for i in 1:step
        curr_scores = all_scores[i + 1]
        (D₁, D₂) = all_scores[i + 1]["dists"]
        update_dictionary!(score_vals, curr_scores, D₁, D₂)
    end

    return score_vals
end


function get_score_values(all_scores::Array{Dict{String, Any}, 1},
                          score::Union{DistrictAggregate, DistrictScore};
                          nested_key::Union{String,Nothing}=nothing)::Array
    """ Returns the value of specified DistrictScore/DistrictAggregate score
        at every step of the chain.

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
    """ Returns the value of specified PlanScore at every step of the chain.

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
    """ Returns the value of specified CompositeScore at every step of the chain.

        Arguments:
            all_scores  : List of scores of partitions at each step of
                          the Markov Chain
            composite   : CompositeScore of interest
    """
    fetch_vals = s -> get_score_values(all_scores, s, nested_key = composite.name)
    return Dict(s.name => fetch_vals(s) for s in composite.scores)
end


function save_scores(filename::String,
                     scores::Array{Dict{String, Any}, 1})
    """ Save the `scores` in a JSON file named `filename`.
    """
    open(filename, "w") do f
        JSON.print(f, scores)
    end
end
