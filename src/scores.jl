abstract type AbstractScore end


struct DistrictAggregate <: AbstractScore
    """ A DistrictAggregate score is a simple sum of a particular property
        over all nodes in a given district.
    """
    name::String
    key::String
end


struct DistrictScore <: AbstractScore
    """ A CustomDistrictScore takes a user-supplied function that returns some
        quantity of interest given the nodes in a given district.
    """
    name::String
    score_fn::Function # signature: f(graph::BaseGraph, nodes::BitSet)
end


struct PlanScore <: AbstractScore
    """ A CustomDistrictScore takes a user-supplied function that returns some
        quantity of interest given a Graph and corresponding Partition object.
    """
    name::String
    score_fn::Function # signature: f(graph::BaseGraph, partition::Partition)
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
        return score.score_fn(graph, partition.dist_nodes[district])
    catch e # Check if the user-specified method was constructed incorrectly
        if isa(e, MethodError)
            error_msg = "DistrictScore function must accept graph and array of nodes."
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
        number of district-level scores, and v is the number of partition-level
        scores):
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
        that were altered after `proposal` was accepted and (b) partition-level
        scores.

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
        else # efficiently calculate & store scores only on changed districts
            score_values[s.name] = eval_score_on_districts(graph, partition, s, Δ_districts)
        end
    end
    return score_values
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
        # scores at index i+1 represent the scores for the plan after i steps
        curr_scores = all_scores[i + 1]
        (D₁, D₂) = all_scores[i + 1]["dists"]

        for key in score_names
            if curr_scores[key] isa Array # district-level score
                score_vals[key][D₁] = curr_scores[key][1]
                score_vals[key][D₂] = curr_scores[key][2]
            else
                score_vals[key] = curr_scores[key]
            end
        end
    end

    return score_vals
end


function save_scores(filename::String,
                     scores::Array{Dict{String, Any}, 1})
    """ Save the `scores` in a JSON file named `filename`.
    """
    open(filename, "w") do f
        JSON.print(f, scores)
    end
end
