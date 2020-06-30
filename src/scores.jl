abstract type AbstractScore end


struct DistrictAggregate <: AbstractScore
    """ A DistrictAggregate score is a simple sum of a particular property
        over all nodes in a given district. These scores are evaluated on every
        district in a plan.
    """
    name::String
    key::String
end


struct DistrictScore <: AbstractScore
    """ A CustomDistrictScore takes a user-supplied function that returns some
        quantity of interest given the nodes in a given district.
    """
    name::String
    score_fn::Function
end


struct PlanScore <: AbstractScore
    """ A CustomDistrictScore takes a user-supplied function that returns some
        quantity of interest given all the nodes in an entire plan and the
        corresonding Partition object.
    """
    name::String
    score_fn::Function
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
        else
            throw(e)
        end
    end
end


function eval_score_on_districts(graph::BaseGraph,
                                 partition::Partition,
                                 score::Union{DistrictScore,DistrictAggregate},
                                 districts::Array{Int, 1})
    """ Evaluates a user-supplied DistrictScore function repeatedly on districts
        specified by the districts array.
    """
    return [eval_score_on_district(graph, partition, score, d) for d in districts]
end


function eval_score_on_partition(graph::BaseGraph,
                                 partition::Partition,
                                 score::Union{DistrictScore,DistrictAggregate})
    """ Evaluates a user-supplied DistrictScore function on the nodes in a
     particular district.
    """
    all_districts = Array(1:graph.num_dists)
    return eval_score_on_districts(graph, partition, score, all_districts)
end


function eval_score_on_partition(graph::BaseGraph,
                                 partition::Partition,
                                 score::PlanScore)
    """ Evaluates a user-supplied PlanScore function on the entire
        partition.
    """
    try
        return score.score_fn(graph, partition)
    catch e # Check if the user-specified method was constructed incorrectly
        if isa(e, MethodError)
            error_msg = "PlanScore function must accept graph and array of nodes."
            throw(MethodError(error_msg))
        else
            throw(e)
        end
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
        else
            score_values[s.name] = eval_score_on_districts(graph, partition, s, Δ_districts)
        end
    end
    return score_values
end


function get_scores_at_step(all_scores::Array{}, step::Int)
    """ Returns the detailed scores of the partition at step `step`.

        Arguments:
            all_scores : List of scores of partitions at each step of
                         the Markov Chain
            step       : The step of the chain at which scores are desired
    """
    if step == 1
        return all_scores[1]
    end

    # we don't want to alter the data in all_scores
    scores = deepcopy(all_scores[1])

    for i in 2:step
        curr_scores = all_scores[i]
        (D₁, D₂) = all_scores[i]["dists"]

        for key in keys(scores)
            # TODO: this handling for non-array singular values is poor
            if key == "num_cut_edges"
                scores[key] = curr_scores[key]
                continue
            end
            scores[key][D₁] = curr_scores[key][1]
            scores[key][D₂] = curr_scores[key][2]
        end
    end

    return scores
end
