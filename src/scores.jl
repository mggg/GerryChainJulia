
abstract type AbstractScore end
abstract type DistrictScore <: AbstractScore end
abstract type PartitionScore <: AbstractScore end

function initialize_dist_scores(score_keys::Array{String, 1})
    """ Initializes a Dict of the form
            {
              score_key₁ : 0,
                ...
              score_keyᵥ : 0
            }
        for each key in `score_keys`
    """
    dist_scores = Dict{String, Int}()
    for key in score_keys
        dist_scores[key] = 0
    end
    return dist_scores
end

function get_district_scores(graph::BaseGraph,
                             partition::Partition,
                             dist::Int,
                             score_keys::Array{String, 1})
    """ Return a Dict of the form
        {
            score_key₁ : x₁
               ...
            score_keyᵥ : xᵥ
        }
        for district `dist`
    """
    dist_scores = initialize_dist_scores(score_keys)
    for node in partition.dist_nodes[dist]
        for key in score_keys
            dist_scores[key] += graph.attributes[node][key]
        end
    end
    return dist_scores
end

function update_scores!(scores::Dict{String, Any},
                        dist_scores::Dict{String, Int},
                        score_keys::Array{String, 1})
    """ Update the `scores` Dictionary with the district_scores in `dist_scores`.
        The `scores` dictionary holds scores for the entire partition, and the
        `dist_scores` dictionary only holds the scores for a single district.
    """
    for key in score_keys
        push!(scores[key], dist_scores[key])
    end
end

function initialize_scores(partition::Partition,
                           score_keys::Array{String, 1})
    """ Returns a Dict of the form
            {
              num_cut_edges  : partition.num_cut_edges
              score_key₁     : Array{Int, 1}(),
                 ...
              score_keyᵥ     : Array{Int, 1}()
            }
        for each key in score_keys
    """
    scores = Dict{String, Any}()
    # scores["num_cut_edges"] = partition.num_cut_edges
    for key in score_keys
        scores[key] = Array{Int, 1}()
    end
    return scores
end

# function initialize_score!(scores:: Dict{String, Any}, key::String)
#     scores[key] = Array{Int, 1}()
# end

# function initialize_score!(scores:: Dict{String, Any}, key::Tuple)
#     """ key should be of atleast length 2
#         arg1 will be the score name
#         arg2 will be th function
#         the rest of the args will be the arguments to the function
#
#         TODO: change the variable name for "key" to something more appropriate
#     """
#     key_name = key[1]
#     func = key[2]
#     args = key[3:length(key)]
#
#     scores[key_name] = func(args...)
# end
#

function update_scores!(scores::Dict{String, Any},
                        nodes::BitSet,
                        graph::BaseGraph,
                        score_keys::Array{String, 1},
                        dist::Int)
    """ Update the `scores` Dict with scores from  'score_keys'
        for district 'dist'. 'nodes' is the set of nodes in the district `dist`.
    """
    for node in nodes
        for key in score_keys
            scores[key][dist] += graph.attributes[node][key]
        end
    end
end

function initialize_scores(partition::Partition,
                           score_keys::Array{String, 1},
                           proposal::AbstractProposal)
    """ Returns a Dict of the form
        {
            "num_cut_edges" : partition.num_cut_edges,
            "dists"         : (D₁, D₂)
            score_key₁      : [0, 0],
              ...
            score_keyᵥ      : [0, 0]
        }
        for each key in score_keys
    """
    Δ_scores = Dict{String, Any}()
    # Δ_scores["num_cut_edges"] = partition.num_cut_edges
    Δ_scores["dists"] = (proposal.D₁, proposal.D₂)

    for score_key in score_keys
        Δ_scores[score_key] = [0, 0]
    end

    return Δ_scores
end


function get_detailed_scores(graph::BaseGraph,
                             partition::Partition,
                             score_keys::Array{String, 1})
    """ Return all scores collected for `partition`
        Eg. {
              "num_cut_edges" : partition.num_cut_edges,
              score_key₁  : [x₁, x₂, ..., xᵤ]
                 ...
              score_keyᵥ  : [y₁, y₂, ..., yᵤ]
            }
        where each value at index i of the arrays is the score at district i,
        and u is the total number of districts.
    """
    scores = initialize_scores(partition, score_keys)
    for dist in 1:graph.num_dists
        dist_scores = get_district_scores(graph, partition, dist, score_keys)
        update_scores!(scores, dist_scores, score_keys)
    end
    return scores
end

function get_Δ_scores(graph::BaseGraph,
                      partition::Partition,
                      score_keys::Array{String, 1},
                      proposal::AbstractProposal)
    """ Returns only the change in scores from the last partition after
        `proposal` was accepted.

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
    Δ_scores = initialize_scores(partition, score_keys, proposal)
    update_scores!(Δ_scores, proposal.D₁_nodes, graph, score_keys, 1)
    update_scores!(Δ_scores, proposal.D₂_nodes, graph, score_keys, 2)
    return Δ_scores
end

function get_scores(graph::BaseGraph,
                    partition::Partition,
                    dist_score_keys::Array{String, 1},
                    partition_score_keys::Array{Any, 1}, #TODO: fix this type check
                    # score_keys::Array{String, 1},
                    steps_taken::Int=1,
                    proposal::AbstractProposal=DummyProposal("Optional argument used when you want Δ metrics."))
    """ Return all the scores of `partition`.

        steps_taken : Number of steps in the chain
        score_keys  : Array of score names

        Important:
            The `proposal` is the step the chain just took. It is used to identify
            only the districts just modified, so only the change in information
            from those districts can be stored instead of all the scores.
            If steps_taken == 1, then it returns the detailed scores instead
            of just the Δ scores.
    """
    # first get the district scores
    if steps_taken == 1
        scores = get_detailed_scores(graph, partition, dist_score_keys)
    else
        scores = get_Δ_scores(graph, partition, dist_score_keys, proposal)
    end

    # then get the partition scores
    partition_scores = get_partition_scores(partition_score_keys)
    merge!(scores, partition_scores)

    return scores
end

function get_scores_at_step(all_scores::Array{},
                            dist_keys::Array{String, 1},
                            partition_keys::Array{Any, 1},
                            step::Int)
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

        for key in dist_keys
            scores[key][D₁] = curr_scores[key][1]
            scores[key][D₂] = curr_scores[key][2]
        end
    end

    # fill the rest with partition keys
    for key in partition_keys
        scores[key[1]] = all_scores[step][key[1]]
    end

    return scores
end

function get_partition_scores(partition_score_keys)
    scores = Dict{String, Any}()
    for key in partition_score_keys
        compute_partition_score!(scores, key)
    end
    return scores
end

function compute_partition_score!(scores:: Dict{String, Any}, key::Tuple)
    """ key should be of atleast length 2
        arg1 will be the score name
        arg2 will be th function
        the rest of the args will be the arguments to the function

        TODO: change the variable name for "key" to something more appropriate
    """
    key_name = key[1]
    func = key[2]
    args = key[3:length(key)]

    scores[key_name] = func(args...)
end

function save_scores(filename::String,
                     scores::Array{Dict{String, Any}, 1})
    """ Save the `scores` as `filename`.
        `filename` needs to be a .json.
    """
    open(filename, "w") do f
        JSON.print(f, scores)
    end
end

function seperate_score_keys(score_keys::Array{NamedTuple, 1})
    """ Seperate scores into `DistrictScores` and `PartitionScores`.
        TODO: This needs to be redone.
    """
    dist_scores = Array{String, 1}()
    partition_scores = Array{Any, 1}()
    for key in score_keys
        if key isa String push!(dist_scores, key) end
        if key isa Tuple push!(partition_scores, key) end
        # TODO: if wrong type then raise an error
    end
    return dist_scores, partition_scores
end
