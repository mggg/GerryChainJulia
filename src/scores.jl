
function initialize_dist_scores(score_keys::Array{NamedTuple, 1})
    """ Initializes a Dict of the form
            {
              score_key.name₁ : 0,
                ...
              score_key.nameᵥ : 0
            }
        for each key in `score_keys`
    """
    dist_scores = Dict{String, Int}()
    foreach(key -> dist_scores[key.name] = 0, score_keys)
    return dist_scores
end

function fill_node_scores!(scores::Dict{String, Int},
                           graph::BaseGraph,
                           dist_nodes::BitSet,
                           node_attrs::Array{NamedTuple, 1})
    """ Computes a sum of all node attributes `node_attrs` for the nodes in
        the set `dist_nodes` and adds them to the counts in the scores dict
        `scores`.

        The list `node_attrs` only contains tuples whose keys are present in
        each node eg. (name="total_pop", key="TOT_POP")
    """
    for node in dist_nodes
        for key in node_attrs
            scores[key.name] += graph.attributes[node][key.key]
        end
    end
end

function fill_node_scores!(scores::Dict{String, Any},
                           nodes::BitSet,
                           graph::BaseGraph,
                           node_attrs::Array{NamedTuple, 1},
                           dist_idx::Int)
    """ Computes a sum of all node attributes `node_attrs` for the nodes in
        the set `dist_nodes` and adds them to the counts in the `dist_idx`
        position of scores `scores` keys.

        The list `node_attrs` only contains tuples whose keys are present in
        each node eg. (name="total_pop", key="TOT_POP")

    """
    for node in nodes
        for key in node_attrs
            scores[key.name][dist_idx] += graph.attributes[node][key.key]
        end
    end
end

function get_scores_of_dist(graph::BaseGraph,
                            partition::Partition,
                            dist::Int,
                            node_attrs::Array{NamedTuple, 1})
    """ Return a Dict of the form
        {
            node_attr₁.name : x₁
               ...
            node_attrᵥ.name : xᵥ
        }
        for district `dist`.

        The list `node_attrs` only contains tuples whose keys are present in
        each node eg. (name="total_pop", key="TOT_POP").
    """
    scores = initialize_dist_scores(node_attrs)
    fill_node_scores!(scores, graph, partition.dist_nodes[dist], node_attrs)
    return scores
end

function initialize_scores(partition::Partition,
                           node_attrs::Array{NamedTuple, 1},
                           dist_score_keys::Array{NamedTuple, 1})
    """ Returns a Dict of the form
            {
              dist_score_key.name₁     : Array{Int, 1}(),
                 ...
              dist_score_key.nameᵥ     : Array{Int, 1}()
            }
        for each score key in dist_score_keys.
    """
    scores = Dict{String, Any}()
    foreach(key -> scores[key.name] = Array{Int, 1}(), node_attrs)
    foreach(key -> scores[key.name] = Array{Int, 1}(), dist_score_keys)
    return scores
end

function initialize_scores(partition::Partition,
                           node_attrs::Array{NamedTuple, 1},
                           dist_score_keys::Array{NamedTuple, 1},
                           proposal::AbstractProposal)
    """ Returns a Dict of the form
        {
            "dists"         : (proposal.D₁, proposal.D₂)
            score.name₁      : [0, 0],
              ...
            score.nameᵥ      : [0, 0]
        }
        for each key in {node_attrs} ∪ {dist_score_keys}.
    """
    Δ_scores = Dict{String, Any}()
    Δ_scores["dists"] = (proposal.D₁, proposal.D₂)
    foreach(key -> Δ_scores[key.name] = [0, 0], node_attrs)
    foreach(key -> Δ_scores[key.name] = [0, 0], dist_score_keys)
    return Δ_scores
end

function compute_function_score!(scores:: Dict{String, Any},
                                 key::NamedTuple)
    """ `key` is a tuple that is of the form (name=N, key=F, args=[,]).
    """
    func = key.key
    args = key.args
    scores[key.name] = func(args...)
end

function compute_function_score!(scores:: Dict{String, Any},
                                 key::NamedTuple,
                                 dist_idx::Int,
                                 dist::Int)
    """ For the Δ function.
        TODO: change the variable name for "key" to something more appropriate
    """
    func = key.key
    args = key.args
    push!(args, dist) # This is extremely ugly, and is only happening because the district functions are set up such that the Δ take a dist index at the end.
    scores[key.name][dist_idx] = func(args...)
    pop!(key.args)
end

function update_scores!(scores::Dict{String, Any},
                        dist_scores::Dict{String, Int})
    """ Appends the `scores` Dictionary with the district_scores in `dist_scores`.
        The `scores` dictionary holds scores for the entire partition, and the
        `dist_scores` dictionary only holds the scores for a single district.
    """
    foreach(key -> push!(scores[key], dist_scores[key]), keys(dist_scores))
end

function update_scores!(scores::Dict{String, Any},
                        nodes::BitSet,
                        graph::BaseGraph,
                        node_attrs::Array{NamedTuple, 1},
                        dist_score_keys::Array{NamedTuple, 1},
                        dist_idx::Int,
                        dist::Int)
    """ Used for Δ updates.
    """
    fill_node_scores!(scores, nodes, graph, node_attrs, dist_idx)
    foreach(score -> compute_function_score!(scores, score, dist_idx, dist), dist_score_keys)
end

function get_detailed_scores(graph::BaseGraph,
                             partition::Partition,
                             node_attrs::Array{NamedTuple, 1},
                             dist_score_keys::Array{NamedTuple, 1})
    """ Return all scores collected for `partition`
        Eg. {
              score_key₁  : [x₁, x₂, ..., xᵤ]
                 ...
              score_keyᵥ  : [y₁, y₂, ..., yᵤ]
            }
        where each value at index i of the arrays is the score at district i,
        and u is the total number of districts.
    """
    scores = initialize_scores(partition, node_attrs, dist_score_keys)
    for dist in 1:graph.num_dists
        dist_scores = get_scores_of_dist(graph, partition, dist, node_attrs)
        update_scores!(scores, dist_scores)
    end
    foreach(key -> compute_function_score!(scores, key), dist_score_keys)
    return scores
end

function get_Δ_scores(graph::BaseGraph,
                      partition::Partition,
                      node_attrs::Array{NamedTuple, 1},
                      dist_score_keys::Array{NamedTuple, 1},
                      proposal::AbstractProposal)
    """ Returns only the change in scores from the last partition after
        `proposal` was accepted.

        For example, suppose district 4's new White population is 43 and
        the new Sen2010_Dem population is 62, district 8's new White population
        is 22 and new Sen2010_Dem population is 66. The Δ scores would
        look like:
            {
                "dists"         : (5, 8),
                "White"         : [43, 22],
                "Sen2010_Dem"   : [62, 66],
            }

    """
    Δ_scores = initialize_scores(partition, node_attrs, dist_score_keys, proposal)
    update_scores!(Δ_scores, proposal.D₁_nodes, graph, node_attrs, dist_score_keys, 1, proposal.D₁)
    update_scores!(Δ_scores, proposal.D₂_nodes, graph, node_attrs, dist_score_keys, 2, proposal.D₂)
    return Δ_scores
end

function get_scores(graph::BaseGraph,
                    partition::Partition,
                    node_attrs::Array{NamedTuple, 1},
                    dist_score_keys::Array{NamedTuple, 1},
                    partition_score_keys::Array{NamedTuple, 1},
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
    # first get the node scores
    if steps_taken == 1
        scores = get_detailed_scores(graph, partition, node_attrs, dist_score_keys)
    else
        scores = get_Δ_scores(graph, partition, node_attrs, dist_score_keys, proposal)
    end

    # then get the partition scores
    partition_scores = get_partition_scores(partition_score_keys)
    merge!(scores, partition_scores)

    return scores
end

function get_scores_at_step(all_scores::Array{},
                            node_attrs::Array{NamedTuple, 1},
                            dist_score_keys::Array{NamedTuple, 1},
                            partition_score_keys::Array{NamedTuple, 1},
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

        for score in node_attrs
            scores[score.name][D₁] = curr_scores[score.name][1]
            scores[score.name][D₂] = curr_scores[score.name][2]
        end
        for score in dist_score_keys
            scores[score.name][D₁] = curr_scores[score.name][1]
            scores[score.name][D₂] = curr_scores[score.name][2]
        end
    end

    # fill the rest with partition keys
    for score in partition_score_keys
        scores[score.name] = all_scores[step][score.name]
    end

    return scores
end

function get_partition_scores(partition_score_keys)
    """
    """
    scores = Dict{String, Any}()
    foreach(key -> compute_function_score!(scores, key), partition_score_keys)
    return scores
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

function categorize_score_keys(scores::Array{NamedTuple, 1})
    """ Seperate scores into `DistrictScores` and `PartitionScores`.
        TODO: This needs to be redone.
    """
    validate_score_keys(scores)
    district_wide_scores = [vote_counts_by_district, vote_shares_by_district]

    node_attrs = Array{NamedTuple, 1}()
    dist_scores = Array{NamedTuple, 1}()
    partition_scores = Array{NamedTuple, 1}()

    for score in scores
        if score.key isa String
            push!(node_attrs, score)
        elseif score.key isa Function && score.key ∈ district_wide_scores
            push!(dist_scores, score)
        elseif score.key isa Function
            push!(partition_scores, score)
        end
    end
    return node_attrs, dist_scores, partition_scores
end

function validate_score_keys(scores::Array{NamedTuple, 1})
    """
    """
    for score in scores
        if !haskey(score, :name) || !haskey(score, :key)
            throw(ArgumentError(string("Score ", score, " requires a name and a key attribute.")))
        end
        if score.key isa Function && !haskey(score, :args)
            throw(ArgumentError(string("Score ", score.name, " has an function as a key but no args.")))
        end
        if haskey(score, :args) && !(score.args isa Array)
            throw(ArgumentError(string("Score ", score.name, " has an args field that needs to be of type Array.")))
        end
    end
end
