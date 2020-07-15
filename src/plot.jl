function score_boxplot(score_values::Array{S, 2};
                       sort_by_score::Bool=true,
                       label::String="GerryChain",
                       comparison_scores::Array{Tuple{String, Array{S, 1}},1}=[]) where {S<:Number}
    """ Produces a graph with multiple matplotlib box plots for the values of
        scores throughout the chain. Intended for use with district-level scores
        (DistrictAggregate, DistrictScore).

        Arguments:
            score_values        : A 2-dimensional array of score values with
                                  dimension (n x d), where n is the number of
                                  states in the chain and d is the number of
                                  districts
            sort_by_score       : Whether we should order districts on the
                                  by median of score value.
            label               : Legend key for the GerryChain boxplots. Only shown
                                  if there are scores from other plans passed in
                                  as reference points.
            comparison_scores   : A list of Tuples that is passed in if the user
                                  would like to compare the per-district scores
                                  of a particular plan with the GerryChain results
                                  on the same graph. The list of tuples should
                                  have the structure [(l₁, scores₁), ... , (lᵤ, scoresᵤ)],
                                  where lᵢ is a label that will appear on the
                                  legend and scoresᵢ is an array of length d,
                                  where d is the number of districts.

    """
    if sort_by_score
        # within every step of the chain (i.e., within each row),
        # sort districts by value of the score
        score_values = sort(score_values, dims=2)
        # sort columns by median value of score
        score_values = sortslices(score_values, dims=2, lt=(x,y) -> isless(median(x), median(y)))
    end
    # plot GerryChain boxplots
    plt.boxplot(score_values, showcaps=true, showbox=true, showfliers=false)
    plt.xlabel("Indexed districts")
    if length(comparison_scores) > 0
        # inserts a legend entry that shows the "GerryChain" label next to a
        # marker that looks like a boxplot
        plt.plot([], [], color="k", marker="s", markerfacecolor="white", markersize=15, label=label)
        # iterate through the comparison scores and plot them one by one
        for p in comparison_scores
            plan_score_vals = sort_by_score ? sort(p[2]) : p[2]
            plt.scatter(1:length(p[2]), plan_score_vals, label=p[1])
        end
        plt.legend()
    end
end


function score_boxplot(score_values::Array{S, 1};
                       label::String="GerryChain",
                       comparison_scores::Array{Tuple{String, S},1}=[]) where {S<:Number}
    """ Produces a single matplotlib box plot for the values of scores
        throughout the chain. Intended for use with plan-level scores.

        Arguments:
            score_values        : A 1-dimensional array of score values of
                                  length n, where n is the number of states in
                                  the chain.
            label               : Legend key for the GerryChain boxplots. Only shown
                                  if there are scores from other plans passed in
                                  as reference points.
            comparison_scores   : A list of Tuples that is passed in if the user
                                  would like to compare the score of a particular
                                  plan with the GerryChain boxplot on the same graph.
                                  The list of tuples should have the structure
                                  [(l₁, score₁), ... , (lᵤ, scoreᵤ)], where lᵢ
                                  is a label that will appear on the legend and
                                  scoreᵢ is the value of the plan-wide score
                                  for the comparison plan.
    """
    # plot GerryChain boxplot
    plt.boxplot(score_values, showcaps=true, showbox=true, showfliers=false)
    if length(comparison_scores) > 0
        # inserts a legend entry that shows the "GerryChain" label next to a
        # marker that looks like a boxplot
        plt.plot([], [], color="k", marker="s", markerfacecolor="white", markersize=15, label=label)
        # iterate through the comparison scores and plot them one by one
        for p in comparison_scores
            plt.scatter(1, p[2], label=p[1])
        end
        plt.legend()
    end
end


function score_boxplot(chain_data::ChainScoreData, score_name::String; kwargs...)
    """ Creates a graph with boxplot(s) of the values of scores throughout
        the chain.

        Arguments:
            chain_data  : ChainScoreData object that contains the values of
                          scores at every step of the chain
            score_name  : name of the score (i.e., the `name` field of an
                          AbstractScore)
            kwargs      : Optional arguments, including label, comparison_scores,
                          and sort_by_score (the latter should only be passed
                          for district-level scores).
    """
    index = findfirst(s -> s.name == score_name, chain_data.scores)
    if chain_data.scores[index] isa CompositeScore
        throw(ArgumentError("Cannot make a boxplot of a CompositeScore."))
    elseif index == 0
        throw(ArgumentError("Cannot find score in ChainScoreData object."))
    end
    score_boxplot(get_score_values(chain_data, score_name); kwargs...)
    plt.ylabel(score_name)
end
