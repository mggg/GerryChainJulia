function score_boxplot(score_values::Array{S, 2};
                       sort_by_score::Bool=true,
                       label::String="GerryChain",
                       comparison_scores::Array=[],
                       ax::Union{Nothing, PyPlot.PyObject}=nothing) where {S<:Number}
    """ Produces a graph with multiple matplotlib box plots for the values of
        scores throughout the chain. Intended for use with district-level scores
        (DistrictAggregate, DistrictScore).

        Arguments:
            score_values        : A 2-dimensional array of score values with
                                  dimension (n x d), where n is the number of
                                  states in the chain and d is the number of
                                  districts
            sort_by_score       : Whether we should order districts by median
                                  of score value.
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
                                  where d is the number of districts. Each
                                  element of the tuple should be of type
                                  Tuple{String, Array{S, 1}}. Example:
                                  [
                                    (name₁, [v₁, v₂, ... , vᵤ]),
                                    ...
                                    (nameₓ, [w₁, w₂, ... , wᵤ])
                                  ], where there are x comparison plans and u
                                  districts.
            ax                  : A PyPlot (matplotlib) Axis object

    """
    if isnothing(ax)
        _, ax = plt.subplots()
    end
    if sort_by_score
        # within every step of the chain (i.e., within each row),
        # sort districts by value of the score
        score_values = sort(score_values, dims=2)
        # sort columns by median value of score
        score_values = sortslices(score_values, dims=2, lt=(x,y) -> isless(median(x), median(y)))
    end
    # plot GerryChain boxplots
    medianprops=Dict("color" => "black") # make sure median line is black
    ax.boxplot(score_values, showcaps=true, showbox=true, showfliers=false, medianprops=medianprops)
    ax.set_xlabel("Indexed districts")
    if length(comparison_scores) > 0
        # inserts a legend entry that shows the "GerryChain" label next to a
        # marker that looks like a boxplot
        ax.plot([], [], color="k", marker="s", markerfacecolor="white", markersize=15, label=label)
        # iterate through the comparison scores and plot them one by one
        for p in comparison_scores
            if !(p isa Tuple) || length(p) != 2 || !(p[1] isa String) || !(typeof(p[2]) <: AbstractArray)
                throw(ArgumentError("Scores of comparison plans must be passed as tuples with structure (name of plan, [scores for each district])."))
            end
            plan_score_vals = sort_by_score ? sort(p[2]) : p[2]
            ax.scatter(1:length(p[2]), plan_score_vals, label=p[1])
        end
        ax.legend()
    end
    return ax
end


function score_boxplot(score_values::Array{S, 1};
                       label::String="GerryChain",
                       comparison_scores::Array=[],
                       ax::Union{Nothing, PyPlot.PyObject}=nothing) where {S<:Number}
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
            ax                  : A PyPlot (matplotlib) Axis object
    """
    if isnothing(ax)
        _, ax = plt.subplots()
    end
    medianprops=Dict("color" => "black") # make sure median line is black
    ax.boxplot(score_values, showcaps=true, showbox=true, showfliers=false, medianprops=medianprops)
    if length(comparison_scores) > 0
        # inserts a legend entry that shows the "GerryChain" label next to a
        # marker that looks like a boxplot
        ax.plot([], [], color="k", marker="s", markerfacecolor="white", markersize=15, label=label)
        # iterate through the comparison scores and plot them one by one
        for p in comparison_scores
            if !(p isa Tuple) || length(p) != 2 || !(p[1] isa String) || !(typeof(p[2]) <: Number)
                throw(ArgumentError("Scores of comparison plans must be passed as tuples with structure (name of plan, score of plan)."))
            end
            ax.scatter(1, p[2], label=p[1])
        end
        ax.legend()
    end
    return ax
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
    score, nested_key = get_score_by_name(chain_data, score_name)
    if score isa CompositeScore
        throw(ArgumentError("Cannot make a boxplot of a CompositeScore"))
    end
    score_vals = get_score_values(chain_data.step_values, score, nested_key=nested_key)
    ax = score_boxplot(score_vals; kwargs...)
    ax.set_ylabel(score_name)
    return ax
end


function score_histogram(score_values::Array{S, 1};
                         comparison_scores::Array=[],
                         bins::Union{Nothing, Int, Vector}=nothing,
                         range::Union{Nothing, Tuple}=nothing,
                         density::Bool=false,
                         rwidth::Union{Nothing, T}=nothing,
                         ax::Union{Nothing, PyPlot.PyObject}=nothing) where {S<:Number, T<:Number}
    """ Creates a graph with histogram of the values of a score throughout
        the chain. Only applicable for scores of type PlanScore.

        Arguments:
            score_values        : A 1-dimensional array of score values of
                                  length n, where n is the number of states in
                                  the chain.
            comparison_scores   : A list of Tuples that is passed in if the user
                                  would like to compare core of a particular
                                  plan with the GerryChain histogram on the same
                                  figure. The list of tuples should have the
                                  structure [(l₁, score₁), ... , (lᵤ, scoreᵤ)],
                                  where lᵢ is a label that will appear on the
                                  legend and scoreᵢ is the value of the plan-wide
                                  score for the comparison plan.
            ax                  : A PyPlot (matplotlib) Axis object
    """
    # plot GerryChain histogram
    if isnothing(ax)
        _, ax = plt.subplots()
    end
    ax.hist(score_values, bins=bins, range=range, density=density, rwidth=rwidth)
    if length(comparison_scores) > 0
        # cycle through colors so vertical lines do not appear all blue
        colors = plt.rcParams["axes.prop_cycle"].by_key()["color"]
        color_index = 1
        for p in comparison_scores
            color = colors[color_index % length(colors) + 1] # ensure that we don't go out of bounds
            ax.axvline(p[2], color=color, label=p[1])
            color_index += 1
        end
        ax.legend()
    end
    return ax
end


function score_histogram(chain_data::ChainScoreData, score_name::String; kwargs...)
    """ Creates a graph with histogram of the values of a score throughout
        the chain. Only applicable for scores of type PlanScore.

        Arguments:
            chain_data  : ChainScoreData object that contains the values of
                          scores at every step of the chain
            score_name  : name of the score (i.e., the `name` field of an
                          AbstractScore)
            kwargs      : Optional arguments, including comparison_scores
                          and other matplotlib arguments.
    """
    score, nested_key = get_score_by_name(chain_data, score_name)
    # throw argument error if score passed was not a PlanScore
    if !(score isa PlanScore)
        throw(ArgumentError("Can only create histogram plot of a PlanScore"))
    end
    score_vals = get_score_values(chain_data.step_values, score, nested_key=nested_key)
    ax = score_histogram(score_vals; kwargs...)
    ax.set_ylabel("Frequency")
    ax.set_xlabel(score_name)
    return ax
end
