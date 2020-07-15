function score_boxplot(score_values::Array{S, 2};
                       sort_by_score::Bool=true,
                       label::String="GerryChain",
                       comparison_scores::Array{Tuple{String, Array{S, 1}},1}=[]) where {S<:Number}
    """ DistrictScore. Expects data with dimensions (# of steps, districts)
    """
    if sort_by_score
        # within every step of the chain (i.e., within each row),
        # sort districts by value of the score
        score_values = sort(score_values, dims=2)
        # sort columns by median value of score
        score_values = sortslices(score_values, dims=2, lt=(x,y) -> isless(median(x), median(y)))
    end
    plt.boxplot(score_values, showcaps=true, showbox=true, showfliers=false)
    if length(comparison_scores) > 0
        plt.plot([], [], color="k", marker="s", markerfacecolor="white", markersize=15, label=label)
        for p in comparison_scores
            plan_score_vals = sort_by_score ? sort(p[2]) : p[2]
            plt.scatter(1:length(p[2]), plan_score_vals, label=p[1])
        end
        plt.legend()
    end
end


function score_boxplot(score_values::Array{S, 1};
                       label::String="GerryChain",
                       comparison_scores::Array{Tuple{String, Array{S, 1}},1}=[]) where {S<:Number}
    """ PlanScore
    """
    # graph
    plt.boxplot(score_values, showcaps=true, showbox=true, showfliers=false)
    if length(comparison_scores) > 0
        plt.plot([], [], color="k", marker="s", markerfacecolor="white", markersize=15, label=label)
        for p in comparison_scores
            plt.scatter(1, p[2], label=p[1])
        end
        plt.legend()
    end
end


function score_boxplot(chain_data::ChainScoreData, score_name::String; kwargs...)
    """
    """
    index = findfirst(s -> s.name == score_name, chain_data.scores)
    if chain_data.scores[index] isa CompositeScore
        throw(ArgumentError("Cannot make a boxplot of a CompositeScore."))
    end
    score_boxplot(get_score_values(chain_data, score_name); kwargs...)
    plt.xlabel("Indexed districts")
    plt.ylabel(score_name)
end
