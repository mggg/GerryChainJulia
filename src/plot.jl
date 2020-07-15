function score_boxplot(score_values::Array{S, 2}, sort_by_score=true) where {S<:Number}
    """ DistrictScore. Expects data with dimensions (# of steps, districts)
    """
    if sort_by_score
        # sort columns of by the mean
        score_values = sortslices(score_values, dims=1, lt=(x,y) -> isless(mean(x), mean(y)))
    end
    plt.boxplot(score_values)
end

function score_boxplot(score_values::Array{S, 1}) where {S<:Number}
    """ PlanScore
    """
    # graph
    plt.boxplot(score_values)
end


function score_boxplot(chain_data::ChainScoreData, score_name::String)
    """
    """
    index = findfirst(s -> s.name == score_name, chain_data.scores)
    if chain_data.scores[index] isa CompositeScore
        throw(ArgumentError("Cannot make a boxplot of a CompositeScore."))
    end
    score_boxplot(get_score_values(chain_data, score_name))
    plt.xlabel("Indexed districts")
    plt.ylabel(score_name)
end
