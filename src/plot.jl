
function score_boxplot(chain_data::ChainScoreData,
                       score_key::Union{DistrictAggregte, DistrictScore},
                       sort_by_score=false)
    """
    """
    
    # graph
end


function score_boxplot(chain_data::ChainScoreData,
                       score_key::Union{PlanScore})
    """
    """
    index = findfirst(s -> s.name == score_name, chain_data.scores)
    if index == 0
        throw(ArgumentError("No score with requested name found."))
    end
    score_boxplot(chain_data.step_values, chain_data.scores[index])
end


function score_boxplot(chain_data::ChainScoreData, score_key::String)
    """
    """
    index = findfirst(s -> s.name == score_name, chain_data.scores)
    if index == 0
        throw(ArgumentError("No score with requested name found."))
    elseif chain_data.scores[index] isa CompositeScore
        throw(ArgumentError("Cannot make a boxplot of a CompositeScore."))
    end
    score_boxplot(chain_data.step_values, chain_data.scores[index])
end
