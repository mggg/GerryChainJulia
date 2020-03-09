abstract type AbstractConstraint end

struct PopulationConstraint <: AbstractConstraint
    min_pop::Int
    max_pop::Int
end

function PopulationConstraint(graph::BaseGraph,
                              population_col::AbstractString,
                              tolerance::Float64=0.01)

    ideal_pop = graph.total_pop / graph.num_dists

    # no particular reason to not use floor() instead of ceil()
    min_pop = Int(ceil((1-tolerance) * ideal_pop))
    max_pop = Int(floor((1+tolerance) * ideal_pop))
    return PopulationConstraint(min_pop, max_pop)
end

function satisfy_constraint(constraint::PopulationConstraint,
                            proposal::RecomProposal)
    if proposal.D₁_pop >= constraint.min_pop && proposal.D₁_pop <= constraint.max_pop
        if proposal.D₂_pop >= constraint.min_pop && proposal.D₂_pop <= constraint.max_pop
            return true
        end
    end
    return false
end

function satisfy_constraint(constraint::PopulationConstraint,
                            D₁_pop::Int,
                            D₂_pop::Int)
    if D₁_pop >= constraint.min_pop && D₁_pop <= constraint.max_pop
        if D₂_pop >= constraint.min_pop && D₂_pop <= constraint.max_pop
            return true
        end
    end
    return false
end
