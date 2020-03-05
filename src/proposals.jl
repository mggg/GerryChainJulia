abstract type AbstractProposal end

struct RecomProposal <: AbstractProposal
    D₁::Int
    D₂::Int
    D₁_pop::Int
    D₂_pop::Int
    D₁_nodes::Set{Int}
    D₂_nodes::Set{Int}
end
