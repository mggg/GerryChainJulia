abstract type AbstractProposal end

struct RecomProposal <: AbstractProposal
    D₁::Int
    D₂::Int
    D₁_pop::Int
    D₂_pop::Int
    D₁_nodes::BitSet
    D₂_nodes::BitSet
end

struct FlipProposal <: AbstractProposal
    node::Int # node that is being flipped
    D₁::Int # original district
    D₂::Int # new district
    D₁_pop::Int
    D₂_pop::Int
    D₁_nodes::BitSet
    D₂_nodes::BitSet
end

struct DummyProposal <: AbstractProposal
    reason::AbstractString
end
