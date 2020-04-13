using GerryChain
using Test
using LightGraphs
using Random
using DataStructures

const testdir = dirname(@__FILE__)
filepath = "./test_grid_4x4.json"

tests = [
    "graph",
    "partition",
    "constraints",
    "recom",
    "scores"
]

@testset "GerryChainJulia" begin
    for t in tests
        tp = joinpath(testdir, "$(t).jl")
        include(tp)
    end
end
