using GerryChain
using Test

const testdir = dirname(@__FILE__)
filepath = "./test_grid_4x4.json"

tests = [
    "graph",
    "partition",
    "constraints",
    "flip",
    "recom",
    "scores",
    "election",
    "partisan_metrics"
]

@testset "GerryChainJulia" begin
    for t in tests
        tp = joinpath(testdir, "$(t).jl")
        include(tp)
    end
end
