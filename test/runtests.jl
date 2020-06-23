using GerryChain
using Test

const testdir = dirname(@__FILE__)
square_grid_filepath = "./maps/test_grid_4x4.json"
cols_grid_filepath = "./maps/cols_grid_4x4.json"

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
