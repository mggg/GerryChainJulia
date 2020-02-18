using Test
include("../src/graph.jl")

const testdir = dirname(@__FILE__)
filepath = "./test_grid_4x4.json"

graph_dict = JSON.parsefile(filepath)
graph = Graph(graph_dict)

tests = [
    "graph"
]

@testset "GerryChainJulia" begin
    for t in tests
        tp = joinpath(testdir, "$(t).jl")
        include(tp)
    end
end
