using GerryChain
import LibGEOS
import LibSpatialIndex
using Test
using LightGraphs
using JSON
using HDF5
using Logging

const testdir = dirname(@__FILE__)
square_grid_filepath = "./maps/test_grid_4x4.json"
cols_grid_filepath = "./maps/cols_grid_4x4.json"
square_shp_filepath = "./maps/simple_squares.shp"

tests = [
    "graph",
    "partition",
    "balance_edges",
    "geo",
    "constraints",
    "flip",
    "recom",
    "scores",
    "accept",
    "election",
    "plot"
]

@testset "GerryChainJulia" begin
    for t in tests
        tp = joinpath(testdir, "$(t).jl")
        include(tp)
    end
end
