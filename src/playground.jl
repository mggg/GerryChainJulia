# include("./GerryChain.jl")

using JSON
using SparseArrays
using LightGraphs
using Random
using DataStructures
using Profile
using ProfileView
using StatProfilerHTML

include("./graph.jl")
include("./partition.jl")
include("./proposals.jl")
include("./constraints.jl")
include("./measures.jl")
include("./recom.jl")


rng = MersenneTwister(1234)

function run_chain()
    println("Hello World")
    filepath = "./PA_VTD.json"
    graph = BaseGraph(filepath, "TOT_POP", "538GOP_PL")
    partition = Partition(filepath, graph, "TOT_POP", "538GOP_PL")

    # println(graph.attributes[1])
    elections = [Election("PRES12", "Dem", "Rep", "PRES12D", "PRES12R"),
                 Election("SEN10", "Dem", "Rep", "SEN10D", "SEN10R")]

    racial_pops = [RacePopulations("White", "WHITE_POP"),
                   RacePopulations("Black", "BLACK_POP")]

    num_steps = 100
    population_col = "TOT_POP"
    # measures_save_dir = "foo.json"
    pop_constraint = PopulationConstraint(graph, population_col, 0.02)

    println("starting the chain")
    recom_chain(graph, partition, pop_constraint, 10, elections, racial_pops)
    println("actually starting the chain")
    Profile.init(n = 10^7, delay = 0.001)
    # Profile.clear_malloc_data()
    # @profilehtml recom_chain(graph, partition, pop_constraint, num_steps)
    @time recom_chain(graph, partition, pop_constraint, num_steps, elections, racial_pops)
    # Profile.print()
end

run_chain()
