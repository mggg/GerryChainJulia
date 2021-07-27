using GerryChain
using Test
using BenchmarkTools

SHAPEFILE_PATH = "./maps/PA_VTDs.json"
POPULATION_COL = "TOTPOP"
ASSIGNMENT_COL = "CD_2011"

# Initialize graph and partition
graph = BaseGraph(SHAPEFILE_PATH, POPULATION_COL)
partition = Partition(graph, ASSIGNMENT_COL)

# Define parameters of chain (number of steps and population constraint)
pop_constraint = PopulationConstraint(graph, partition, 0.02)
num_steps = 1000

# Run the chain
results = @benchmark recom_chain(graph, partition, pop_constraint, num_steps, [DistrictAggregate("presd", "PRES12D"),], progress_bar=false) seconds=600
display(results)
@test true
