def populate(graph):
    # calculatedly populate the graph
    for i in (0, 6, 8, 15):
        graph.nodes[i]["population"] = 20
    for i in (1, 2, 3, 5, 9, 10, 11, 13):
        graph.nodes[i]["population"] = 10
    for i in (4, 7, 12, 14):
        graph.nodes[i]["population"] = 1

def set_assignments(graph):
    # assignments
    for i in (0, 1, 4, 5):
        graph.nodes[i]["assignment"] = 1
    for i in (2, 3, 6, 7):
        graph.nodes[i]["assignment"] = 2
    for i in (8, 9, 12, 13):
        graph.nodes[i]["assignment"] = 3
    for i in (10, 11, 14, 15):
        graph.nodes[i]["assignment"] = 4

def set_election_data(graph):
    for i in (0, 1, 2, 3, 8, 9, 10, 11):
        graph.nodes[i]["electionD"] = 2
        graph.nodes[i]["electionR"] = 0
    for j in (4, 5, 6, 7, 12, 13, 14, 15):
        graph.nodes[j]["electionD"] = 1
        graph.nodes[j]["electionR"] = 3

def set_race_populations(graph):
    graph.nodes[0]["purple"] = 15
    graph.nodes[1]["purple"] = 6
    graph.nodes[2]["purple"] = 6
    graph.nodes[3]["purple"] = 6
    graph.nodes[4]["purple"] = 1
    graph.nodes[5]["purple"] = 6
    graph.nodes[6]["purple"] = 15
    graph.nodes[7]["purple"] = 1
    graph.nodes[8]["purple"] = 5
    graph.nodes[9]["purple"] = 4
    graph.nodes[10]["purple"] = 4
    graph.nodes[11]["purple"] = 4
    graph.nodes[12]["purple"] = 0
    graph.nodes[13]["purple"] = 4
    graph.nodes[14]["purple"] = 0
    graph.nodes[15]["purple"] = 5

    graph.nodes[0]["pink"] = 5
    graph.nodes[1]["pink"] = 4
    graph.nodes[2]["pink"] = 4
    graph.nodes[3]["pink"] = 4
    graph.nodes[4]["pink"] = 0
    graph.nodes[5]["pink"] = 4
    graph.nodes[6]["pink"] = 5
    graph.nodes[7]["pink"] = 0
    graph.nodes[8]["pink"] = 15
    graph.nodes[9]["pink"] = 6
    graph.nodes[10]["pink"] = 6
    graph.nodes[11]["pink"] = 6
    graph.nodes[12]["pink"] = 1
    graph.nodes[13]["pink"] = 6
    graph.nodes[14]["pink"] = 1
    graph.nodes[15]["pink"] = 15

# simple script to create a 4x4 graph to test the population constraint
import matplotlib.pyplot as plt
import networkx as nx
from gerrychain.graph import Graph

graph = nx.grid_graph([4,4])
graph = nx.convert_node_labels_to_integers(graph)
# node_coords = list(graph.nodes())

populate(graph)
set_assignments(graph)
set_election_data(graph)
set_race_populations(graph)

# we are going to use gerrychain's to_json method to convert the graph
g = Graph(graph)
g.to_json("test_grid_4x4.json")

# color by assignment
plt.figure()
nx.draw(
    graph,
    # pos={x: x for x in node_coords},
    node_color=[graph.nodes[x]["assignment"] for x in graph.nodes()]
)
plt.show()
