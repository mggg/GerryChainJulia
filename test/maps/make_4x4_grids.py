def populate(graph):
    # calculatedly populate the graph
    for i in (0, 6, 8, 15):
        graph.nodes[i]["population"] = 20
    for i in (1, 2, 3, 5, 9, 10, 11, 13):
        graph.nodes[i]["population"] = 10
    for i in (4, 7, 12, 14):
        graph.nodes[i]["population"] = 1

def set_square_assignments(graph):
    # assignments
    for i in (0, 1, 4, 5):
        graph.nodes[i]["assignment"] = 1
    for i in (2, 3, 6, 7):
        graph.nodes[i]["assignment"] = 2
    for i in (8, 9, 12, 13):
        graph.nodes[i]["assignment"] = 3
    for i in (10, 11, 14, 15):
        graph.nodes[i]["assignment"] = 4

def set_col_assignments(graph):
    # assignments
    for i in (0, 4, 8, 12):
        graph.nodes[i]["assignment"] = 1
    for i in (1, 5, 9, 13):
        graph.nodes[i]["assignment"] = 2
    for i in (2, 6, 10, 14):
        graph.nodes[i]["assignment"] = 3
    for i in (3, 7, 11, 15):
        graph.nodes[i]["assignment"] = 4

def set_election_data(graph):
    for i in (0, 1, 2, 3, 8, 9, 10, 11):
        graph.nodes[i]["electionD"] = 2
        graph.nodes[i]["electionR"] = 0
    for j in (4, 5, 6, 7, 12, 13, 14, 15):
        graph.nodes[j]["electionD"] = 1
        graph.nodes[j]["electionR"] = 3

def set_race_populations(graph):
    purple_pops = [15, 6, 6, 6, 1, 6, 15, 1, 5, 4, 4, 4, 0, 4, 0, 5]
    pink_pops = [5, 4, 4, 4, 0, 4, 5, 0, 15, 6, 6, 6, 1, 6, 1, 15]

    for node, purple, pink in zip(graph.nodes, purple_pops, pink_pops):
        graph.nodes[node]["purple"] = purple
        graph.nodes[node]["pink"] = pink

# simple script to create a 4x4 graph to test the population constraint
import matplotlib.pyplot as plt
import networkx as nx
from gerrychain.graph import Graph

graph = nx.grid_graph([4,4])
graph = nx.convert_node_labels_to_integers(graph)
print(graph.nodes())
node_coords = list(graph.nodes())

populate(graph)
# set_col_assignments(graph)
set_square_assignments(graph)
set_election_data(graph)
set_race_populations(graph)

# we are going to use gerrychain's to_json method to convert the graph
g = Graph(graph)
# g.to_json("cols_grid_4x4.json")
g.to_json("test_grid_4x4.json")

# color by assignment
plt.figure()
nx.draw(
    graph,
    labels={x: x for x in node_coords},
    node_color=[graph.nodes[x]["assignment"] for x in graph.nodes()]
)
plt.show()
