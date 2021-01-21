abstract type AbstractConstraint end

struct PopulationConstraint <: AbstractConstraint
    min_pop::Int
    max_pop::Int
end

"""
    ContiguityConstraint()

Initializes and returns a `ContiguityConstraint` object.
"""
struct ContiguityConstraint <: AbstractConstraint
    # No metadata (for now); implements the `AbstractConstraint` interface.
end

"""
    PopulationConstraint(graph::BaseGraph,
                         partition::Partition,
                         tolerance::Float64)::PopulationConstraint

Initializes a `PopulationConstraint` that stores the minimum and maximum populations
a district in a `partition` could have within `tolerance`.

*Returns* the PopulationConstraint object.
"""
function PopulationConstraint(
    graph::BaseGraph,
    partition::Partition,
    tolerance::Float64,
)::PopulationConstraint
    ideal_pop = graph.total_pop / partition.num_dists

    # no particular reason to not use floor() instead of ceil()
    min_pop = Int(ceil((1 - tolerance) * ideal_pop))
    max_pop = Int(floor((1 + tolerance) * ideal_pop))
    return PopulationConstraint(min_pop, max_pop)
end

"""
    satisfy_constraint(constraint::PopulationConstraint,
                       proposal::RecomProposal)

Test whether a RecomProposal satisfies a population constraint.
"""
function satisfy_constraint(constraint::PopulationConstraint, proposal::RecomProposal)
    if proposal.D₁_pop >= constraint.min_pop && proposal.D₁_pop <= constraint.max_pop
        if proposal.D₂_pop >= constraint.min_pop && proposal.D₂_pop <= constraint.max_pop
            return true
        end
    end
    return false
end

"""
    satisfy_constraint(constraint::PopulationConstraint,
                       D₁_pop::Int,
                       D₂_pop::Int)

Test whether two population counts satisfy a PopulationConstraint.
"""
function satisfy_constraint(constraint::PopulationConstraint, D₁_pop::Int, D₂_pop::Int)
    if D₁_pop >= constraint.min_pop && D₁_pop <= constraint.max_pop
        if D₂_pop >= constraint.min_pop && D₂_pop <= constraint.max_pop
            return true
        end
    end
    return false
end

"""
    satisfy_constraint(constraint::ContiguityConstraint,
                       graph::BaseGraph,
                       partition::Partition,
                       flip::FlipProposal)

Test whether a FlipProposal satisfies the contiguity constraint.
Based on Parker's implementation on GitHub, located in the Flips.jl
repository at src/constraints.jl.
"""
function satisfy_constraint(
    constraint::ContiguityConstraint,
    graph::BaseGraph,
    partition::Partition,
    flip::FlipProposal,
)
    # get node's neighbors who were in its old district
    neighbors =
        [n for n in graph.neighbors[flip.node] if partition.assignments[n] == flip.D₁]
    if isempty(neighbors) # this is the only node of this district left!
        return false
    end
    source_node = pop!(neighbors)

    # DFS search to verify contiguity is not broken
    @inbounds for target_node in neighbors
        visited = zeros(Bool, graph.num_nodes)
        queue = Queue{Int}(64)  # TODO: auto-tune?
        enqueue!(queue, target_node)
        visited[target_node] = true
        found = false
        while !isempty(queue)
            curr_node = dequeue!(queue)
            if curr_node == source_node
                found = true
                break
            end
            for neighbor in graph.neighbors[curr_node]
                if (
                    !visited[neighbor] &&
                    partition.assignments[neighbor] == flip.D₁ &&
                    neighbor != flip.node
                )
                    visited[neighbor] = true
                    enqueue!(queue, neighbor)
                end
            end
        end
        if (isempty(queue) && !found)
            return false
        end
    end
    return true
end
