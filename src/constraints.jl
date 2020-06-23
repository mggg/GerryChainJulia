abstract type AbstractConstraint end

struct PopulationConstraint <: AbstractConstraint
    min_pop::Int
    max_pop::Int
end


struct ContiguityConstraint <: AbstractConstraint
    # No metadata (for now); implements the `AbstractConstraint` interface.
end


function PopulationConstraint(graph::BaseGraph,
                              population_col::AbstractString,
                              tolerance::Float64)

    ideal_pop = graph.total_pop / graph.num_dists

    # no particular reason to not use floor() instead of ceil()
    min_pop = Int(ceil((1-tolerance) * ideal_pop))
    max_pop = Int(floor((1+tolerance) * ideal_pop))
    return PopulationConstraint(min_pop, max_pop)
end


function satisfy_constraint(constraint::PopulationConstraint,
                            proposal::RecomProposal)
    if proposal.D₁_pop >= constraint.min_pop && proposal.D₁_pop <= constraint.max_pop
        if proposal.D₂_pop >= constraint.min_pop && proposal.D₂_pop <= constraint.max_pop
            return true
        end
    end
    return false
end


function satisfy_constraint(constraint::PopulationConstraint,
                            D₁_pop::Int,
                            D₂_pop::Int)
    if D₁_pop >= constraint.min_pop && D₁_pop <= constraint.max_pop
        if D₂_pop >= constraint.min_pop && D₂_pop <= constraint.max_pop
            return true
        end
    end
    return false
end


function satisfy_constraint(constraint::ContiguityConstraint, graph::BaseGraph,
                            partition::Partition, flip::FlipProposal)::Bool
    neighbors = [n for n in graph.neighbors[flip.Node]
                 if partition.assignments[n] == flip.D₁]
    source_node = pop!(neighbors)

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
                if (!visited[neighbor] &&
                    partition.assignments[neighbor] == flip.D₁ &&
                    neighbor != flip.Node)
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
