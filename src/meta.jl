# TODO: force AbstractScore to have names
function short_bursts_recom(
    score::AbstractScore,
    burst_length::Int,
    num_bursts::Int,
    graph::BaseGraph,
    partition::Partition,
    pop_constraint::PopulationConstraint,
    acceptance_fn::F,
)::Tuple{Partition,Float64} where {F<:Function,S<:AbstractScore}
    best_partition = partition
    best_score = deepcopy(eval_score_on_partition(graph, partition, score))
    for _ = 1:num_bursts
        # for (partition, score_vals) in recom_chain_iter(graph, deepcopy(best_partition), pop_constraint, burst_length, [score], num_tries, acceptance_fn, rng, no_self_loops, progress_bar)
        for (partition, score_vals) in recom_chain_iter(
            graph,
            deepcopy(best_partition),
            pop_constraint,
            burst_length,
            [score],
            acceptance_fn = acceptance_fn,
        )
            if score_vals[score.name] >= best_score
                best_partition = deepcopy(partition)
                best_score = deepcopy(eval_score_on_partition(graph, partition, score))
            end
        end
    end
    return best_partition, best_score
end

function probabilistic_hill_climb(graph::BaseGraph, score::AbstractScore)
    function acceptance(partition::Partition)
        if isnothing(partition.parent)
            return 1.0
        end
        partition_score = eval_score_on_partition(graph, partition, score)

        parent_score = eval_score_on_partition(graph, partition, score)

        if partition_score > parent_score
            return 1.0
        end
        return (partition_score / parent_score)
    end
    return acceptance
end
