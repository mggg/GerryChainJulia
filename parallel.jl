using ArgParse
using Distributed
#using Distributed, ClusterManagers
#addprocs(SlurmManager(32), partition="batch", t="12:00:00", mem_per_cpu="2G")
@everywhere using HDF5
@everywhere using Random
@everywhere using DataStructures
@everywhere include("./src/GerryChain.jl")
@everywhere using .GerryChain

@everywhere struct JobPacket
    partition::Partition
    pop_constraint::PopulationConstraint
    M::Int
    steps::Int
end

@everywhere struct ResultPacket
    steps::Vector{Union{RecomProposal, String}}
end

@everywhere struct ScoresPacket
    partition::Partition
    proposal::Union{RecomProposal, Missing}
end

@everywhere const CHANNEL_SIZE = 64
const jobs = RemoteChannel(()->Channel{JobPacket}(CHANNEL_SIZE))
const results = RemoteChannel(()->Channel{ResultPacket}(CHANNEL_SIZE))
const accepted = RemoteChannel(()->Channel{ScoresPacket}(CHANNEL_SIZE))

@everywhere function weight_score(graph::BaseGraph, partition::Partition)::Int
    partition.weight
end


function parse_cli()
    s = ArgParseSettings()

    @add_arg_table s begin
        "--graph-json"
            required = true
        "--pop-col"
            default = "TOTPOP"
        "--pop-tolerance"
            arg_type = Float64
            default = 0.01
        "--assignment-col"
            required = true
        "--n-steps"
            required = true
            arg_type = Int
        "--M"
            required = true
            arg_type = Int
        "--out-file"
            required = true
        "--batch-size"
            default = 160
            arg_type = Int
        "--record-cols"
            nargs = '*'
    end

    return parse_args(s)
end


@everywhere function chain_worker(jobs::RemoteChannel,
                                  results::RemoteChannel,
                                  graph_path::AbstractString,
                                  pop_col::AbstractString)
    graph = BaseGraph(graph_path, pop_col)
    println("Chain worker: loaded graph.")
    while true
        packet = take!(jobs)
        steps = []
        for i in 1:packet.steps
            push!(steps, get_reversible_recom_proposal(
                graph,
                packet.partition,
                packet.pop_constraint,
                packet.M,
                Random.default_rng()
            ))
        end
        put!(results, ResultPacket(steps))
    end
end


@everywhere function scores_worker(accepted::RemoteChannel,
                                   scores::Array{AbstractScore, 1},
                                   out_file::AbstractString,
                                   graph_path::AbstractString,
                                   pop_col::AbstractString,
                                   num_steps::Int)
    graph = BaseGraph(graph_path, pop_col)
    println("Scores worker: loaded graph.")

    first_part = true
    idx = 1
    h5open(out_file, "w") do file
        scores_group = g_create(file, "/scores/diff")
        scores_datasets = Dict{String, Any}()
        while true
            packet = take!(accepted)
            if first_part
                first_part = false
                step_values = score_initial_partition(graph, packet.partition, scores)
                scores_init_group = g_create(file, "/scores/init")
                for (score, data) in step_values
                    if data isa AbstractDict
                        write(scores_init_group, score, [JSON.json(data)])
                        scores_datasets[score] = d_create(
                            scores_group,
                            score,
                            datatype(JSON.json(data)),
                            ((num_steps,), (-1,)),
                            "chunk",
                            (100,)
                        )
                    elseif data isa AbstractArray
                        # TODO: multidimensional arrays.
                        write(scores_init_group, score, data)
                        scores_datasets[score] = d_create(
                            scores_group,
                            score,
                            datatype(data),
                            ((num_steps, 2), (-1, 2)),
                            "chunk",
                            (100, 2)
                        )
                    else
                        write(scores_init_group, score, data)
                        scores_datasets[score] = d_create(
                            scores_group,
                            score,
                            datatype(data),
                            ((num_steps,), (-1,)),
                            "chunk",
                            (100,)
                        )
                    end
                end
                scores_datasets["dists"] = d_create(
                    scores_group,
                    "dists",
                    Int,
                    ((num_steps, 2), (-1, 2)),
                    "chunk",
                    (100, 2)
                )
            else
                step_values = score_partition_from_proposal(graph, packet.partition,
                                                            packet.proposal, scores)
                for (score, data) in step_values
                    if data isa AbstractDict
                        #scores_datasets[score][idx - 1] = JSON.json(data)
                    elseif data isa AbstractArray
                        scores_datasets[score][idx,  :] = data
                    else
                        scores_datasets[score][idx] = data
                    end
                end
                idx += 1
            end
        end
    end
end

function main(args)
    graph_path = args["graph-json"]
    pop_col = args["pop-col"]
    out_file = args["out-file"]
    num_steps = args["n-steps"]

    # Generate scores (For scores worker).
    record_cols = args["record-cols"]
    scores = Array{AbstractScore, 1}()
    push!(scores, num_cut_edges("cut_edges"))
    push!(scores, PlanScore("weights", weight_score))
    for col in record_cols
        push!(scores, DistrictAggregate(col))
    end

    # Start workers.
    for (idx, p) in enumerate(workers())
        if idx < nworkers()
            remote_do(chain_worker, p, jobs, results,
                      graph_path, pop_col)
        else
            remote_do(scores_worker, p, accepted, scores,
                      out_file, graph_path, pop_col, num_steps)
        end
    end

    graph = BaseGraph(graph_path, pop_col)
    partition = Partition(graph, args["assignment-col"])
    # TODO: check against initial constraints?
    put!(accepted, ScoresPacket(partition, missing))
    pop_constraint = PopulationConstraint(graph, partition, args["pop-tolerance"])
    batch_size = args["batch-size"]
    M = args["M"]

    steps_taken = 0
    self_loops = 0
    reasons = DefaultDict{String, Int}(0)
    while steps_taken < num_steps
        println(steps_taken)
        candidates = []
        for i in 1:nworkers()
            println("putting...")
            put!(jobs, JobPacket(partition, pop_constraint, M, batch_size))
        end
        for _ in 1:nworkers()
            println("took.")
            packet = take!(results)
            candidates = vcat(candidates, packet.steps)
        end

        candidates = shuffle(candidates)
        for cand in candidates
            if steps_taken > num_steps
                break
            end
            steps_taken += 1
            if cand isa RecomProposal
                # TODO: support for custom acceptance functions.
                update_partition!(partition, graph, cand)
                partition.weight = self_loops + 1
                partition.chain_meta = Dict("reasons" => reasons)
                put!(accepted, ScoresPacket(partition, cand))
                self_loops = 0
                reasons = DefaultDict{String, Int}(0)
                break
            else
                self_loops += 1
                reasons[cand] += 1
            end
        end
    end
end

main(parse_cli())

#remote_do(main, workers()[1], parse_cli())