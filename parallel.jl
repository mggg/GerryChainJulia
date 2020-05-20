using Distributed
@everywhere using GerryChain

const wp = CachingPool(workers())
const N = nprocs()
@everywhere const graph = BaseGraph("../reversible-recom/data/PA_VTD.json",
                                    "TOT_POP", "2011_PLA_1")


@everywhere function attempt_proposal(partition::Partition,
                                      pop_constraint::PopulationConstraint)
    D₁, D₂, sg_edges, sg_nodes = sample_subgraph(graph, partition) 
    weights = rand(length(sg_edges))
    mst_edges = weighted_kruskal_mst(graph, sg_edges, collect(sg_nodes), weights)
    get_balanced_proposal(graph, mst_edges, sg_nodes, partition,
                          pop_constraint, D₁, D₂)

end

@everywhere function pmap_test(i::Int)
    println("[pmap test] got ", i)
end

function main()
    partition = Partition("../reversible-recom/data/PA_VTD.json",
                          graph,
                          "TOT_POP",
                          "2011_PLA_1")
    pop_constraint = PopulationConstraint(graph,
                                          "lol this isn't actually necessary",
                                          0.01)
    pmap(pmap_test, wp, 1:100)

    for i in 1:1000
        proposals = pmap(part->attempt_proposal(part, pop_constraint),
                         wp,
                         [partition for _ in 1:N])

        n_recom_proposals = 0
        updated = false
        for p in proposals
            if p isa RecomProposal
                n_recom_proposals += 1
                if !updated
                    update_partition!(partition, graph, p)
                end
            end
        end
        println("[batch ", i, "] ", n_recom_proposals, " ReCom proposals, ",
                N - n_recom_proposals, " dummy proposals")
    end
end

main()
