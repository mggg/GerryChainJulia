
# We generate a new Partition for each testset because the partition object is
# modified in each testset
@testset "Score tests" begin
    graph = BaseGraph(filepath, "population", "assignment")
    score_keys = ["electionD", "electionR", "purple", "pink"]

    @testset "get_scores() detailed" begin
        partition = Partition(filepath, graph, "population", "assignment")
        scores = get_scores(graph, partition, score_keys, 1)
        @test scores["num_cut_edges"] == 8
        @test sort(scores["purple"]) == sort([28, 28, 13, 13])
        @test sort(scores["pink"]) == sort([13, 13, 28, 28])
        @test sort(scores["electionD"]) == sort([6, 6, 6, 6])
        @test sort(scores["electionR"]) == sort([6, 6, 6, 6])
    end

    @testset "get_scores() Δ" begin
        partition = Partition(filepath, graph, "population", "assignment")
        proposal = RecomProposal(1,2, 51, 31, BitSet([1, 2, 3, 5, 6]), BitSet([4, 7, 8]))
        update_partition!(partition, graph, proposal)
        scores = get_scores(graph, partition, score_keys, 2, proposal)
        @test scores["num_cut_edges"] == 9
        @test sort(scores["purple"]) == sort([34, 22])
        @test sort(scores["pink"]) == sort([17, 9])
        @test sort(scores["electionD"]) == sort([8, 4])
        @test sort(scores["electionR"]) == sort([6, 6])
    end

    @testset "get_scores_at_step()" begin
        partition = Partition(filepath, graph, "population", "assignment")
        all_scores = Array{Dict{String, Any}, 1}()
        scores = get_scores(graph, partition, score_keys, 1)
        push!(all_scores, scores)

        proposal = RecomProposal(1,2, 51, 31, BitSet([1, 2, 3, 5, 6]), BitSet([4, 7, 8]))
        update_partition!(partition, graph, proposal)
        Δ_scores = get_scores(graph, partition, score_keys, 2, proposal)
        push!(all_scores, Δ_scores)
        detailed_scores = get_scores(graph, partition, score_keys, 1)

        parsed_scores = get_scores_at_step(all_scores, 2)
        @test keys(detailed_scores) == keys(parsed_scores)
        for key in keys(detailed_scores)
            @test detailed_scores[key] == parsed_scores[key]
        end
    end
end
