
# We generate a new Partition for each testset because the partition object is
# modified in each testset
@testset "Score tests" begin
    graph = BaseGraph(filepath, "population", "assignment")
    node_attrs = Array{NamedTuple, 1}()
    push!(node_attrs, (name="purples", key="purple"))
    push!(node_attrs, (name="pinks", key="pink"))

    @testset "get_scores() detailed" begin
        partition = Partition(filepath, graph, "population", "assignment")
        election = Election("TestElection", Array{AbstractString, 1}(["electionD", "electionR"]))
        update_elections!([election], graph, partition, DummyProposal(""), 1)

        dist_score_keys = Array{NamedTuple, 1}()
        push!(dist_score_keys, (name="dems", key=vote_counts_by_district, args=[election, "electionD"]))
        push!(dist_score_keys, (name="reps", key=vote_counts_by_district, args=[election, "electionR"]))
        plan_score_keys = Array{NamedTuple, 1}()
        push!(plan_score_keys, (name="num_cut_edges", key=num_cut_edges, args=[partition]))

        scores = get_scores(graph, partition, node_attrs, dist_score_keys, plan_score_keys, 1)
        @test scores["num_cut_edges"] == 8
        @test sort(scores["purples"]) == sort([28, 28, 13, 13])
        @test sort(scores["pinks"]) == sort([13, 13, 28, 28])
        @test sort(scores["dems"]) == sort([6, 6, 6, 6])
        @test sort(scores["reps"]) == sort([6, 6, 6, 6])
    end

    @testset "get_scores() Δ" begin
        partition = Partition(filepath, graph, "population", "assignment")
        election = Election("TestElection", Array{AbstractString, 1}(["electionD", "electionR"]))
        update_elections!([election], graph, partition, DummyProposal(""), 1)

        dist_score_keys = Array{NamedTuple, 1}()
        push!(dist_score_keys, (name="dems", key=vote_counts_by_district, args=[election, "electionD"]))
        push!(dist_score_keys, (name="reps", key=vote_counts_by_district, args=[election, "electionR"]))
        plan_score_keys = Array{NamedTuple, 1}()
        push!(plan_score_keys, (name="num_cut_edges", key=num_cut_edges, args=[partition]))

        # take a step, so we can check for Δ scores
        proposal = RecomProposal(1, 2, 51, 31, BitSet([1, 2, 3, 5, 6]), BitSet([4, 7, 8]))
        update_partition!(partition, graph, proposal)
        update_elections!([election], graph, partition, proposal, 2)

        scores = get_scores(graph, partition, node_attrs, dist_score_keys, plan_score_keys, 2, proposal)
        @test scores["num_cut_edges"] == 9
        @test sort(scores["purples"]) == sort([34, 22])
        @test sort(scores["pinks"]) == sort([17, 9])
        @test sort(scores["dems"]) == sort([8, 4])
        @test sort(scores["reps"]) == sort([6, 6])
    end

    @testset "get_scores_at_step()" begin
        partition = Partition(filepath, graph, "population", "assignment")
        election = Election("TestElection", Array{AbstractString, 1}(["electionD", "electionR"]))
        update_elections!([election], graph, partition, DummyProposal(""), 1)

        dist_score_keys = Array{NamedTuple, 1}()
        push!(dist_score_keys, (name="dems", key=vote_counts_by_district, args=[election, "electionD"]))
        push!(dist_score_keys, (name="reps", key=vote_counts_by_district, args=[election, "electionR"]))
        plan_score_keys = Array{NamedTuple, 1}()
        push!(plan_score_keys, (name="num_cut_edges", key=num_cut_edges, args=[partition]))

        # take a step, so we can check for Δ scores
        proposal = RecomProposal(1, 2, 51, 31, BitSet([1, 2, 3, 5, 6]), BitSet([4, 7, 8]))
        update_partition!(partition, graph, proposal)
        update_elections!([election], graph, partition, proposal, 2)

        all_scores = Array{Dict{String, Any}, 1}()
        scores = get_scores(graph, partition, node_attrs, dist_score_keys, plan_score_keys, 1)
        push!(all_scores, scores)

        proposal = RecomProposal(1, 2, 51, 31, BitSet([1, 2, 3, 5, 6]), BitSet([4, 7, 8]))
        update_partition!(partition, graph, proposal)
        Δ_scores = get_scores(graph, partition, node_attrs, dist_score_keys, plan_score_keys, 2, proposal)
        push!(all_scores, Δ_scores)

        # sneaky... simply calling detailed_scores on the new partition with step=1
        detailed_scores = get_scores(graph, partition, node_attrs, dist_score_keys, plan_score_keys, 1)
        parsed_scores = get_scores_at_step(all_scores, node_attrs, dist_score_keys, plan_score_keys, 2)
        
        @test keys(detailed_scores) == keys(parsed_scores)
        for key in keys(detailed_scores)
            @test detailed_scores[key] == parsed_scores[key]
        end
    end
end
