
# We generate a new Partition for each testset because the partition object is
# modified in each testset
@testset "Measure tests" begin
    graph = BaseGraph(filepath, "population", "assignment")

    elections = [Election("test_election", "Dem", "Rep", "electionD", "electionR")]

    racial_pops = [RacePopulation("Purple", "purple"),
                   RacePopulation("Pink", "pink")]

    @testset "get_measures() detailed" begin
        partition = Partition(filepath, graph, "population", "assignment")
        measures = get_measures(graph, partition, elections, racial_pops, 1)
        @test measures["num_cut_edges"] == 8
        @test sort(measures["Purple"]) == sort([28, 28, 13, 13])
        @test sort(measures["Pink"]) == sort([13, 13, 28, 28])
        @test sort(measures["test_election_Dem"]) == sort([6, 6, 6, 6])
        @test sort(measures["test_election_Rep"]) == sort([6, 6, 6, 6])
    end

    @testset "get_measures() Δ" begin
        partition = Partition(filepath, graph, "population", "assignment")
        proposal = RecomProposal(1,2, 51, 31, BitSet([1, 2, 3, 5, 6]), BitSet([4, 7, 8]))
        update_partition!(partition, graph, proposal)
        measures = get_measures(graph, partition, elections, racial_pops, 2, proposal)
        @test measures["num_cut_edges"] == 9
        @test sort(measures["Purple"]) == sort([34, 22])
        @test sort(measures["Pink"]) == sort([17, 9])
        @test sort(measures["test_election_Dem"]) == sort([8, 4])
        @test sort(measures["test_election_Rep"]) == sort([6, 6])
    end

    @testset "get_measures_at_step()" begin
        partition = Partition(filepath, graph, "population", "assignment")
        all_measures = Array{Dict{String, Any}, 1}()
        measures = get_measures(graph, partition, elections, racial_pops, 1)
        push!(all_measures, measures)

        proposal = RecomProposal(1,2, 51, 31, BitSet([1, 2, 3, 5, 6]), BitSet([4, 7, 8]))
        update_partition!(partition, graph, proposal)
        Δ_measures = get_measures(graph, partition, elections, racial_pops, 2, proposal)
        push!(all_measures, Δ_measures)
        detailed_measures = get_measures(graph, partition, elections, racial_pops, 1)

        parsed_measures = get_measures_at_step(all_measures, 2)
        @test keys(detailed_measures) == keys(parsed_measures)
        for key in keys(detailed_measures)
            @test detailed_measures[key] == parsed_measures[key]
        end
    end
end
