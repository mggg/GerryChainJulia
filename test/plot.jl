@testset "Plotting tests" begin
    graph = BaseGraph(square_grid_filepath, "population", "assignment")
    partition = Partition(graph, "assignment")
    # this is a dummy constraint
    pop_constraint = PopulationConstraint(graph, "population", 10.0)
    election = Election("election", ["electionD", "electionR"], graph.num_dists)
    scores = [
        DistrictAggregate("electionD"),
        DistrictAggregate("electionR"),
        ElectionTracker(election, [efficiency_gap("e_gap", election, "electionD")])
    ]
    num_steps = 10
    chain_data = recom_chain(graph, partition, pop_constraint, num_steps, scores)

    @testset "score_boxplot()" begin
        function boxplot_district_score()
            try
                score_boxplot(chain_data, "electionD")
            catch ex
                return ex
            end
        end

        function boxplot_plan_score()
            try
                score_boxplot(chain_data, "e_gap")
            catch ex
                return ex
            end
        end

        function boxplot_plan_score_with_comparison()
            try
                score_boxplot(chain_data, "e_gap", comparison_scores=[("a", 0)])
            catch ex
                return ex
            end
        end

        function boxplot_plan_score_with_custom_legend()
            try
                score_boxplot(
                    chain_data,
                    "e_gap",
                    comparison_scores=[("a", 0)],
                    bbox_to_anchor=(0.5, 0.5)
                )
            catch ex
                return ex
            end
        end

        @test !isa(boxplot_district_score(), Exception)
        @test !isa(boxplot_plan_score(), Exception)
        @test !isa(boxplot_plan_score_with_comparison(), Exception)
        @test !isa(boxplot_plan_score_with_custom_legend(), Exception)
    end

    @testset "score_histogram()" begin
        function histogram_no_comparison()
            try
                score_histogram(chain_data, "e_gap")
            catch ex
                return ex
            end
        end

        function histogram_with_comparison()
            try
                score_histogram(chain_data, "e_gap", comparison_scores=[("abc", 0.05)])
            catch ex
                return ex
            end
        end

        function histogram_with_custom_legend()
            try
                score_histogram(
                    chain_data,
                    "e_gap",
                    comparison_scores=[("abc", 0.05)],
                    bbox_to_anchor=(0.5, 0.5)
                )
            catch ex
                return ex
            end
        end

        @test !isa(histogram_no_comparison(), Exception)
        @test !isa(histogram_with_comparison(), Exception)
        @test !isa(histogram_with_custom_legend(), Exception)
    end
end
