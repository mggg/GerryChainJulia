@testset "Recom tests" begin
    graph = BaseGraph(square_grid_filepath, "population")

    @testset "traverse_mst()" begin
        nodes = [1, 2, 3, 4, 5, 6, 7, 8]
        edges = [graph.adj_matrix[1,5], graph.adj_matrix[5,6], graph.adj_matrix[2,6],
                 graph.adj_matrix[2,3], graph.adj_matrix[3,7], graph.adj_matrix[3,4],
                 graph.adj_matrix[4,8]]
        mst = GerryChain.build_mst(graph, BitSet(nodes), BitSet(edges))
        stack = Stack{Int}()
        component_container = BitSet([])
        cut_edge = graph.adj_matrix[2,3]
        component = GerryChain.traverse_mst(mst, 2, 3, stack, component_container)
        @test component == BitSet([1, 2, 5, 6])
        component = GerryChain.traverse_mst(mst, 1, 5, stack, component_container)
        @test component == BitSet([1])
    end

    @testset "recom_chain()" begin
        partition = Partition(graph, "assignment")
        # this is a dummy constraint
        pop_constraint = PopulationConstraint(graph, partition, "population", 10.0)
        scores = [
            DistrictAggregate("electionD"),
            DistrictAggregate("electionR"),
            DistrictAggregate("purple"),
            DistrictAggregate("pink"),
        ]
        num_steps = 2 # test 2 steps for now

        function run_chain()
            try
                recom_chain(graph, partition, pop_constraint, num_steps, scores)
            catch ex
                return ex
            end
        end
        recom_chain(graph, partition, pop_constraint, num_steps, scores)
        # hacky way to run flip chain and test that it doesn't yield an exception
        @test !isa(run_chain(), Exception)
    end
end
