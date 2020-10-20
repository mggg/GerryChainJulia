@testset "satisfies_acceptance_fn" begin
    graph = BaseGraph(square_grid_filepath, "population")
    partition = Partition(graph, "assignment")

    inval_acc_fn_1(p) = "hi"
    inval_acc_fn_2(p) = -3
    inval_acc_fn_3(x,y) = x*y
    always_pass(p) = 1.0
    always_fail(p) = 0.0

    # partition doesn't have a "parent" defined yet, so the call to
    # satisfies_acceptance_fn should fail here. As a reminder, all partitions
    # passed to an acceptance function should have the parent field assigned.
    @test_throws AssertionError satisfies_acceptance_fn(partition, always_accept)

    partition.parent = deepcopy(partition)
    @test_throws ArgumentError satisfies_acceptance_fn(partition, inval_acc_fn_1)
    @test_throws ArgumentError satisfies_acceptance_fn(partition, inval_acc_fn_2)
    @test_throws MethodError satisfies_acceptance_fn(partition, inval_acc_fn_3)
    @test satisfies_acceptance_fn(partition, always_pass) == true
    @test satisfies_acceptance_fn(partition, always_fail) == false
end
