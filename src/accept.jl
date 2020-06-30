function always_accept(partition::Partition)
    """ Accepts new partition with probability 1.
    """
    return 1
end


function satisfies_acceptance_fn(partition::Partition,
                                 acceptance_fn::Function)::Bool
    """ Determines whether a partition should be accepted, according to the
        user-specified acceptance function. Acceptance function must
        return a valid probability in [0, 1], and satisfies_acceptance_fn will
        use this probability to determine whether the partition should be
        accepted.

        Arguments:
            partition:      Partition. Should have a valid "parent" field
                            so the acceptance function can compare the new
                            partition to the previous partition, if necessary.
            acceptance_fn:  A user-specified function that should take
                            partition as an argument and return a probability
                            in the range [0, 1].
    """
    # check that partition has a valid parent
    @assert partition.parent != nothing
    prob = 0.0
    try
        prob = acceptance_fn(partition)
        if !(typeof(prob) <: Number) || prob < 0 || prob > 1
            error_msg = "Acceptance function must return value in [0, 1] range."
            throw(ArgumentError(error_msg))
        end
    catch e
        if isa(e, MethodError)
            error_msg = "Acceptance function must accept Partition as input."
            throw(MethodError(error_msg))
        else
            throw(e)
        end
    end
    return rand() <= prob
end
