function mean_median(election::Election, party::AbstractString)
    """ Computes the mean-median score for `party` on in the `election`.
    """
    if party == election.party₁
        return median(election.party₁_vote_shares) - mean(election.party₁_vote_shares)
    elseif party == election.party₂
        return median(election.party₂_vote_shares) - mean(election.party₂_vote_shares)
    else
        throw(ArgumentError(string("Party ", party, " not contesting in election.")))
    end
end
