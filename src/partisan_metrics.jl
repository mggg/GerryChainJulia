
function mean_median(election::Election, party::AbstractString)
    """ Computes the mean-median score for `party` on in the `election`.
    """
    validate_party(party, election)
    vote_shares = vote_shares_by_district(election, party)

    return median(vote_shares) - mean(vote_shares)
end

function efficiency_gap(election::Election,
                        party::AbstractString)
    """ Computes the efficiency gap for `party` in `election`.
    """
    validate_party(party, election)
    if length(election.party) > 2
        throw(ArgumentError("Efficiency Gap only works with elections with 2 parties."))
    end

    party_idx  = findfirst(x -> x == party, election.party)
    other_idx = findfirst(x -> x != party, election.party)

    party_wasted_total = 0
    other_wasted_total = 0
    vote_counts = zip(election.vote_counts[:, party_idx], election.vote_counts[:, other_idx])

    for (party_votes, other_votes) in vote_counts
        party_wasted, other_wasted = wasted_votes(party_votes, other_votes)
        party_wasted_total += party_wasted
        other_wasted_total += other_wasted
    end

    total_votes = sum(election.vote_counts)
    return (party_wasted_total - other_wasted_total) / total_votes
end

function wasted_votes(party₁_votes::Int, party₂_votes::Int)
    """ Computes the number of votes "wasted" by each party. Wasted votes are
        votes that are either more than necessary than the party needed to win
        a seat or votes in a race that party lost.
    """
    total = party₁_votes + party₂_votes

    if party₁_votes > party₂_votes
        party₁_waste = party₁_votes - total / 2
        party₂_waste = party₂_votes
    else
        party₂_waste = party₂_votes - total / 2
        party₁_waste = party₁_votes
    end

    return party₁_waste, party₂_waste
end
