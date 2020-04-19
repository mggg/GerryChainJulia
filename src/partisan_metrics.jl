
function efficiency_gap(election::Election,
                        party::AbstractString)
    """ Computes the efficiency gap for `party` in `election`.
    """
    wasted_votes₁_total = 0
    wasted_votes₂_total = 0

    for (party₁_votes, party₂_votes) in zip(election.party₁_votes, election.party₂_votes)
        wasted_votes₁, wasted_votes₂ = wasted_votes(party₁_votes, party₂_votes)
        wasted_votes₁_total += wasted_votes₁
        wasted_votes₂_total += wasted_votes₂
    end

    total_votes = sum(election.party₁_votes) + sum(election.party₂_votes)

    if party == election.party₁
        return (wasted_votes₁_total - wasted_votes₂_total) / total_votes
    elseif party == election.party₂
        return (wasted_votes₂_total - wasted_votes₁_total) / total_votes
    else
        throw(ArgumentError(string("Party ", party, " not contesting in election.")))
    end
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
