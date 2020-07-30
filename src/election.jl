mutable struct Election
    name::String
    parties::Array{String, 1}
    vote_counts::Array{Int64, 2} # row: district, col: votes by party
    vote_shares::Array{Float64, 2} # row: district, col: vote share by party
end


function Election(name::String, parties::Array{String, 1}, num_districts::Int)
    """ Initializes an Election for a given number of parties and districts,
        initializing the vote counts & shares to zero.
    """
    vote_counts = zeros((num_districts, length(parties)))
    vote_shares = zeros((num_districts, length(parties)))
    return Election(name, parties, vote_counts, vote_shares)
end

function vote_updater(election::Election)::DistrictScore
    """ Returns a nameless DistrictScore function that updates the vote counts
        and shares of the passed `election`, which can then be used by other
        functions (such as seats_won, mean_median, etc.) This score function
        explicitly returns `nothing` and is meant only for internal use
        by the ElectionTracker object.
    """
    party_names = election.parties
    function score_fn(graph::BaseGraph, nodes::BitSet, district::Int)
        """ Updates the Election object to reflect the new election results
            in the district. Returns a NamedTuple where party name is matched to
            election results for that party in the specified district.
        """
        election.vote_counts[district, :] = zeros(length(election.parties))
        # update vote counts
        for node in nodes
            for i in 1:length(election.parties)
                party = election.parties[i]
                party_votes = graph.attributes[node][party]
                election.vote_counts[district, i] += party_votes
            end
        end
        # update vote shares
        vote_totals = sum(election.vote_counts[district, :])
        vote_shares = election.vote_counts[district, :] ./ vote_totals
        election.vote_shares[district, :] = vote_shares
        return nothing
    end
    return DistrictScore(score_fn)
end


function vote_count(name::String, election::Election, party::String)::DistrictScore
    """ Returns a DistrictScore that will return the number of votes won by
        the specified party.
    """
    function score_fn(graph::BaseGraph, nodes::BitSet, district::Int)
        """ Extracts the number of votes for the specified party in the
            specified district from the Election object.
        """
        party_index = findfirst(isequal(party), election.parties)
        return election.vote_counts[district, party_index]
    end
    return DistrictScore(name, score_fn)
end


function vote_share(name::String, election::Election, party::String)::DistrictScore
    """ Returns a DistrictScore that will return the percentage of votes won by
        the specified party.
    """
    function score_fn(graph::BaseGraph, nodes::BitSet, district::Int)
        """ Extracts the share of votes for the specified party in the
            specified district from the Election object.
        """
        party_index = findfirst(isequal(party), election.parties)
        return election.vote_shares[district, party_index]
    end
    return DistrictScore(name, score_fn)
end


function seats_won(name::String,
                   election::Election,
                   party::String)::PlanScore
    """ Returns a PlanScore with a custom scoring function specific to
        `election` that returns the number of seats won by a particular party
        across all districts in a given plan.
    """
    function score_fn(args...)
        """ Calculates the number of seats won by a particular party across
            all districts in a given plan. In the case of a tie, neither party
            is considered to have won the district. Note that while the function
            will be passed a graph and partition (as is required for a
            PlanScore), it only uses information from the Election object.

            In the case of a tie, no parties are considered winners.
        """
        party_index = findfirst(isequal(party), election.parties)
        # find the maximum vote count in each district
        max_vote_counts = findmax(election.vote_counts, dims=2)[1]
        # which parties achieved the maximum vote count in each district?
        achieved_max_votes = max_vote_counts .== election.vote_counts
        # if multiple parties achieved the maximum vote count in the same
        # district, then there was a tie. we eliminate ties from our vote
        # count data, as we count ties as having no winning district.
        one_winner_districts = sum(achieved_max_votes, dims=2)[:, 1] .== 1
        districts_won = achieved_max_votes[one_winner_districts, :]
        # if all districts were tied, then return 0, otherwise, return
        # the number of districts in which there were no ties and the
        # party achieved the maximum number of votes
        seats_won = length(districts_won) == 0 ? 0 : sum(districts_won[:, party_index])
        return seats_won
    end
    return PlanScore(name, score_fn)
end


function mean_median(name::String,
                     election::Election,
                     party::String)::PlanScore
     """ Returns a PlanScore with a custom scoring function specific to
         `election` that calculates the mean-median score
         of a particular plan for a particular party.
     """
    function score_fn(args...)
        """ Computes the mean-median score for `party` in `election`. Note that
            while the function will be passed a graph and partition (as is required
            for a PlanScore), it only uses information from the Election object.
        """
        party_index = findfirst(isequal(party), election.parties)
        vote_shares = election.vote_shares[:, party_index]
        score = median(vote_shares) - mean(vote_shares)
        return score
    end
    return PlanScore(name, score_fn)
end


function wasted_votes(party₁_votes::Int, party₂_votes::Int)
    """ Computes the number of votes "wasted" by each party. Wasted votes are
        votes that are either more than necessary than the party needed to win
        a seat or votes in a race that party lost. In a tie, all votes are
        considered to have been wasted.
    """
    total = party₁_votes + party₂_votes

    if party₁_votes > party₂_votes
        party₁_waste = party₁_votes - total / 2
        party₂_waste = party₂_votes
    elseif party₂_votes > party₁_votes
        party₂_waste = party₂_votes - total / 2
        party₁_waste = party₁_votes
    else party₁_votes == party₂_votes
        party₁_waste, party₂_waste = party₁_votes, party₂_votes
    end

    return party₁_waste, party₂_waste
end


function efficiency_gap(name::String,
                        election::Election,
                        party::String)::PlanScore
    """ Returns a PlanScore with a custom scoring function specific to
        `election` that calculates the efficiency gap of a particular plan for
        a particular party.
    """
    function score_fn(args...)
        """ Computes the efficiency gap for both parties in `election`. Note
            that while the function takes a graph and partition (as is required for
            a PlanScore), it only uses information from the Election object.
        """
        if length(election.parties) != 2
            throw(ArgumentError("Efficiency gap is only valid for elections with 2 parties."))
        end

        party_index = findfirst(isequal(party), election.parties)
        other_index = 3 - (party_index)
        p_wasted_total, o_wasted_total= 0, 0
        num_dists = size(election.vote_counts)[1]

        # iterate through all districts and count wasted votes
        for district in 1:num_dists
            p_votes = election.vote_counts[district, party_index]
            o_votes = election.vote_counts[district, other_index]
            p_wasted, o_wasted = wasted_votes(p_votes, o_votes)
            p_wasted_total += p_wasted
            o_wasted_total += o_wasted
        end

        total_votes = sum(election.vote_counts)
        p_gap = (p_wasted_total - o_wasted_total) / total_votes
        return p_gap
    end
    return PlanScore(name, score_fn)
end


function ElectionTracker(election::Election,
                         scores::Array{S, 1}=AbstractScore[])::CompositeScore where {S <: AbstractScore}
    """ The ElectionTracker method returns a CompositeScore that first updates
        the vote count / share for changed districts and then proceeds to
        run other scores (such as vote count for a particular party, partisan
        metrics, etc.), as desired by the user.
        Re-calculating vote counts only for changed districts means that the
        CompositeScore does not perform redundant computations for all of the
        partisan metrics. Furthermore, packaging all election-related scores
        within the CompositeScore ensures that the vote update occurs first,
        followed by the partisan metrics scoring functions.
    """
    count_votes = vote_updater(election)
    scores = Array{AbstractScore, 1}([count_votes; scores])
    return CompositeScore(election.name, scores)
end
