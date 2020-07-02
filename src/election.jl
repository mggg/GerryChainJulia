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

function count_votes(name::String, election::Election)::DistrictScore
    """ Returns a DistrictScore function that returns the vote count
        and share for all parties in a given district. Importantly, this
        DistrictScore function also has the side effect of updating the
        vote counts and shares of the passed `election`, which can then be
        used by other functions (such as seats_won, mean_median, etc.)
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
        total_votes = sum(election.vote_counts[district, :])
        vote_shares = election.vote_counts[district, :] ./ total_votes
        election.vote_shares[district, :] = vote_shares
        # create Dict matching party to vote count & share
        vote_pairs = Dict{String, NamedTuple}()
        for i in 1:length(party_names)
            party_votes = election.vote_counts[district, i]
            party_share = election.vote_shares[district, i]
            vote_pairs[party_names[i]] = (vote_count=party_votes, vote_share=party_share)
        end
        return vote_pairs
    end
    return DistrictScore(name, score_fn)
end


function seats_won(name::String,
                   election::Election,
                   party::String)::PlanScore
    """ Returns a PlanScore function that returns the number of seats won by
        a particular party across all districts in a given plan.
    """
    function score_fn(graph::BaseGraph, partition::Partition)
        """ Calculates the number of seats won by a particular party across
            all districts in a given plan. In the case of a tie, neither party
            is considered to have won the district. Note that while the function
            takes a graph and partition (as is required for a PlanScore), it
            only uses information from the Election object.

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
        no_tie_vote_counts = election.vote_counts[one_winner_districts, :]
        if length(no_tie_vote_counts) == 0 # all districts are tied, no winners
            return 0
        end
        most_votes = argmax(no_tie_vote_counts, dims=2)
        winning_parties = getindex.(most_votes, [2])
        seats_won = sum(party_index .== winning_parties)
        return seats_won
    end
    return PlanScore(name, score_fn)
end


function mean_median(name::String,
                     election::Election,
                     party::String)::PlanScore
     """ Returns a PlanScore function that calculates the mean-median score
         of a particular plan for a particular party.
     """
    function score_fn(graph::BaseGraph, partition::Partition)
        """ Computes the mean-median score for `party` in `election`.
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
    """ Returns a PlanScore function that calculates the efficiency gap.
        of a particular plan for a particular party.
    """
    function score_fn(graph::BaseGraph, partition::Partition)
        """ Computes the efficiency gap for both parties in `election`.
        """
        if length(election.parties) != 2
            throw(ArgumentError("Efficiency gap is only valid for elections with 2 parties."))
        end

        party_index = findfirst(isequal(party), election.parties)
        other_index = 3 - (party_index)
        p_wasted_total, o_wasted_total= 0, 0

        for district in 1:graph.num_dists
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
                         partisan_metrics::Array{S, 1}=AbstractScore[])::CompositeScore where {S <: AbstractScore}
    """ The ElectionTracker method returns a CompositeScore that first updates
        the vote count / share for changed districts and then proceeds to
        calculate other partisan metrics, as desired by the user.
        Re-calculating vote counts only for changed districts means that the
        CompositeScore does not perform redundant computations for all of the
        partisan metrics. Furthermore, packaging all partisan metrics within
        the CompositeScore ensures that the vote update occurs first, followed
        by the partisan metrics scoring functions.
    """
    vote_tracker = [count_votes("votes", election)]
    scores = Array{AbstractScore, 1}([vote_tracker; partisan_metrics])
    return CompositeScore(election.name, scores)
end
