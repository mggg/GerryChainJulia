
# TODO: Perhaps these elements should not be subscripts because people might want to access them.
#       This problem could also be fixed if we allow for more than a 2 party race.
abstract type AbstractElection end

struct Election <: AbstractElection
    name::AbstractString
    party₁::AbstractString
    party₂::AbstractString
    party₁_votes::Array{Int, 1}
    party₂_votes::Array{Int, 1}
    party₁_vote_shares::Array{Float64, 1}
    party₂_vote_shares::Array{Float64, 1}
end

function Election(name::AbstractString,
                  party₁::AbstractString,
                  party₂::AbstractString)::Election
    return Election(name,
                    party₁,
                    party₂,
                    Array{Int, 1}(),
                    Array{Int, 1}(),
                    Array{Float64, 1}(),
                    Array{Float64, 1}()
                    )
end

function update_elections!(elections::Array{Election, 1},
                           graph::BaseGraph,
                           partition::Partition,
                           proposal::AbstractProposal,
                           steps_taken::Int)
    """ Updates each Election in `elections` with the step in `proposal`.
        If steps_taken == 1, then modifies all the party vote counts and
        party vote shares in the Elections.

        Otherwise, will only update the party vote counts and party vote shares
        of the districts changed by the `proposal`.
    """
    if steps_taken < 1
        throw(ArgumentError("steps_taken should be atleast 1, received "
                             + string(steps_taken)))
    end

    for election in elections
        if steps_taken == 1
            update_election_detailed!(election, graph, partition)
        else
            update_election_Δ!(election, graph, partition, proposal)
        end
    end
end

function update_election_detailed!(election::Election,
                                   graph::BaseGraph,
                                   partition::Partition)
    """ Computes all the values for each Election in `elections` for the
        districts in `partition`.
    """
    for dist in 1:graph.num_dists
        party₁_votes = 0
        party₂_votes = 0
        for node in partition.dist_nodes[dist]
            party₁_votes += graph.attributes[node][election.party₁]
            party₂_votes += graph.attributes[node][election.party₂]
        end
        push!(election.party₁_votes, party₁_votes)
        push!(election.party₂_votes, party₂_votes)

        total_votes = party₁_votes + party₂_votes

        push!(election.party₁_vote_shares, party₁_votes / total_votes)
        push!(election.party₂_vote_shares, party₂_votes / total_votes)
    end
end

function update_election_Δ!(election::Election,
                            graph::BaseGraph,
                            partition::Partition,
                            proposal::AbstractProposal)
    """ Updates only the vote counts and vote shares of the districts changed
        in `proposal` for each Election in `elections`.

        The vote counts districts not modified by `proposal` are left untouched.
    """
    update_election_Δ_for_dist!(election, graph, partition, proposal.D₁)
    update_election_Δ_for_dist!(election, graph, partition, proposal.D₂)
end

function update_election_Δ_for_dist!(election::Election,
                                     graph::BaseGraph,
                                     partition::Partition,
                                     dist::Int)
    """ Updates the vote counts and vote shares of the district `dist` for
        each Election in `elections`.
    """
    party₁_votes = 0
    party₂_votes = 0

    for node in partition.dist_nodes[dist]
        party₁_votes += graph.attributes[node][election.party₁]
        party₂_votes += graph.attributes[node][election.party₂]
    end

    election.party₁_votes[dist] = party₁_votes
    election.party₂_votes[dist] = party₂_votes

    total_votes = party₁_votes + party₂_votes

    election.party₁_vote_shares[dist] = party₁_votes / total_votes
    election.party₂_vote_shares[dist] = party₂_votes / total_votes
end

function seats_won(election::Election,
                   party::AbstractString)::Int
    """ Returns the number of seats won by `party` in the `election`
    """
    wins = 0
    if party == election.party₁
        for i in 1:length(election.party₁_votes)
            if election.party₁_votes[i] > election.party₂_votes[i]
                wins += 1
            end
        end
    elseif party == election.party₂
        for i in 1:length(election.party₁_votes)
            if election.party₁_votes[i] < election.party₂_votes[i]
                wins += 1
            end
        end
    else
        throw(ArgumentError(string("Party ", party, " not contesting in election.")))
    end
    return wins
end

function total_vote_counts(election::Election,
                           party::AbstractString)::Int
    """ Returns the total vote counts of `party` in `election`.
    """
    if party == election.party₁
        return sum(election.party₁_votes)
    elseif party == election.party₂
        return sum(election.party₂_votes)
    else
        throw(ArgumentError(string("Party ", party,  " not contesting in election.")))
    end
end

function vote_counts_by_district(election::Election,
                                 party::AbstractString)::Array{Int, 1}
    """ Returns an array of vote counts by district for
        `party` in `election`.
    """
    if party == election.party₁
        return deepcopy(election.party₁_votes)
    elseif party == election.party₂
        return deepcopy(election.party₂_votes)
    else
        throw(ArgumentError(string("Party ", party, " not contesting in election.")))
    end
end

function vote_shares_by_district(election::Election,
                                 party::AbstractString)::Array{Float64, 1}
    """ Returns an array of vote shares by district for
        `party` in `election`.
    """
    if party == election.party₁
        return deepcopy(election.party₁_vote_shares)
    elseif party == election.party₂
        return deepcopy(election.party₂_vote_shares)
    else
        throw(ArgumentError(string("Party ", party, " not contesting in election.")))
    end
end
