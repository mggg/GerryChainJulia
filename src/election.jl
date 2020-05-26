
# TODO: Perhaps these elements should not be subscripts because people might want to access them.
#       This problem could also be fixed if we allow for more than a 2 party race.
abstract type AbstractElection end

struct Election <: AbstractElection
    name::AbstractString
    party::Array{AbstractString, 1}
    vote_counts::Array{Array{Int, 1}, 1}  # of length num_dists # TODO: remove
    vote_shares::Array{Array{Float64, 1}, 1}
end

function Election(name::AbstractString,
                  party_names::Array{AbstractString, 1})::Election

    # vote_counts = Array{Array{Int, 1}, 1}()
    # vote_shares = Array{Array{Float64, 1}, 1}()
    #
    # for party in party_names
    #     party_vote_counts = Array{Int, 1}()
    #     party_vote_shares = Array{Float64, 1}()
    #
    #     push!(vote_counts, party_vote_counts)
    #     push!(vote_shares, party_vote_shares)
    # end

    # return Election(name, party_names, vote_counts, vote_shares)
    return Election(name,
                    party_names,
                    Array{Array{Int, 1}, 1}(),
                    Array{Array{Float64, 1}, 1}())
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
    num_parties = length(election.party)

    for dist in 1:graph.num_dists

        vote_counts = zeros(Int64, num_parties)

        for node in partition.dist_nodes[dist]
            for i in 1:num_parties
                vote_counts[i] += graph.attributes[node][election.party[i]]
            end
        end

        total_votes = sum(vote_counts)
        vote_shares = map(x -> x / total_votes, vote_counts)

        push!(election.vote_counts, vote_counts)
        push!(election.vote_shares, vote_shares)
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
    num_parties = length(election.party)
    vote_counts = zeros(Int64, num_parties)

    for node in partition.dist_nodes[dist]
        for i in 1:num_parties
            vote_counts[i] += graph.attributes[node][election.party[i]]
        end
    end

    total_votes = sum(vote_counts)
    vote_shares = map(x -> x / total_votes, vote_counts)

    election.vote_counts[dist] = vote_counts
    election.vote_shares[dist] = vote_shares
end

function seats_won(election::Election,
                   party::AbstractString)::Int
    """ Returns the number of seats won by `party` in the `election`
    """
    wins = 0
    party_idx  = findall(x -> x == party, election.party)[1]

    for i in 1:length(election.vote_counts)
        if argmax(election.vote_counts[i]) == party_idx
            wins += 1
        end
    end
    # loop over each votecount
    # add 1 if the idmax is the party's index
    # if party == election.party₁
    #     for i in 1:length(election.party₁_votes)
    #         if election.party₁_votes[i] > election.party₂_votes[i]
    #             wins += 1
    #         end
    #     end
    # elseif party == election.party₂
    #     for i in 1:length(election.party₁_votes)
    #         if election.party₁_votes[i] < election.party₂_votes[i]
    #             wins += 1
    #         end
    #     end
    # else
    #     throw(ArgumentError(string("Party ", party, " not contesting in election.")))
    # end
    return wins
end

function total_vote_counts(election::Election,
                           party::AbstractString)::Int
    """ Returns the total vote counts of `party` in `election`.
    """
    party_idx  = findall(x -> x == party, election.party)[1]

    total = 0
    for i in 1:length(election.vote_counts)
        total += election.vote_counts[i][party_idx]
    end

    return total
    # else
    #     throw(ArgumentError(string("Party ", party,  " not contesting in election.")))
    # end
end

function vote_counts_by_district(election::Election,
                                 party::AbstractString)::Array{Int, 1}
    """ Returns an array of vote counts by district for
        `party` in `election`.
    """
    party_idx  = findall(x -> x == party, election.party)[1] # TODO: a cleaner way? this should always be unique but

    vote_counts = Array{Int64, 1}()
    # println(election)
    # println()
    # println(party_idx)
    # println()
    # println(election.vote_counts)
    for i in 1:length(election.vote_counts)
        push!(vote_counts, election.vote_counts[i][party_idx])
    end
    return vote_counts
    # if party == election.party₁
    #     return deepcopy(election.party₁_votes)
    # elseif party == election.party₂
    #     return deepcopy(election.party₂_votes)
    # else
    #     throw(ArgumentError(string("Party ", party, " not contesting in election.")))
    # end
end

function vote_shares_by_district(election::Election,
                                 party::AbstractString)::Array{Float64, 1}
    """ Returns an array of vote shares by district for
        `party` in `election`.
    """
    party_idx  = findall(x -> x == party, election.party)[1]

    vote_shares = Array{Float64, 1}()
    for i in 1:length(election.vote_shares)
        push!(vote_shares, election.vote_shares[i][party_idx])
    end
    return vote_shares
    # if party == election.party₁
    #     return deepcopy(election.party₁_vote_shares)
    # elseif party == election.party₂
    #     return deepcopy(election.party₂_vote_shares)
    # else
    #     throw(ArgumentError(string("Party ", party, " not contesting in election.")))
    # end
end
