
abstract type AbstractElection end

mutable struct Election <: AbstractElection
    name::AbstractString
    party::Array{AbstractString, 1}
    vote_counts::Array{Int64}
    vote_shares::Array{Float64}
end


function Election(name::AbstractString,
                  party_names::Array{AbstractString, 1})::Election
    return Election(name, party_names, Array{Int64}[], Array{Float64}[])
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
            update_election!(election, graph, partition)
        else
            update_election!(election, graph, partition, proposal)
        end
    end
end


function update_election!(election::Election,
                          graph::BaseGraph,
                          partition::Partition)
    """ Computes all the values for each Election in `elections` for the
        districts in `partition`.
    """
    num_parties = length(election.party)
    election.vote_counts = zeros(Int64, graph.num_dists, num_parties)
    election.vote_shares = zeros(Float64, graph.num_dists, num_parties)

    for dist in 1:graph.num_dists
        for node in partition.dist_nodes[dist]
            for i in 1:num_parties
                election.vote_counts[dist, i] += graph.attributes[node][election.party[i]]
            end
        end

        total_votes = sum(election.vote_counts[dist, :])
        election.vote_shares[dist, :] = map(x -> x / total_votes, election.vote_counts[dist,:])
    end

end


function update_election!(election::Election,
                          graph::BaseGraph,
                          partition::Partition,
                          proposal::AbstractProposal)
    """ Updates only the vote counts and vote shares of the districts changed
        in `proposal` for each Election in `elections`.

        The vote counts districts not modified by `proposal` are left untouched.
    """
    update_election_for_dist!(election, graph, partition, proposal.D₁)
    update_election_for_dist!(election, graph, partition, proposal.D₂)
end


function update_election_for_dist!(election::Election,
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
    election.vote_counts[dist, :] = vote_counts
    election.vote_shares[dist, :] = map(x -> x / total_votes, vote_counts)
end


function seats_won(election::Election,
                   party::AbstractString)::Int
    """ Returns the number of seats won by `party` in the `election`
    """
    validate_party(party, election)

    wins = 0
    party_idx  = findfirst(x -> x == party, election.party)

    for dist in 1:size(election.vote_counts)[1]
        if argmax(election.vote_counts[dist, :]) == party_idx
            wins += 1
        end
    end

    return wins
end


function total_vote_counts(election::Election,
                           party::AbstractString)::Int
    """ Returns the total vote counts of `party` in `election`.
    """
    validate_party(party, election)
    party_idx  = findfirst(x -> x == party, election.party)
    return sum(election.vote_counts[:, party_idx])
end


function vote_counts_by_district(election::Election,
                                 party::AbstractString)::Array{Int, 1}
    """ Returns an array of vote counts by district for
        `party` in `election`.
    """
    validate_party(party, election)
    party_idx  = findfirst(x -> x == party, election.party)
    return election.vote_counts[:, party_idx]
end

function vote_shares_by_district(election::Election,
                                 party::AbstractString)::Array{Float64, 1}
    """ Returns an array of vote shares by district for
        `party` in `election`.
    """
    validate_party(party, election)
    party_idx  = findfirst(x -> x == party, election.party)
    return election.vote_shares[:, party_idx]
end


function vote_counts_by_district(election::Election,
                                 party::AbstractString,
                                 dist::Int)::Int
    """ Returns the sum of vote counts in a district for
        `party` in `election`.
    """
    return vote_counts_by_district(election, party)[dist]
end

function vote_shares_by_district(election::Election,
                                 party::AbstractString,
                                 dist::Int)::Float64
    """ Returns an array of vote shares by district for
        `party` in `election`.
    """
    return vote_shares_by_district(election, party)[dist]
end


function validate_party(party::AbstractString,
                        election::Election)
    """ Throws an ArgumentError if `party` is not in `election`.
    """
    if party ∉ election.party
        throw(ArgumentError(string("Party ", party,  " not contesting in election ", election, ".")))
    end
end
