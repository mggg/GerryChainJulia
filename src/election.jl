
function VotesByPartyScore(name::String,
                           party_names::Array{String, 1})::DistrictScore
    """ Given the names of the parties in a particular election, creates a
        DistrictScore that will track the total votes and vote share per
        district for each party.
    """
    function count_votes(graph::BaseGraph, nodes::BitSet)
        vote_counts = DefaultDict{String, DefaultDict}(()->DefaultDict{String, Int}(0))
        total_votes = 0
        for node in nodes
            for party in party_names
                party_votes = graph.attributes[node][party]
                vote_counts[party]["vote_count"] += party_votes
                total_votes += party_votes
            end
        end
        vote_pairs = Dict{String, NamedTuple}() # stores vote share and vote counts
        for party in party_names
            party_votes = vote_counts[party]["vote_count"]
            party_share = party_votes / total_votes
            vote_pairs[party] = (vote_count=party_votes, vote_share=party_share)
        end
        return vote_pairs
    end
    return DistrictScore(name, count_votes)
end


function SeatsWonByPartyScore(name::String,
                              party_names::Array{String, 1})::PlanScore
      """ Given the names of the parties in a particular election, creates a
          PlanScore that will track the total number of seats won for each
          party.
      """
      function count_seats(graph::BaseGraph, partition::Partition)
          seat_counts = zeros(length(party_names))
          for d in 1:partition.num_dists
              vote_counts = zeros(length(party_names)) # store vote counts by party
              for node in partition.dist_nodes[d]
                  for i in 1:length(party_names)
                      party_name = party_names[i]
                      vote_counts[i] += graph.attributes[node][party_name]
                  end
              end
              winning_party = argmax(vote_counts)
              seat_counts[winning_party] += 1
          end
          return NamedTuple{party_names}(seat_counts)
      end
      return PlanScore(name, count_seats)
end
