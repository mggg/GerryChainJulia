function sample_subgraph(graph::BaseGraph,
                         partition::Partition,
                         rng::AbstractRNG)
    """ Randomly sample two adjacent districts and returns them and their
        induced edges and nodes.
    """
    D₁, D₂ = sample_adjacent_districts_randomly(partition, graph.num_dists, rng)

    # take all their nodes
    nodes = union(partition.dist_nodes[D₁],
                  partition.dist_nodes[D₂])

    # get a subgraph of these two districts
    edges = induced_subgraph_edges(graph, collect(nodes))

    return D₁, D₂, edges, BitSet(nodes)
end

function build_mst(graph::BaseGraph, nodes::BitSet,
                   edges::BitSet)::Dict{Int, Array{Int, 1}}
    """ Builds a graph as an adjacency list from the `mst_nodes` and `mst_edges`.
    """
    mst = Dict{Int, Array{Int, 1}}()
    for node in nodes
        mst[node] = Array{Int, 1}()
    end
    for edge in edges
        add_edge_to_mst!(graph, mst, edge)
    end
    return mst
end

function remove_edge_from_mst!(graph::BaseGraph, mst::Dict{Int, Array{Int,1}}, edge::Int)
    """ Removes an edge from the graph built by `build_mst`.
    """
    filter!(e -> e != graph.edge_dst[edge], mst[graph.edge_src[edge]])
    filter!(e -> e != graph.edge_src[edge], mst[graph.edge_dst[edge]])
end

function add_edge_to_mst!(graph::BaseGraph, mst::Dict{Int, Array{Int,1}}, edge::Int)
    """ Adds an edge to the graph built by `build_mst`.
    """
    push!(mst[graph.edge_src[edge]], graph.edge_dst[edge])
    push!(mst[graph.edge_dst[edge]], graph.edge_src[edge])
end

function traverse_mst(mst::Dict{Int, Array{Int, 1}},
                      start_node::Int,
                      avoid_node::Int)::BitSet
    """ Returns the component of the MST `mst` that contains the vertex
        `start_node`.

        Arguments:
            mst:        mst to traverse
            start_node: the node to start traversing from
            avoid_node: the node to avoid adn which seperates the mst into
                        two components
            stack:      an empty Stack
            traversed_nodes: an empty BitSet that is to be populated.

        `stack` and `traversed_nodes` are are pre-allocated and passed in to
        reduce the number of memory allocations and consequently, time taken.
        In the course of calling this function multiple times, it is intended that
        we pass in the same (empty) objects repeatedly.
    """
    stack = Stack{Int}()
    traversed_nodes = BitSet([])
    push!(stack, start_node)

    while !isempty(stack)
        new_node = pop!(stack)
        push!(traversed_nodes, new_node)

        for neighbor in mst[new_node]
            if !(neighbor in traversed_nodes) && neighbor != avoid_node
                push!(stack, neighbor)
            end
        end
    end
    return traversed_nodes
end

function get_balanced_proposal(graph::BaseGraph,
                               mst_edges::BitSet,
                               mst_nodes::BitSet,
                               partition::Partition,
                               pop_constraint::PopulationConstraint,
                               D₁::Int,
                               D₂::Int)::AbstractProposal
    """ Tries to find a balanced cut on the subgraph induced by `mst_edges` and
        `mst_nodes` such that the population is balanced accoriding to
        `pop_constraint`.
        This subgraph was formed by the combination of districts `D₁` and `D₂`.
    """
    mst = build_mst(graph, mst_nodes, mst_edges)
    subgraph_pop = partition.dist_populations[D₁] + partition.dist_populations[D₂]

    for edge in mst_edges
        component₁ = traverse_mst(mst,
                                  graph.edge_src[edge],
                                  graph.edge_dst[edge])

        population₁ = get_subgraph_population(graph, component₁)
        population₂ = subgraph_pop - population₁

        if satisfy_constraint(pop_constraint, population₁, population₂)
            component₂ = setdiff(mst_nodes, component₁)
            proposal = RecomProposal(D₁, D₂, population₁, population₂, component₁, component₂)
            return proposal
        end
    end
    return DummyProposal("Could not find balanced cut.")
end

function get_all_balanced_proposals(graph::BaseGraph,
                                    mst_edges::BitSet,
                                    mst_nodes::BitSet,
                                    partition::Partition,
                                    pop_constraint::PopulationConstraint,
                                    D₁::Int,
                                    D₂::Int)::Set{AbstractProposal}
    """ Tries to find the set of balanced cuts on the subgraph induced by
       `mst_edges` and `mst_nodes` such that the population is balanced
        according to `pop_constraint`.

        This subgraph was formed by the combination of districts `D₁` and `D₂`.
    """
    mst = build_mst(graph, mst_nodes, mst_edges)
    subgraph_pop = partition.dist_populations[D₁] + partition.dist_populations[D₂]
    #println("subgraph pop: ", subgraph_pop)

    proposals = Set{AbstractProposal}()

    for edge in mst_edges
        component₁ = traverse_mst(mst,
                                  graph.edge_src[edge],
                                  graph.edge_dst[edge])

        population₁ = get_subgraph_population(graph, component₁)
        population₂ = subgraph_pop - population₁

        if satisfy_constraint(pop_constraint, population₁, population₂)
            component₂ = setdiff(mst_nodes, component₁)
            proposal = RecomProposal(D₁, D₂, population₁, population₂,
                                     component₁, component₂)
            #println("edge: ", edge, "\tpop1: ", population₁, "\tpop2:", population₂)
            push!(proposals, proposal)
        end
    end
    return proposals
end


function get_valid_proposal(graph::BaseGraph,
                            partition::Partition,
                            pop_constraint::PopulationConstraint,
                            tree_fn::F,
                            rng::AbstractRNG,
                            num_tries::Int=3) where
                            {F<:Function, S<:AbstractScore}
    """ Returns a population balanced proposal.

        Arguments:
            graph:          BaseGraph
            partition:      Partition
            pop_constraint: PopulationConstraint to adhere to
            tree_fn:        Function used to generate spanning trees
            num_tries:      num times to try getting a balanced cut from a subgraph
                            before giving up
    """
    while true
        D₁, D₂, sg_edges, sg_nodes = sample_subgraph(graph, partition, rng)

        for _ in 1:num_tries
            mst_edges = tree_fn(graph, sg_edges, collect(sg_nodes), rng)

            # see if we can get a population-balanced cut in this mst
            proposal = get_balanced_proposal(graph, mst_edges, sg_nodes,
                                             partition, pop_constraint,
                                             D₁, D₂)

            if proposal isa RecomProposal return proposal end
        end
    end
end


function update_partition!(partition::Partition,
                           graph::BaseGraph,
                           proposal::RecomProposal,
                           copy_parent::Bool=false,
                           weight::Float64=1.0)
    """ Updates the Partition with the RecomProposal
    """
    if copy_parent
        partition.parent = nothing
        old_partition = deepcopy(partition)
        partition.parent = old_partition
    end

    partition.dist_populations[proposal.D₁] = proposal.D₁_pop
    partition.dist_populations[proposal.D₂] = proposal.D₂_pop

    for node in proposal.D₁_nodes
        partition.assignments[node] = proposal.D₁
    end
    for node in proposal.D₂_nodes
        partition.assignments[node] = proposal.D₂
    end

    partition.dist_nodes[proposal.D₁] = proposal.D₁_nodes
    partition.dist_nodes[proposal.D₂] = proposal.D₂_nodes

    update_partition_adjacency!(partition, graph)
    update_partition_weight!(partition, weight)
end

function recom_chain(graph::BaseGraph,
                     partition::Partition,
                     pop_constraint::PopulationConstraint,
                     num_steps::Int,
                     scores::Array{AbstractScore, 1};
                     num_tries::Int=3,
                     acceptance_fn::Function=always_accept,
                     tree_fn::Function=random_kruskal_mst,
                     rng::AbstractRNG=Random.default_rng(),
                    )::ChainScoreData
    """Runs a Markov chain for `num_steps` steps using ReCom. Returns
        a ChainScoreData object which can be queried to retrieve the values of
        every score at each step of the chain.

        Arguments:
            graph:          BaseGraph
            partition:      Partition with the plan information
            pop_constraint: PopulationConstraint
            num_steps:      Number of steps to run the chain for
            scores:         Array of AbstractScores to capture at each step
            num_tries:      num times to try getting a balanced cut from a subgraph
                            before giving up
            acceptance_fn:  A function generating a probability in [0, 1]
                            representing the likelihood of accepting the
                            proposal. Should accept a Partition as input.
            tree_fn:        A function used to generate random spanning trees.
                            The GerryChain version of ReCom uses random minimum
                            spanning trees (fast, but not from the uniform
                            distribution); the reversible ReCom chain demands
                            spanning trees drawn from the uniform distribution.
            rng:            Random number generator. The user can pass in their
                            own; otherwise, we use the default RNG from Random.
    """
    steps_taken = 0
    first_scores = score_initial_partition(graph, partition, scores)
    chain_scores = ChainScoreData(deepcopy(scores), [first_scores])

    while steps_taken < num_steps
        proposal = get_valid_proposal(graph, partition, pop_constraint,
                                      tree_fn, rng, num_tries)
        custom_acceptance = acceptance_fn !== always_accept
        update_partition!(partition, graph, proposal, custom_acceptance)
        if custom_acceptance && !satisfies_acceptance_fn(partition, acceptance_fn)
            # go back to the previous partition
            partition = partition.parent
        end
        score_vals = score_partition_from_proposal(graph, partition, proposal, scores)
        push!(chain_scores.step_values, score_vals)
        steps_taken += 1
    end
    return chain_scores
end

function reversible_recom_chain(
    graph::BaseGraph,
    partition::Partition,
    pop_constraint::PopulationConstraint,
    M::Int,
    num_steps::Int, 
    scores::Array{S, 1}; 
    num_tries::Int=3,
    tree_fn::Function=wilson_ust,
    acceptance_fn::F=always_accept, 
    rng::AbstractRNG=Random.default_rng())::ChainScoreData where
    {F<:Function, S<:AbstractScore}
    """Runs a Markov chain for `num_steps` steps using reversible ReCom.
       (See Cannon, Duchin, Randall, Rule 2020; forthcoming.)  Returns
        a ChainScoreData object which can be queried to retrieve the values of
        every score at each step of the chain.

        The reversible ReCom chain is slower than the "normal" (i.e. GerryChain-
        flavored) ReCom chain for the same number of _unique_ plans.
        This is for multiple reasons:
            * We draw spanning trees from the uniform distribution using
              a self-avoiding random walk, as described in Wilson 1996.
              This processs is asymptotically slower than drawing a random
              minimum spanning tree (that is, generating random edge weights
              and finding the minimum MST using Kruskal's or Prim's algorithm).
            * The chain has a high rejection probability compared to an otherwise
              equivalent ReCom chain. For the reversible chain, we count self-loops
              using the `weight` field in `partition`.

        `n_steps` specifies the number to steps to take _including_ self-loops;
        the number of plans in the returned data will usually be substantially
        smaller than `n_steps`.

        Arguments:
            graph:          BaseGraph
            partition:      Partition with the plan information
            pop_constraint: PopulationConstraint
            M:              An upper bound on the number of balance edges
                            in a proposal.
            num_steps:      Number of steps to run the chain for
            scores:         Array of AbstractScores to capture at each step
            num_tries:      num times to try getting a balanced cut from a subgraph
                            before giving up
            acceptance_fn:  A function generating a probability in [0, 1]
                            representing the likelihood of accepting the
                            proposal. Should accept a Partition as input.
            rng:            Random number generator. The user can pass in their
                            own; otherwise, we use the default RNG from Random.
    """
    steps_taken = 0
    self_loops = 0
    reasons = DefaultDict{String, Int}(0)
    first_scores = score_initial_partition(graph, partition, scores)
    chain_scores = ChainScoreData(deepcopy(scores), [first_scores])
    district_pairs = Set{Tuple}()
    for i in 1:graph.num_dists
        for j in (i + 1):graph.num_dists
            push!(district_pairs, (i, j))
        end
    end

    while steps_taken < num_steps
        #println(reasons)
        #println(partition.dist_populations)
        # Choose two random districts, not necessarily adjacent.
        D₁, D₂ = rand(district_pairs)
        if partition.dist_adj[D₁, D₂] == 0
            self_loops += 1
            reasons["non-adjacent district pair"]  += 1
        else
            sg_nodes = union(partition.dist_nodes[D₁], partition.dist_nodes[D₂])
            sg_edges = induced_subgraph_edges(graph, collect(sg_nodes))
            mst_edges = tree_fn(graph, sg_edges, collect(sg_nodes), rng)
            # TODO: would this be faster if proposals were generated lazily?
            proposals = get_all_balanced_proposals(graph, mst_edges, sg_nodes,
                                                   partition, pop_constraint, D₁, D₂) 
            n_proposals = length(proposals)
            if n_proposals > M
                throw("Number of balance edges ($n_proposals) exceeds upper bound ($M).")
            end
            if n_proposals > 0
                proposal = rand(proposals)
                seam_length = partition.dist_adj[D₁, D₂]
                if rand() < 1 / (M * seam_length)
                    custom_acceptance = acceptance_fn !== always_accept
                    update_partition!(partition, graph, proposal, custom_acceptance)
                    if custom_acceptance && !satisfies_acceptance_fn(partition, acceptance_fn)
                        # go back to the previous partition
                        partition = partition.parent
                        # case: self-loop on non-reversible ReCom acceptance function
                        self_loops += 1
                        reasons["custom acceptance function"] += 1
                    else
                        partition.weight = self_loops + 1
                        partition.chain_meta = Dict("reasons" => reasons)
                        println(reasons)
                        score_vals = score_partition_from_proposal(graph, partition,
                                                                   proposal, scores)
                        push!(chain_scores.step_values, score_vals)
                        # case: reset self-loops
                        self_loops = 0
                        reasons = DefaultDict{String, Int}(0)
                    end
                else
                    # case: self-loop on reversible ReCom rejection
                    self_loops += 1
                    reasons["seam length"] += 1
                end
            else
                # case: self-loop on lack of proposals
                self_loops += 1
                reasons["no proposals"] += 1
            end
        end
        steps_taken += 1
    end
    return chain_scores
end
