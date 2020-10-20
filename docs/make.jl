using Documenter, GerryChain

makedocs(sitename="GerryChain",
         modules = [GerryChain],
         pages = Any[
            "Home" => "index.md",
            "Installation" => "installation.md",
            "Getting started with a chain" => "getting_started.md",
            "BaseGraph" => "graph.md",
            "Partition" => "partition.md",
            "Constraints" => "constraints.md",
            "Acceptance Functions" => "accept.md",
            "Scores" => Any[
                "Score Types" => "scores.md",
                "ChainScoreData" => "chain_score_data.md",
                "Plotting" => "plotting.md",
                "Saving Results" => "saving_results.md",
            ],
            "Election" => "election.md",
            "Markov Chains" => "chains.md",
         ]
)

deploydocs(
    repo = "github.com/mggg/GerryChainJulia.git",
    branch = "gh-pages",
    devbranch = "main",
    versions = ["stable" => "v^", "v#.#", devurl => devurl],
)
