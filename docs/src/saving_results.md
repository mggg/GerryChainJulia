# Saving results
Let's say that you've run a chain and want to save the resulting `ChainScoreData` object to your hard drive, so that you can do analysis of the results at a later time without having to re-run the chain. Below are some options you could pursue to do that.

```@index
Order = [:type, :function]
Pages   = ["saving_results.md"]
```

## Serialization
One super simple way to save the results is to use Julia's built-in `Serialization` library to save the `ChainScoreData` object. Here's an example:

```
chain_data = recom_chain(...)
serialize("example.jld", chain_data)
```

Then, in order to read back the saved data in another file, just run
```
chain_data = deserialize("example.jld")
```
Note that the `.jld` format is specific to Julia, so you won't be able to `deserialize` in a script written in another language, like Python or R.

## Saving scores to csv
CSV is a common format used to store data. We've written a function called `save_scores_to_csv()` into GerryChain that makes it super simple to export scores to a CSV format, which can then be read by programs in any language of your choosing. Each row of the CSV will correspond to one state in the chain, while each score corresponds to one or more columns. (District level scores will produce one column for each district. For example: a district-level score called `bvap` evaluated on plans with 10 districts will generate 10 columns: `bvap_1`, `bvap_2`, ...`bvap_10`.) The order of the rows corresponds to the order of the states visited by the chain. Here's what the function looks like:

```@docs
save_scores_to_csv
```

**Usage**

```
# let's say there are 3 districts and 2 steps in the chain
chain_data = recom_chain(...)
save_scores_to_csv("data.csv", chain_data, score_names = ["cut_edges", "vote_count_d", "vote_share_d"]) # 1 plan score, 2 district-level scores
```

**Resulting CSV**

```
cut_edges,vote_count_d_1,vote_count_d_2,vote_count_d_3,vote_share_d_1,vote_share_d_2,vote_share_d_3
5,2,4,4,0.2,0.4,0.4
6,3,5,2,0.3,0.5,0.2
```

## Saving score to JSON
```@docs
save_scores_to_json
```

**Usage**

```
chain_data = recom_chain(graph, partition, pop_constraint, 10, scores)
save_scores_to_json("test.json", chain_data) # by default will save all scores in the chain
# alternatively, save_scores_to_json("test.json", chain_data, ["names", "of", "desired", "scores"])
```
