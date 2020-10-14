# ChainScoreData

The purpose of the `ChainScoreData` object is reflected in its name: its purpose is to store data about the values of scores throughout the entire history of the Markov chain. You can think of it as containing an `Array` of `Dict` objects, where each element in the array is a `Dict` that corresponds to one state of the chain. In turn, each `Dict` contains keys for every `AbstractScore` passed to the chain by the user, and the values of the `Dict` are the values of the scoring functions, evaluated on a particular plan in the chain.

## get\_scores\_at\_step()

```@docs
get_scores_at_step
```

`recom_chain` or `flip_chain` returns a `ChainScoreData` object. If you want to know what the values of any/all scores were at a particular step in the chain, use `get_scores_at_step`. This will return a `Dict{String, Any}` from the name of the score to its value at step `step`. If no scores are passed in, all scores are returned by default. Here, `step=0` represents the score of the original (initial) partition, so step=`t` will return the scores of the plan that was produced after taking `t` steps of the Markov chain.

Running `get_scores_at_step()` for different score types would get you different
kinds of return values:
- Running `get_scores_at_step(chain_data, step, "name_of_district_aggregate_score")`
  would return an `Array` of length `d`, where `d` is the number of districts.
- Running `get_scores_at_step(chain_data,
  step, "name_of_district_score")` would return an `Array` of length `d`, where
  `d` is the number of districts.
- Running `get_scores_at_step(chain_data, step, "name_of_plan_score")` would
  return a single value, representing the value of the `PlanScore` for the plan
  at step `step`.

## get\_score\_values()

```@docs
get_score_values(::ChainScoreData,
                 ::String)
```

If you want to query the `ChainScoreData` for all values of a particular score throughout the history of the chain, use `get_score_values`. It will return an array of values where each element of the array corresponds to the value of the score at step `i` of the chain.
