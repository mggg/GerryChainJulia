# Contributing to GerryChainJulia

Thank you for your interest in contributing to GerryChainJulia! Please use the following guidelines on how to contribute to this project:

## How can I contribute?

### Reporting bugs

If you find a bug while using any tools in this repository, consider creating a [GitHub issue](https://github.com/mggg/GerryChainJulia/issues) documenting the problem. Be sure to be detailed. Explain what you did, what you expected to happen, and what actually happened.

### Updating documentation

If you find any documentation that is lacking or in error, submit a pull request that updates it. 

### Suggesting enhancements

If you have ideas for additional features, consider creating a [GitHub issue](https://github.com/mggg/GerryChainJulia/issues) documenting the desired feature. Be sure to be detailed. Explain what the tool currently does (or does not do), what you would like it to do, and how this benefits users.

### Contributing code (Pull Requests)

The main way to contribute code to the project is via GitHub pull requests (PRs). To create a pull request, see the [GitHub
docs](https://help.github.com/articles/creating-a-pull-request/). Be detailed in the description of your pull request. Explain what your proposed changes do, and reference any relevant issues.

**When making a PR:**

- If the PR you are going to submit is a ~large change with say > 80 lines of code, open an Issue and talk to the maintainers to get feedback before working on it. 
- We recommend [these guidelines](https://chris.beams.io/posts/git-commit/) for commit messages.
- Fix one issue at a time. If you are fixing more than one issue, then it should probably be multiple PRs.
- Be sure to follow the style guide.
- The title of the PR should be clear, and there should be an accompanying description. 
- If the PR changes something user-facing, the description should give an example of intended usage. The PR should also change the user guide to reflect the change.
- If you are adding new code, it should be unit tested.
- Respond to every comment. (to the best of your judgement! For example if the reviewer has simply complimented a smart line of code, it is ok to simply resolve it.)
- Comment on individual lines of the PR yourself if you want to bring special attention to it for feedback.
- If you are a core contributor and wish to push a large feature that would require multiple PRs, the recommended workflow is to first create a "feature" branch off of `main`. Then, make a PR that commits the architecture of this feature. This could simply be a function that has function stubs like 
	`do_x()`
	`do_y()` 
	without implementing the functions themselves. Then, make PRs that would handle `do_x()` and `do_y()` seperately. Request a review for each PR, and when the entire feature is complete it is easier to merge that into the `main` branch.

**When reviewing a PR:**

- Don't accept code that doesn't pass all tests.
- Don't accept commented out code.
- Don't accept code that has not been unit tested.
- Don't accept functions without documentation.
- If you see a bug in the PR, don't merge and fix the bug with another PR. Comment on the PR for the person putting in the PR to fix.
- If the person submitting the PR **is** a core contributor, do not merge the PR yourself, just approve it. The person submitting the PR should merge it.
- If the person submitting the PR **is not** a core contributor, the reviewer should merge the PR. 
- In the case that a PR is "good enough for now" but it is known that it will induce future changes, document that foreseen problem as an **Issue** immediately.
- Be judicious with any comments in the code that you don't understand.

**Guidelines for both the PR submitter and PR reviewer:**

- Emoji usage is encouraged, especially if you think it conveys tone 🙂

### Style Guide
* State the types of the arguments in a function whenever possible eg. `function foo(x::Int)`. If you intend the type to be anything, make that explicit with `Any` eg. `function foo(x::Any)`.
* State the return type of the function whenever possible eg. `function add_two(num::Int)::Int`.
* Append ! to names of functions that modify their arguments.
* Avoid using global variables. If you absolutely, absolutely must, use all caps eg. `const UNICORN = 4`.
* Use `in` instead of `=` in for loop iterations, eg. use `for i in 1:5` instead of `for i = 1:5`.
* Separate positional arguments from keyword arguments using a semicolon (;).
* Explicitly state `return` in your functions eg. `return x` instead of just `x`, which would also work.

This contributing document has been partially been derived from [its Python sibling](https://github.com/mggg/GerryChain/blob/master/CONTRIBUTING.md), and the Style Guide has been adopted from [the official Style Guide](https://docs.julialang.org/en/v1/manual/style-guide/index.html) and [jrevel's style guide.](https://github.com/jrevels/YASGuide)

