## How to add documentation to the GerryChain Julia project

### Adding a Doc-string
We use `Documenter.jl` to generate our documentation. For `Documenter` to recognize
a doc-string, you should add the doc-string _before_ the function. Lets say you
have a function that adds two numbers. You would want to add documentation in the
following way:

"""
    MyAdd(arg₁::Int,
          arg₂::Int)::Int

Adds the two argument integers. *Returns* the result as an Int.
"""
MyAdd(arg₁::Int,
      arg₂::Int)::Int
    arg₁ + arg₂
end

The doc-string inside the tripe quotation marks will render as MarkDown, so you
can also use other MarkDown styles like _italics_ or **bold** there if you want.

### Rendering the doc-string in the documentation page
Each page in the documentation is a different `.md` file in `docs/src/`. Say you
want to add documentation of `your_function()` to `doc_file.md`. Then, you would
go to `docs/src/doc_file.md` and add the documentation where you would want it
to appear like this:

```@docs
save_scores_to_json
```

That's it! You might want to add something more about this function, which you
could add simply as MarkDown text underneath it. Please check out any of the
`.md` files in `docs/src` for examples.

### Rendering the documentation locally
You will probably want to see how your added documentation looks like before you
make a Pull Request. To do that:  
1. Go to the root of this repository locally in your terminal.
2. Enter `julia --project=. docs/make.jl` . This will generate the HTML files
   locally in the `docs/build` directory.
3. Enter `python -m http.server --bind localhost` . This will run a local server
   that hosts the documentation.
4. The terminal should say that the documentation is being served at some port number.
   Use your favorite browser to go to that port (for eg, it looks like `localhost:8000`.
   Enter that into the url.)
5. Go to `docs/build/`. You should be able to see what the updated documentation looks like!

## Adding a new page
If you wish to add a new page to the documentation, you can add a new `.md` file
in `docs/src/`.

Then, add the file to the `makedocs()` function in `docs/make.jl`. Additionally,
this if you wish to change the order in which the pages are listed, changing the
ordering in this function is how you would do so.

## Final Note
An important note to know is that even if the documentation gets merged into the
`main` branch, the documentation page _will not_ be updated until a new version
of GerryChain is released.

So don't worry if your fresh, much-appreciated documentation is not up there yet,
it will be soon!!
