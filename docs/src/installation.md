# Installations

If you haven't already installed Julia, first order of business is to get that set up.

## Installing Julia

1. **Install Julia**. You can find a link to the appropriate installer at
[https://julialang.org/downloads/](https://julialang.org/downloads/).
2. **Check to make sure you can run Julia from the command line**.
Open up a Terminal/Command Prompt and type `julia` and press Enter.
If this results in an error instead of the Julia interpreter, there may be
a `PATH` issue. You can find official instructions on how to add Julia to
your `PATH` at [this page](https://julialang.org/downloads/platform/).
(**N.B.** If you are using Windows, the full path you should be adding
to your path should look something like this:
`C:\Users\[username]\AppData\Local\Programs\Julia\Julia-[version]\bin`.)
3. **Install a code editor**. For development in Julia, we recommend Juno,
which requires the installation of Atom. You can find instructions on the
steps to install Juno [here](http://docs.junolab.org/latest/man/installation/).
5. **Check that you can run the Julia REPL in Atom**. Open Atom and then
click `Juno > Open REPL`.  If you see an error, you may need to navigate
to `Juno > Settings` and change the Julia Path to the appropriate location.
This should allow you start the Julia interpreter from the REPL.

## Setting up the GerryChain environment

We highly recommend you use a virtual environment for development in your projects. Virtual Environments can be easily set up using Julia's builtin package manager `Pkg`.
1. Type `julia` into your command line to start the julia interpreter.
2. Type `using Pkg; Pkg.activate("NameOfEnvironment"; shared=true)` .
This creates a virtual environment with the name `NameOfEnvironment`
(you should pick a snazzier name!).
3. Type `Pkg.add("GerryChain")`. After the package has been installed you can
exit out of the interpreter by typing `exit()`. You have successfully
installed `GerryChain` in your virtual environment!
4. Optional: If you want to code on Jupyter Notebook, also type in
`Pkg.add("IJulia")` on the julia console. Next time you do `jupyter notebook`,
you will see an option under the `New` tab to start a Julia notebook.
5. When you run a script or want to use GerryChain through Jupyter notebooks,
you will want to start your script/notebook with
```
using Pkg; Pkg.activate("NameOfEnvironment"; shared=true)
using GerryChain
```
You are all set to go!
