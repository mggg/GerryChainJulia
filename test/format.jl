using Pkg
Pkg.add(PackageSpec(name = "JuliaFormatter"))
using JuliaFormatter
format(".", verbose = true)
