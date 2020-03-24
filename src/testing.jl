using JSON


open("measures.json", "r") do f
    global arr
    arr = JSON.parse(f)  # parse and transform data
end

# println(arr)

# println(arr)
println(typeof(arr))

println(arr[2]["PRES12_Rep"][1])
println(arr[1]["PRES12_Rep"][1])


# also need to store metadata for the file with types and keys and stuff
