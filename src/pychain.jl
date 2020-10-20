function call_test()
    println("Hello world!")
end

function call_test(a::Function)
    a()
end
