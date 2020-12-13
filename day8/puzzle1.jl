include("utils.jl")

instructions = readlines("instructions.txt")
program = buildProgram(instructions)

println("program state before revisit: ", run(program).value)
