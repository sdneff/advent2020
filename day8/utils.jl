# program model

abstract type AbstractInstruction end

struct Nop <: AbstractInstruction
    index::Int
    value::Int
end

struct Accum <: AbstractInstruction
    index::Int
    increment::Int
end

struct Jump <: AbstractInstruction
    index::Int
    offset::Int
end

const Program = Array{AbstractInstruction}

function parseInstruction(index::Int, s::String)
    m = match(r"(nop|acc|jmp) ([+-]\d+)", s)

    name = m[1]
    value = parse(Int, m[2])

    if name == "nop"
        return Nop(index, value)
    elseif name == "acc"
        return Accum(index, value)
    elseif name == "jmp"
        return Jump(index, value)
    end
end

function buildProgram(instructions::Array{String})
    return [parseInstruction(index, s) for (index, s) = enumerate(instructions)]
end

# program execution

mutable struct ProgramState
    value::Int
    terminated::Bool
    function ProgramState()
        return new(0, false)
    end
end

function processInstruction(index::Int, state::ProgramState, program::Program)
    if length(program) < index
        state.terminated = true
        return 0
    end
    instruction = program[index]
    return processInstruction(instruction, state)
end

function processInstruction(nop::Nop, state::ProgramState)
    return nop.index + 1
end

function processInstruction(acc::Accum, state::ProgramState)
    state.value = state.value + acc.increment
    return acc.index + 1
end

function processInstruction(jmp::Jump, state::ProgramState)
    return jmp.index + jmp.offset
end

function run(program::Program)
    history = Set{Int}()
    state = ProgramState()

    index = 1
    while (index > 0            # program terminates naturally
        && !in(index, history)) # program enters infinite loop
        push!(history, index)
        index = processInstruction(index, state, program)
    end

    return state
end
