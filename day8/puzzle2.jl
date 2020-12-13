include("utils.jl")

function swapInstruction(jmp::Jump)
    return Nop(jmp.index, jmp.offset)
end

function swapInstruction(nop::Nop)
    return Jump(nop.index, nop.value)
end

function runAlternate(program::Program, instruction::AbstractInstruction)
    altInstruction = swapInstruction(instruction)

    # modify instruction in-place to avoid cloning
    program[instruction.index] = altInstruction

    result = run(program)

    # swap back to restore original state
    program[instruction.index] = instruction

    return result
end

function runAlternate(program::Program, instruction::Accum)
    # special case: we don't swap acc operations, so program would stay in original state
    # (which we know is non-terminating) so there's no point in running it
    return ProgramState()
end

instructions = readlines("instructions.txt")
program = buildProgram(instructions)

terminatingResult = first(Iterators.filter(s -> s.terminated,
    [runAlternate(program, instr) for instr = program]))

println("program state after termination (modified): ", terminatingResult.value)
