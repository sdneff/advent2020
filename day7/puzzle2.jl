include("utils.jl")

function countContents(graph::WeightedDirectedGraph, key::String)
    function countChildren(k::String)
        if !haskey(graph, k) || isempty(keys(graph[k]))
            return 0
        end

        return sum([graph[k][c] * (1 + countChildren(c)) for c = keys(graph[k])])
    end

    return countChildren(key)
end

rules = readlines("rules.txt")

# get directed graph parent->child
bagGraph = buildGraph(rules, child::GraphDirection)

withinShinyGold = countContents(bagGraph, "shiny gold")

println("bags within shiny gold: ", withinShinyGold)
