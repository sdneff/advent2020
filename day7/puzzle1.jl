include("utils.jl")

function getDescendants(graph::WeightedDirectedGraph, key::String)
    descendants = Set{String}()

    function visitChildren(k::String)
        if haskey(graph, k)
            for child in keys(graph[k])
                if !in(child, descendants)
                    # println(child)
                    push!(descendants, child)
                    visitChildren(child)
                end
            end
        end
    end

    visitChildren(key)

    return descendants
end

rules = readlines("rules.txt")

# get directed graph child->parent
bagGraph = buildGraph(rules, parent::GraphDirection)

containShinyGold = getDescendants(bagGraph, "shiny gold")

println("bags that contain shiny gold: ", length(containShinyGold))
