# type alias for graph
const WeightedDirectedGraph = Dict{String,Dict{String,Int}}

# graph direction encoding
@enum GraphDirection begin
    parent
    child
end

function loadRule(s::String, map::WeightedDirectedGraph, dir::GraphDirection)
    m1 = match(r"([\w ]+) bags contain (.*).", s)

    bagName = m1[1]
    contents = m1[2]

    if contents == "no other bags"
        if dir == child::GraphDirection
            map[bagName] = Dict{String,Int}()
        end
        return
    end

    # [("child bag", 4), ...]
    members = [(m2[2], parse(Int, m2[1])) for m2 in
                  (match(r"(\d+|no) ([\w ]+) bags?", member) for member in
                      split(contents, ", "))]

    for member = members
        childName = member[1]
        count = member[2]
        if dir == child::GraphDirection
            # map["parent bag"] = { "child bag" => 4, ... }
            d = get!(map, bagName, Dict{String,Int}())
            d[member[1]] = count
        elseif dir == parent::GraphDirection
            # map["child bag"] = { "parent bag" => 4, ... }
            d = get!(map, member[1], Dict{String,Int}())
            d[bagName] = member[2]
        end
    end
end

function buildGraph(rules::Array{String}, dir::GraphDirection)
    graph = Dict{String,Dict{String,Int}}()

    for rule = rules
        loadRule(rule, graph, dir::GraphDirection)
    end

    return graph
end
