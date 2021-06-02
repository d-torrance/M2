newPackage("NetworkX",
    Headline => "interface to NetworkX, the Python graph library",
    PackageExports => {"Graphs", "Python"})

export {
    "nx",
    "nxGraph",
    "NetworkXDigraph",
    "NetworkXGraph"
    }

NetworkXGraph = new SelfInitializingType of BasicList
NetworkXDigraph = new SelfInitializingType of NetworkXGraph
NetworkXMultigraph = new SelfInitializingType of NetworkXGraph
NetworkXMultiDigraph = new SelfInitializingType of NetworkXGraph

toPython NetworkXGraph := first

nx = import "networkx"

nxGraph = method()
installMethod(nxGraph, () -> nxGraph({}, hashTable{}))
nxGraph(NetworkXGraph) :=
nxGraph(VisibleList) := data -> nxGraph(data, hashTable {})
nxGraph(NetworkXGraph, HashTable) :=
nxGraph(VisibleList, HashTable) := (data, attr) -> NetworkXGraph {
    init(nx@@"Graph", "incoming_graph_data" => data, "attr" => attr)}

getnx = (G, attr) -> (toPython G)@@attr
callnx = (G, f, args) -> (getnx(G, f)) args

net NetworkXGraph := G -> callnx(G, "__repr__", ())

vertexSet NetworkXGraph := G -> getnx(G, "nodes")

addVertex(NetworkXGraph, Thing) := (G, v) -> addVertex(G, v, ())
addVertex(NetworkXGraph, Thing, VisibleList) := (G, v, attr) ->
    callnx(G, "add_node", (1:v) | toSequence attr)

addVertices(NetworkXGraph, String) := (G, s) -> addVertices(G, toList s)
addVertices(NetworkXGraph, VisibleList) := (G, V) ->
    addVertices(G, V, ())
addVertices(NetworkXGraph, String, VisibleList) := (G, s, attr) ->
    addVertices(G, toList s, attr)
addVertices(NetworkXGraph, VisibleList, VisibleList) := (G, V, attr) ->
    callnx(G, "add_nodes_from",
	{"nodes_for_adding" => V, "attr" => hashTable toList attr})

addEdge(NetworkXGraph, Thing, Thing) := (G, u, v) -> addEdge(G, u, v, ())
addEdge(NetworkXGraph, Thing, Thing, VisibleList) := (G, u, v, attr) ->
    callnx(G, "add_edge", (u, v) | toSequence attr)


end

loadPackage("NetworkX", Reload => true)

-- example for nxGraph
-- https://networkx.org/documentation/stable/reference/classes/graph.html
G = nxGraph()
addVertex(G, 1)
addVertices(G, {2, 3})
addVertices(G, 100..109)
H = nx@@"path_graph" 10
pathGraph 10
G.add_nodes_from([2, 3])
G.add_nodes_from(range(100, 110))
H = nx.path_graph(10)
G.add_nodes_from(H)
