newPackage("NetworkX",
    Headline => "interface to NetworkX, the Python graph library",
    PackageExports => {"Graphs", "Python"})

export {
    "NetworkXDigraph",
    "NetworkXGraph"
    }

NetworkXGraph = new SelfInitializingType of HashTable
NetworkXDigraph = new SelfInitializingType of NetworkXGraph
NetworkXMultigraph = new SelfInitializingType of NetworkXGraph
NetworkXMultiDigraph = new SelfInitializingType of NetworkXGraph

nx = null
loadnx = () -> if nx === null then nx = import "networkx"

new NetworkXGraph from VisibleList := (G, L) -> (
    loadnx();
    if #L == 0 then
	NetworkXGraph hashTable {"python object" => nx@@"Graph" ()} else
    error "expected 0 arguments"
)

callnx = (G, f, args) -> G#"python object"@@f args
tolist = toFunction rs "list"

net NetworkXGraph := G -> callnx(G, "__repr__", ())

vertexSet NetworkXGraph := G -> tolist callnx(G, "nodes", ())

addVertex(NetworkXGraph, Thing) := (G, v) -> addVertex(G, v, ())
addVertex(NetworkXGraph, Thing, VisibleList) := (G, v, attr) ->
    callnx(G, "add_Node", (1:v) | toSequence attr)

addEdge(NetworkXGraph, Thing, Thing) := (G, u, v) -> addEdge(G, u, v, ())
addEdge(NetworkXGraph, Thing, Thing, VisibleList) := (G, u, v, attr) ->
    callnx(G, "add_edge", (u, v) | toSequence attr)
