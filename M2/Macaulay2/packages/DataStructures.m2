newPackage "DataStructures"

export {
    -- types
    "LinkedList",

    -- methods
    "linkedList"
    }

-----------------
-- linked list --
-----------------

-- each linked list is a hash table (so we can use iteration)
-- with a single key-value pair (Head)
-- every node is an ConsCell object or null

LinkedList = new Type of MutableHashTable
LinkedList.synonym = "linked list"
toString LinkedList := net LinkedList := net @@ toSequence

protect Head

-- cons cell (not exported) ----------------------------------------------------
ConsCell = new Type of MutableList
cons = (x, y) -> new ConsCell from {x, y}
car = x -> x#0
cdr = x -> x#1
toString ConsCell := net ConsCell := x -> concatenate(
    "(", toString car x, " . ", toString cdr x, ")")
--------------------------------------------------------------------------------

linkedList = method(Dispatch => Thing)

llhelper = (a, i) -> if #a <= i then null else cons(a#i, llhelper(a, i + 1))
linkedList Thing := x -> linkedList(1:x)
linkedList VisibleList := a -> new LinkedList from {Head => llhelper(a, 0)}

iterator LinkedList := a -> Iterator (
    node := a.Head;
    () -> (
	if node === null then StopIteration
	else (
	    r := car node;
	    node = cdr node;
	    r)))

length LinkedList := a -> (
    n := 0;
    node := a.Head;
    while node =!= null do (
	node = cdr node;
	n = n + 1);
    n)

end

loadPackage("DataStructures", Reload => true)

errorDepth = 0
linkedList ()
linkedList 1

linkedList(1, 2, 3, 4, "foo")

toString oo
toList oo

linkedList(1..10)
length oo
length linkedList()

linkedList (1)
peek oo

linkedList {(1,2), (3,4), (5, 6)}

LinkedList {Head => null}

peek oo
