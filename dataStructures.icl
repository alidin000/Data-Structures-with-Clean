module dataStructures
import StdEnv
/*List implementation*/
/*Write a reverse function for a LinkedList data type*/
:: Prototype a = Node1 a (Prototype a) | Tail


:: LinkedList a :== (Prototype a)



firstList :: (LinkedList Int)
firstList = (Node1 4 (Node1 5(Node1 1 Tail)))

reverse :: (LinkedList a) -> (LinkedList a)
reverse list = iterate (reverseAux list)

iterate :: [(LinkedList a)] -> (LinkedList a)
iterate [] = Tail 
iterate [(Node1 n Tail):xs] = Node1 n (iterate xs) 

reverseAux :: (LinkedList a) -> [(LinkedList a)] 
reverseAux Tail = []
reverseAux (Node1 x nextChild) = (reverseAux nextChild) ++ [(Node1 x Tail)]

insertToLinkedList :: (LinkedList a) a -> (LinkedList a) | Eq a
insertToLinkedList Tail n = (Node1 n Tail)
insertToLinkedList (Node1 x nextChild) y
|x == y = (Node1 x nextChild)
= (Node1 x (insertToLinkedList nextChild y))

/* 
From the beginning 
insertToLinkedList Tail n = (Node n Tail)
insertToLinkedList (Node x nextChild) y  = (Node y (Node x nextChild ))
*/


instance toString (LinkedList a) | toString a
where
	toString Tail = "[]"
	toString (Node1 x nextChild) = "[" +++ toString x +++ (toStringAux nextChild)
		where
		toStringAux :: (LinkedList a) -> String | toString a
		toStringAux Tail = "]"
		toStringAux (Node1 x nextChild)  = "," +++ (toString x) +++ (toStringAux nextChild)
//Start= toString firstList
//Start= insert firstList 6
//Start = reverse firstList


/*Tree implementation*/
:: Tree a = Node a (Tree a) (Tree a) | Leaf
bestTree :: (Tree Int)
bestTree = Node 10(Node 6(Node 1 Leaf(Node 5(Node 2 Leaf(Node 4(Node 3 Leaf Leaf)Leaf))Leaf))Leaf)(Node 14(Node 11 Leaf(Node 13(Node 12 Leaf Leaf)Leaf))(Node 17(Node 15 Leaf(Node 16 Leaf Leaf))(Node 19(Node 18 Leaf Leaf)(Node 20 Leaf Leaf))))
ourTree :: (Tree Int)
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))
shortTree :: (Tree Int)
shortTree = Node 14(Node 11 Leaf(Node 13 Leaf Leaf))(Node 17(Node 15 Leaf Leaf)Leaf)
noTree :: (Tree Int)
noTree = Leaf
unitTree :: (Tree Int)
unitTree = Node 1337 Leaf Leaf
smallTree :: (Tree Int)
smallTree = Node 8 (Node 6 Leaf Leaf) (Node 12 Leaf Leaf)
//Getting the value at the node
extractNode :: (Tree a) -> a
extractNode (Node x l r) = x

//Going down left/right subtree
goL :: (Tree a) -> (Tree a)
goL (Node x l r) = l

goR :: (Tree a) -> (Tree a)
goR (Node x l r) = r

// Start = goR ourTree
// Start = goL ourTree

isLeaf :: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False

//Get a list of subtrees from a node.
getSubTrees :: (Tree a) -> [(Tree a)]
getSubTrees Leaf =[]
getSubTrees (Node x l r) = [l,r]


//Start = map (\(Node x l r) = x) (getSubTrees smallTree)

//Given a tree, find its depth

depthOfTree::(Tree a)->Int
depthOfTree Leaf = 0
depthOfTree (Node x l r) = 1 + max (depthOfTree l) (depthOfTree r)

//Start = depthOfTree bestTree // 7
//Start = depthOfTree ourTree // 5

treeToList :: (Tree a) -> [a]
treeToList Leaf = []
treeToList (Node x l r) =  treeToList l ++ [x] ++ treeToList r  

/*inorder traversal*/
InOrder :: (Tree a) -> [a]
InOrder Leaf = []
InOrder (Node x l r) =  InOrder l ++ [x] ++ InOrder r  
/*Preorder traversal*/
PreOrder :: (Tree a) -> [a]
PreOrder Leaf = []
PreOrder (Node x l r) = [x] ++ PreOrder l ++  PreOrder r  
/*Postorder traversal*/
PostOrder :: (Tree a) -> [a]
PostOrder Leaf = []
PostOrder (Node x l r) = PostOrder l ++  PostOrder r ++ [x]    
//Start = PostOrder smallTree 
//Start = PreOrder smallTree 
//Start = InOrder smallTree 
/*level order traversal*/
LevelOrder  :: (Tree a) ->[[a]]
LevelOrder t = [y\\y<-(aux [t] (depthOfTree(t)))|length(y) > 0]
where
    MyextractNode :: (Tree a) -> [a]
    MyextractNode Leaf = []
    MyextractNode (Node a l r) = [a]
	aux :: [(Tree a)] Int -> [[a]]
	aux l 0 = [flatten[MyextractNode x\\x<-l]]
	aux [Leaf] _ = [[]]
	aux list level = [flatten[MyextractNode x\\x<-list]] ++ aux (flatten[getSubTrees x\\x<-list]) (level-1)
//Start = LevelOrder bestTree

minTreeNotBST :: (Tree a) -> a | Ord a 
minTreeNotBST tree = minList (treeToList tree)

//Get the min value of a Tree
minTree :: (Tree a) -> a | Ord a
minTree (Node x l r)
| isLeaf l = x
= minTree l

//Start = minTree ourTree


//Reverse a tree
reverseTree :: (Tree a) -> (Tree a)
reverseTree Leaf = Leaf
reverseTree (Node x l r) = (Node x (reverseTree r) (reverseTree l))


//Start = reverseTree ourTree

//Get the max value of a BST
maxTree :: (Tree a) -> a | Ord a
maxTree tree = minTree(reverseTree tree)
//Start = maxTree ourTree

maxTreeNotBST :: (Tree a) -> a | Ord a 
maxTreeNotBST tree = maxList (treeToList tree)


//Create a list of all subtrees
subTreeList :: (Tree a) -> [(Tree a)]
subTreeList Leaf = []
subTreeList (Node x l r) = subTreeList( l) ++ [(Node x l r)] ++ subTreeList r

//Start = subTreeList ourTree 
//Start = treeToList ourTree 

//Extract sublists countaining a specific element

extractSubLists :: a (Tree a) -> [(Tree a)] | Eq a
extractSubLists n tree = [subtree\\subtree<-(subTreeList tree)|(extractNode subtree)==n]


// Start = extractSubLists 3 ourTree

//Get a list of children of a node

getChildren :: a (Tree a) -> [a] | Eq a
getChildren n tree
| isLeaf(goL subtree) && isLeaf(goR subtree)=[]
| isLeaf(goL subtree) = [extractNode(goR subtree)]
| isLeaf(goR subtree) = [extractNode(goL subtree)]
= [extractNode(goL subtree)]++[extractNode(goR subtree)]
    where
        subtree = hd(extractSubLists n tree)

//Start = getChildren 20 ourTree
//Start = getChildren ((getChildren 20 ourTree)!!0) ourTree ++ getChildren ((getChildren 20 ourTree)!!1) ourTree
//binary search tree

SumTree :: (Tree Int) -> Int
SumTree Leaf = 0
SumTree (Node x l r) = x + (SumTree l) + (SumTree r)

getMax :: (Tree a) -> a
getMax (Node x l r)
| isLeaf r = x
= getMax r

// Start = getMax (Node 1 (Node 0 (Node -1 Leaf Leaf) Leaf) (Node 5 Leaf Leaf)) // 5

// Add a new node to a BST
addNode :: a (Tree a) -> (Tree a) | Ord, Eq a
addNode n Leaf = Node n Leaf Leaf
addNode n (Node x l r)
| n == x = (Node x l r)
| n < x = addNode n l
| n > x = addNode n r

listToBST :: [Int] -> (Tree Int)
listToBST [] = Leaf
listToBST [x:xs] = addNode x (listToBST xs)

// Start = listToBST [2,41,2,31,3,1,2,20]


/*HashMap implementation*/
:: HashMap a b :== [(a,b)] 



myMap :: (HashMap String Int)
myMap = [("first", 1),("second", 2),("third", 3)]

myMap2 :: (HashMap Char Real)
myMap2 = [('a', 55.5),('b', 234.2),('c', -563.2)]


/* a) keysNum - Calcualte the number of keys in the HashMap */

keysNum :: (HashMap a b) -> Int
keysNum [] = 0
keysNum [x:xs] = sum (map (\x = 1) [x:xs])

// Start=keysNum myMap//3
// Start=keysNum myMap2//3


/*
    b) valueForKey - Gives back the value associated with a given key.
    If the key is not in the HashMap return "The key is not in the HashMap"
*/

valueForKey::(HashMap a b) a -> b | Eq a
valueForKey [] key2 = abort "The key is not in the HashMap\n" 
valueForKey [(key,value):xs] key2
|key == key2 = value 
= valueForKey xs key2

// Start=valueForKey myMap "first"//1
// Start=valueForKey myMap2 'a'//1
// Start=valueForKey myMap "firstt"//The key is not in the HashMap


/*    c) insert-Inserts a new tuple if the key value is not in the HashMap already,
    or give back "The given key already exists" if the key is already in the HashMap
*/

insert::(HashMap a b) (a,b)->(HashMap a b) |Eq a  
insert [] x = [x]
insert [(key,value):xs] (key2,value2)
|key == key2 = abort "The given key already exists\n"
=  insert xs (key2,value2) ++ [(key,value)]

// Start  = insert myMap ("third",12312)//"The given key already exists"
// Start = insert myMap ("fourth",1)//[("fourth",1),("third",3),("second",2),("first",1)]

/*    d) remove-remove the (key, value) pair for a given key.
    If the key is not in the HashMap return "The key is not in the HashMap"
*/

remove::(HashMap a b) a -> (HashMap a b) |Eq a
remove [] _ = abort "The key is not in the HashMap\n" 
remove [(key,value):xs] key2 |key == key2 = xs = [(key,value)] ++ remove xs key2
 
//Start=remove myMap "first"//[("second",234234),("third",21231)]
//Start=remove myMap "someOtherKey"//The key is not in the dictionary