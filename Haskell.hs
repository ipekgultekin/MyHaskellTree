-- İpek Gültekin 2526390
-- I read and accept the submission rules and the extra rules specified in each question. This is my own work that is done by myself only. 

data TernaryTree = Empty | Node Int TernaryTree TernaryTree TernaryTree deriving (Show, Eq, Ord)

-- Part 1


completeternary n = build [1..n] -- we don't know the number of nodes, so we generate a list from 1 to n

-- I created a helper function to insert values into the tree one by one.
-- I considered all base cases: when the tree is empty, when only left child exists,
-- when there are two children (left and mid), and when all three children exist.
helper val Empty = Node val Empty Empty Empty
helper val (Node v Empty Empty Empty) = Node v (Node val Empty Empty Empty) Empty Empty
helper val (Node v left Empty Empty) = Node v left (Node val Empty Empty Empty) Empty
helper val (Node v left mid Empty) = Node v left mid (Node val Empty Empty Empty)
-- if all three children are full, we recursively try left, then mid, then right subtrees
-- I also defined an isFull function to check whether a node has all three children filled or not.
helper val (Node v left mid right) = if not (isFull left) then Node v (helper val left) mid right
else if not (isFull mid) then Node v left (helper val mid) right
else if not (isFull right) then Node v left mid (helper val right)
else Node v (helper val left) mid right

-- this helper function checks whether all three children of a node are non-empty
isFull (Node _ left mid right) = left /= Empty && mid /= Empty && right /= Empty
isFull _ = False

-- the first element of the list becomes the root,
-- and the rest are inserted into the tree using buildHelper
build [] = Empty -- base case
build (x:xs) = buildHelper xs (Node x Empty Empty Empty)

-- recursively inserts each element from the list into the tree using helper
buildHelper [] tree = tree -- base case
buildHelper (x:xs) tree = buildHelper xs (helper x tree)


-- Part 2

-- recursively finds the longest path in the tree (tree height)
completeternaryheight Empty = 0 -- base case
completeternaryheight (Node _ left mid right) = 1 + max (max (completeternaryheight left) (completeternaryheight mid)) (completeternaryheight right)

-- Part 3

-- returns to [nodes with 3 children, nodes with 2 children, nodes with 1 child, nodes with 0 children]
-- I write all possiblities with if-else, or, and operations 
childnoternary Empty = [0, 0, 0, 0] -- if the tree is empty, there are no nodes
childnoternary (Node _ left mid right) = 
  if left /= Empty && mid /= Empty && right /= Empty then -- if the node has 3 children, we increase the first index (!!0) because nodes with 3 children at the first index of list 
    [1 + childnoternary left !! 0 + childnoternary mid !! 0 + childnoternary right !! 0,
     childnoternary left !! 1 + childnoternary mid !! 1 + childnoternary right !! 1,
     childnoternary left !! 2 + childnoternary mid !! 2 + childnoternary right !! 2,
     childnoternary left !! 3 + childnoternary mid !! 3 + childnoternary right !! 3]
  else if (left /= Empty && mid /= Empty && right == Empty) || (left /= Empty && mid == Empty && right /= Empty) || (left == Empty && mid /= Empty && right /= Empty)then  -- if the node has 2 children, we increase the second index (!!1) because nodes with 2 children at the second index of list 
    [childnoternary left !! 0 + childnoternary mid !! 0 + childnoternary right !! 0,
     1 + childnoternary left !! 1 + childnoternary mid !! 1 + childnoternary right !! 1,
     childnoternary left !! 2 + childnoternary mid !! 2 + childnoternary right !! 2,
     childnoternary left !! 3 + childnoternary mid !! 3 + childnoternary right !! 3]
  else if (left /= Empty && mid == Empty && right == Empty) || (left == Empty && mid /= Empty && right == Empty) || (left == Empty && mid == Empty && right /= Empty)then   -- if the node has 1 child, we increase the third index (!!2) because nodes with 1 children at the third index of the list 
    [childnoternary left !! 0 + childnoternary mid !! 0 + childnoternary right !! 0,
     childnoternary left !! 1 + childnoternary mid !! 1 + childnoternary right !! 1,
     1 + childnoternary left !! 2 + childnoternary mid !! 2 + childnoternary right !! 2,
     childnoternary left !! 3 + childnoternary mid !! 3 + childnoternary right !! 3]
  else -- if the node has no children (if they are node leafs), we increase the fourth index (!!3) because nodes with 0 children at the fourth index of the list 
    [childnoternary left !! 0 + childnoternary mid !! 0 + childnoternary right !! 0,
     childnoternary left !! 1 + childnoternary mid !! 1 + childnoternary right !! 1,
     childnoternary left !! 2 + childnoternary mid !! 2 + childnoternary right !! 2,
     1 + childnoternary left !! 3 + childnoternary mid !! 3 + childnoternary right !! 3]


-- Part 4

-- should return: [[3-child nodes], [2-child nodes], [1-child nodes], [leaf nodes]]
-- I write all possiblities with if-else, or, and operations 
childlistternary Empty = [[], [], [], []] -- if the tree is empty, return four empty lists
childlistternary (Node v left mid right) =
  if left /= Empty && mid /= Empty && right /= Empty then -- the node has 3 children, add v to the first list (!!0) (I assume same idea with part 3 again with indexes)
    [v : (childlistternary left !! 0 ++ childlistternary mid !! 0 ++ childlistternary right !! 0),
     childlistternary left !! 1 ++ childlistternary mid !! 1 ++ childlistternary right !! 1,
     childlistternary left !! 2 ++ childlistternary mid !! 2 ++ childlistternary right !! 2,
     childlistternary left !! 3 ++ childlistternary mid !! 3 ++ childlistternary right !! 3]
  else if (left /= Empty && mid /= Empty && right == Empty) || (left /= Empty && mid == Empty && right /= Empty) || (left == Empty && mid /= Empty && right /= Empty) then  -- the node has 2 children (left and mid), add v to the second list (!!1)
    [childlistternary left !! 0 ++ childlistternary mid !! 0 ++ childlistternary right !! 0,
     v : (childlistternary left !! 1 ++ childlistternary mid !! 1 ++ childlistternary right !! 1),
     childlistternary left !! 2 ++ childlistternary mid !! 2 ++ childlistternary right !! 2,
     childlistternary left !! 3 ++ childlistternary mid !! 3 ++ childlistternary right !! 3]
  else if (left /= Empty && mid == Empty && right == Empty) || (left == Empty && mid /= Empty && right == Empty) || (left == Empty && mid == Empty && right /= Empty) then  -- the node has 1 child (only left), add v to the third list (!!2)
    [childlistternary left !! 0 ++ childlistternary mid !! 0 ++ childlistternary right !! 0,
     childlistternary left !! 1 ++ childlistternary mid !! 1 ++ childlistternary right !! 1,
     v : (childlistternary left !! 2 ++ childlistternary mid !! 2 ++ childlistternary right !! 2),
     childlistternary left !! 3 ++ childlistternary mid !! 3 ++ childlistternary right !! 3]
  else -- if the node has no children (leaf), add v to the fourth list (!!3)
    [childlistternary left !! 0 ++ childlistternary mid !! 0 ++ childlistternary right !! 0,
     childlistternary left !! 1 ++ childlistternary mid !! 1 ++ childlistternary right !! 1,
     childlistternary left !! 2 ++ childlistternary mid !! 2 ++ childlistternary right !! 2,
     v : (childlistternary left !! 3 ++ childlistternary mid !! 3 ++ childlistternary right !! 3)]
