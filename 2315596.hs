--Can, Uncuoglu, 2315596
-- I read and accept the submission rules and the extra rules. This is my own work that is done by myself only

data TernaryTree = Empty | Node String TernaryTree TernaryTree TernaryTree | NodeExists | NotReachable | NodeNotFound deriving (Show, Eq, Ord)

--Part One
charToInt::Char->Int
charToInt a = (read [a]) + 0

-- Part Two
--The helper funciton pathFinder finds a path to the node that wanted to be implemented
pathFinder [] = []
pathFinder (x:xs) = if (x == '.') then charToInt (head xs) : pathFinder xs ++ [] else pathFinder xs

--The helper funciton isEmpty checks wheter tree is empty or not.
isEmpty tree = if (tree == Empty) then True else False 

--The helper funciton nodeValue returns value of input Node
nodeValue Empty = ""
nodeValue (Node value leftSide midSide rightSide) = value

--The helper funciton searchElement checks wheter the input node that wanted to be inserted is already exist
searchElement Empty strNode [] = False
searchElement Empty strNode (x:xs) = False
searchElement (Node a leftSide midSide rightSide) strNode [] = False
searchElement (Node a leftSide midSide rightSide) strNode (x:xs) =  if x == 1 
then (if strNode == (nodeValue leftSide) then True else searchElement leftSide strNode xs)
else if x == 2 
then (if strNode == (nodeValue midSide) then True else  searchElement midSide strNode xs)
else (if strNode == (nodeValue rightSide) then True else searchElement rightSide strNode xs)

--The helper function reachCheck checks tree wheter the input node that wanted to be inserted is at reach.
reachCheck Empty [] = False
reachCheck Empty (x:xs) = if (x:xs) /= [] then True else False
reachCheck (Node a leftSide midSide rightSide) [] = False
reachCheck (Node a leftSide midSide rightSide) (x:xs) = if x == 1 then reachCheck leftSide xs
else if x == 2 then reachCheck midSide xs
else reachCheck rightSide xs

--The helper function insertElement inserts input element to tree
insertElement Empty strNode [] = Node strNode Empty Empty Empty
insertElement (Node a leftSide midSide rightSide) strNode (x:xs) = if (searchElement (Node a leftSide midSide rightSide) strNode (x:xs)) then NodeExists
else if (reachCheck (Node a leftSide midSide rightSide) (x:xs)) then NotReachable
else (if x == 1 then Node a (insertElement leftSide strNode xs) midSide rightSide
else if x == 2 then Node a leftSide (insertElement midSide  strNode xs) rightSide
else Node a leftSide midSide (insertElement rightSide strNode xs))


--The function insertNode inserts input element to tree
insertNode::TernaryTree->[Char]->TernaryTree
insertNode terTree nodeStr = insertElement terTree nodeStr (pathFinder nodeStr)

-- Part Three
--The function totalNodes finds the number of nodes in the input tree
totalNodes::TernaryTree->Int
totalNodes Empty = 0
totalNodes (Node a Empty Empty Empty) = 1
totalNodes (Node a leftSide midSide rightSide) = let counter = 1 in counter + (totalNodes leftSide) + (totalNodes midSide) + (totalNodes rightSide) 

-- Part Four
--The function height finds height of the function via comparing nodes 
height::TernaryTree->Int
height Empty = 0
height (Node a Empty Empty Empty) = 1
height (Node a leftSide midSide rightSide) = let counter = 1 in counter + max (max (height leftSide) (height midSide)) (height rightSide)

-- Part Five
--the function levelcount counts number of nodes at certain level
levelcount::TernaryTree->Int->Int
levelcount Empty level  = 0
levelcount (Node a Empty Empty Empty) level = if level == 0 then 1 else 0 
levelcount (Node a leftSide midSide rightSide) 0 = 1
levelcount (Node a leftSide midSide rightSide) level = let counter = 0 in counter + levelcount leftSide (level-1) +  levelcount midSide (level-1) +  levelcount rightSide (level-1)

-- Part Six
--the helper function findNodehelper finds input node in the tree
findNodehelper (Node a leftSide midSide rightSide) nodeStr [] = Node a leftSide midSide rightSide
findNodehelper Empty nodeStr [] = NodeNotFound
findNodehelper (Node a leftSide midSide rightSide) nodeStr (x:xs) = if nodeStr == a then findNodehelper (Node a leftSide midSide rightSide) nodeStr []
else if x == 1 then findNodehelper leftSide nodeStr xs 
else if x == 2 then findNodehelper midSide nodeStr xs 
else findNodehelper rightSide nodeStr xs 

--the function findNode finds input node in the tree
findNode::TernaryTree->[Char]->TernaryTree
findNode (Node a leftSide midSide rightSide) value = findNodehelper (Node a leftSide midSide rightSide) value (pathFinder value) 