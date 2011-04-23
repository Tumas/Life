data QuadTree a = EmptyQuadTree | 
                  RegionNode a  |
                  QNode Int (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a) 
                    deriving (Show, Read, Eq)

size :: QuadTree a -> Float
size EmptyQuadTree          = 0
size (RegionNode _)         = 1 
size (QNode level _ _ _ _)  = 2 ** fromIntegral(level)

singletonQ :: a -> QuadTree a
singletonQ x = RegionNode x

northWest :: QuadTree a -> QuadTree a
northWest (QNode a nw ne sw se) = nw

northEast :: QuadTree a -> QuadTree a
northEast (QNode a nw ne sw se) = ne

southWest :: QuadTree a -> QuadTree a
southWest (QNode a nw ne sw se) = sw

southEast :: QuadTree a -> QuadTree a
southEast (QNode a nw ne sw se) = se

centeredSubNode :: QuadTree a -> QuadTree a
centeredSubNode (QNode a nw ne sw se) = QNode a (southEast nw) (southWest ne) (northEast sw) (northWest se)

centeredSubSubNode :: QuadTree a -> QuadTree a
centeredSubSubNode (QNode a nw ne sw se) = 
  QNode a (southEast (southEast nw)) (southWest (southWest ne)) (northEast (northEast sw)) (northWest (northWest se))

centeredHorizontal :: QuadTree a -> QuadTree a -> QuadTree a
centeredHorizontal w@(QNode a _ _ _ _) e = 
  QNode a (northEast (southEast w)) (southWest (northWest e)) (northEast (southEast w)) (northWest (southWest e))

centeredVertical :: QuadTree a -> QuadTree a -> QuadTree a
centeredVertical n@(QNode a _ _ _ _) s = 
  QNode a (southWest (southEast n)) (southEast (southWest n)) (northWest (northEast s)) (northEast (northWest s))

countNW :: QuadTree Int -> Int
countNW (QNode _ nw ne sw se) = sum $ map (\(RegionNode x) -> x) [(northWest nw), (northEast nw), (southWest nw), 
  (northWest ne), (southWest ne), (northWest se), (northEast sw), (northWest sw)]  

countNE :: QuadTree Int -> Int
countNE (QNode _ nw ne sw se) = sum $ map (\(RegionNode x) -> x) [(northEast nw), (southEast nw), (northWest ne),
  (northEast ne), (southEast ne), (northEast se), (northWest se), (northEast sw)]  

countSW :: QuadTree Int -> Int
countSW (QNode _ nw ne sw se) = sum $ map (\(RegionNode x) -> x) [(southWest nw), (southEast nw), (northWest ne), 
  (northWest se), (southWest se), (northWest sw), (southEast sw), (southWest sw)]  

countSE :: QuadTree Int -> Int
countSE (QNode _ nw ne sw se) = sum $ map (\(RegionNode x) -> x) [(southEast nw), (southEast ne), (southWest ne), 
  (northEast se), (southWest se), (southEast se), (northEast sw), (southEast sw)]  

judge :: QuadTree Int -> Int -> Int
judge (RegionNode 0) 3 = 1
judge (RegionNode 0) _ = 0
judge (RegionNode 1) 2 = 1
judge (RegionNode 1) 3 = 1
judge (RegionNode 1) _ = 0

evolve :: QuadTree Int -> QuadTree Int
evolve EmptyQuadTree                  = EmptyQuadTree
evolve (QNode 2 nw ne sw se) = QNode 1
  (RegionNode $ judge (southEast nw) (countNW nw))
  (RegionNode $ judge (southWest ne) (countNE ne))
  (RegionNode $ judge (northEast sw) (countSW sw))
  (RegionNode $ judge (northWest se) (countSE se))

evolve node@(QNode level nw ne sw se) = QNode (level-1)
  (QNode (level-2) n00 n01 n10 n11) 
  (QNode (level-2) n01 n02 n11 n12) 
  (QNode (level-2) n10 n11 n20 n21) 
  (QNode (level-2) n11 n12 n21 n22) 
  where 
    n00 = centeredSubNode nw
    n01 = centeredHorizontal nw ne
    n02 = centeredSubNode ne
    n10 = centeredVertical nw sw
    n11 = centeredSubSubNode node
    n12 = centeredVertical ne se
    n20 = centeredSubNode sw
    n21 = centeredHorizontal sw se
    n22 = centeredSubNode se

createQTree :: a -> a -> a -> a -> QuadTree a
createQTree x y z w = QNode 1 (singletonQ x) (singletonQ y) (singletonQ z) (singletonQ w)

formQTree :: QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a
formQTree r1@(RegionNode _) r2@(RegionNode _) r3@(RegionNode _) r4@(RegionNode _) = QNode 1 r1 r2 r3 r4  
formQTree q1@(QNode l _ _ _ _) q2 q3 q4                                           = QNode (l+1) q1 q2 q3 q4
