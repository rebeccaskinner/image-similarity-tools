module Data.BKTree where
import Prelude hiding (lookup)
import Text.Printf

data BKTreeData d a where
  Node :: a -> [(d, BKTreeData d a)] -> BKTreeData d a
  deriving Show

insertWithMetric :: (Eq d, Num d) => (a -> a -> d) -> a -> BKTreeData d a -> BKTreeData d a
insertWithMetric metric val t@(Node nodeVal children)
  | distance == 0 = t
  | otherwise =
      let children' = go children
      in Node nodeVal children'
  where
    distance = metric val nodeVal
    go [] = [(distance, Node val [])]
    go ((distance', n):rest)
      | distance' == distance = (distance', insertWithMetric metric val n) : rest
      | otherwise = (distance', n) : go rest

lookupWith :: (Ord d, Num d) => (a -> a -> d) -> a -> BKTreeData d a -> Maybe (a, d)
lookupWith metric val t =
      let
        (Node nodeVal children) = t
        distance = metric val nodeVal
        egressNodes = candidateEgressNodes distance distance children
      in Just $ lookup' nodeVal distance egressNodes
  where
    candidateEgressNodes currentDistance bestDistance children =
      [childNode | (childDistance, childNode) <- children, childDistance - currentDistance < bestDistance]
    lookup' bestVal bestDistance [] = (bestVal, bestDistance)
    lookup' bestVal bestDistance searchNodes =
      let (bestVal', bestDistance', egressNodes) = foldr filterNodes (bestVal, bestDistance, []) searchNodes
      in lookup' bestVal' bestDistance' egressNodes
    filterNodes (Node nodeVal children) (bestVal, bestDistance, egressNodes) =
      let
        distance = metric nodeVal val
        (bestVal', bestDistance') =
          if distance < bestDistance
          then (nodeVal, distance)
          else (bestVal, bestDistance)
        egressNodes' = egressNodes <> candidateEgressNodes distance bestDistance' children
      in (bestVal', bestDistance', egressNodes')

data BKTree d a = BKTree { bkTreeMetric :: a -> a -> d, bkTree :: Maybe (BKTreeData d a) }

empty :: (a -> a -> d) -> BKTree d a
empty metric = BKTree metric Nothing

singleton :: (a -> a -> d) -> a -> BKTree d a
singleton metric a = BKTree metric (Just $ Node a [])

insert :: (Eq d, Num d) => a -> BKTree d a -> BKTree d a
insert a (BKTree metric t) = BKTree metric (Just t')
  where
    t' = maybe (Node a []) (insertWithMetric metric a) t

insertList :: (Eq d, Num d) => [a] -> BKTree d a -> BKTree d a
insertList as t = foldl (flip insert) t as

fromList :: (Eq d, Num d) => (a -> a -> d) -> [a] -> BKTree d a
fromList metric [] = empty metric
fromList metric (a:as) = insertList as $ singleton metric a

bestMatch :: (Ord d, Num d) => a -> BKTree d a -> Maybe (a, d)
bestMatch a (BKTree metric t) = t >>= lookupWith metric a

ppTree :: (Show d, Show a) => BKTree d a -> String
ppTree (BKTree _ Nothing) = "digraph BKTree {}"
ppTree (BKTree _ (Just t)) =
  let body = unlines $ graphBody t
  in unlines ["digraph BKTree {", body, "}"]
  where
    graphBody (Node nodeVal children) =
      let nodeText = printf "%s;" (show nodeVal)
          nodeEdges = makeEdge nodeVal <$> children
      in nodeText : nodeEdges <> concatMap (graphBody . snd) children
    makeEdge nodeVal (d, Node childName _) =
      printf "%s -> %s [label=\"%s\"];" (show nodeVal) (show childName) (show d)

levenshteinDistance :: String -> String -> Int
levenshteinDistance stringA stringB =
  lookupEditDistance 0 0
  where
    distances =
      map (\idxA -> map (getEditDistance idxA) [0..bLen]) [0..aLen]
    lookupEditDistance idxA idxB =
      distances !! idxA !! idxB
    aLen = length stringA
    bLen = length stringB
    getEditDistance idxA idxB
      | idxA == aLen = bLen - idxB
      | idxB == bLen = aLen - idxA
      | stringA !! idxA == stringB !! idxB =
        lookupEditDistance (idxA + 1) (idxB + 1)
      | otherwise =
        let
          deleteCost = lookupEditDistance (idxA + 1) idxB
          insertCost = lookupEditDistance idxA (idxB + 1)
          swapCost = lookupEditDistance (idxA + 1) (idxB + 1)
        in 1 + minimum [deleteCost, insertCost, swapCost]
