-- |
-- Module     : Data.Graphen
-- Copyright  : (c) 2015 Fritz Grimpen
-- License    : BSD3-style
--
-- Maintainer : Fritz Grimpen <fritz@grimpen.net>
-- Stability  : experimental
module Data.Graphen
  ( -- * Edges
    Edge(..)
  , (~~>)
  , (<~~)
  , (<~>)
  , (%)
  , isDir
  , isUndir
  , isSource
  , isDestination
  , isLoop
  , leftVertex
  , rightVertex
  , color
  , decolor
  , edgeContainsVertex
  , edgeToList
  , edgeToDot
  , fromTuple
  , ereverse
  , undir
  , dirEdge
  , dirEdgeBy
    -- * Graph type classes
  , Graph(..)
  , GraphFunctor(..)
    -- * Generic graph operations
  , (<+>)
  , (<->)
  , outgoing
  , incoming
  , distanceColor
    -- ** Graph types
  , GraphType(Directed, Undirected, Mixed, Empty)
  , graphType
  , isEmpty
  , isDirected
  , isUndirected
  , isMixed
    -- * Adjacency lists implementation
  , Gr()
  , mkGr
    -- * Edge constructors for graph context edge constructor lists
  , eOutgoing
  , eIncoming
  , eTouching
  , eLoop
    -- * Topological sort and cycle detection
  , tweight
    -- * Dijkstra's algorithm
    -- | This implementation of Dijkstra's algorithm is implemented as a fix point
    -- search on a distance-weighted absolute-distance-colored graph and is
    -- divided in three parts:
    -- 
    -- 1. Marking the vertices with their distance, a @Just@ value marks a finite
    --    distance and a @Nothing@ value marks an infinite distance, where infinite
    --    distances mean that the distance is either really infinite by a higher
    --    topology or not computed yet. This step is performed by the `dijkstraG`
    --    function, which takes a graph value and an vertex and marks the vertex
    --    with @Just mempty@, while the other vertices are marked with @Nothing@. This
    --    is comparable to the first step of traditional implementations of
    --    Dijkstra's algorithm, where the implementation has to mark the first
    --    vertex in a path.
    --
    --    Here is an example to clarify the behaviour of the first step:
    -- 
    --    >>> -- g satisfies the constraints given by `dijkstraG`
    --    >>> let g = 'A' <~> 'B' % 5 : 'B' <~> 'C' % 1 : [] & fromEdges
    --    >>> dijkstraG g 'A'
    --    ('A', Just 0) <~> ('B', Nothing) % 5 : ('B', Nothing) <~> ('C', Nothing) % 1 : [] & fromEdges
    --
    -- 2. Performing the cost calculation steps until the fix point is reached.
    --    This step is comparable to the second step of an imperative
    --    implementation of Dijkstra's algorithm. Specifically, this step
    --    performs the `dijkstraS` function, until the return value of the
    --    function is its input, which is the same as a fix point @f(x)=x@.
    --
    --    The `dijkstraS` function is an endomorphism on a graph data type with
    --    marked vertices. It tries to find the lowest known distance to the
    --    source vertex for each vertex in the graph by considering the
    --    neighbor distances and the edge weights. One restriction in the
    --    implementation in `dijkstraS` is, that it acts atomically on the data
    --    and will not respect calculated distances of vertices for other
    --    vertices by design. This is the reason why this process is a fix
    --    point search.
    --
    --    >>> let g = ('A', Just 0) <~> ('B', Nothing) % 5 : ('B', Nothing) <~> ('C', Nothing) % 1 : [] & fromEdges
    --    >>> dijkstraS g
    --    ('A', Just 0) <~> ('B', Just 5) % 5 : ('B', Just 5) <~> ('C', Nothing) % 1 : [] & fromEdges
    --    >>> dijkstraS.dijkstraS $ g
    --    ('A', Just 0) <~> ('B', Just 5) % 5 : ('B', Just 5) <~> ('C', Just 6) % 1 : [] & fromEdges
    --    >>> dijkstraS.dijkstraS.dijkstraS $ g -- Fix point reached
    --    ('A', Just 0) <~> ('B', Just 5) % 5 : ('B', Just 5) <~> ('C', Just 6) % 1 : [] & fromEdges
    --
    -- 3. Convert the distance-weighted graph to a path. This step is trivial
    --    and is performed by the `dijkstraP` function, which returns the way
    --    from a given vertex to the vertex with the distance weight 0 in the
    --    distance-weighted graph.
    --
    --    >>> let g = ('A', Just 0) <~> ('B', Just 5) % 5 : ('B', Just 5) <~> ('C', Just 6) % 1 : [] & fromEdges
    --    >>> dijkstraP g 'C'
    --    ['C', 'B', 'A']
    --
    -- This implementation requires that the edge weights are part of a Monoid,
    -- which provides an neutral element for marking the source vertex and a
    -- binary operation to calculate the minimum distance to the source vertex.
    -- Furthermore, it is required that the edge weights are totally ordered,
    -- which means that they need a left-total `Prelude.compare` function.
    --
    -- These three steps are composited in the `dijkstra` function, which also
    -- reverses the result of the third step `dijkstraP` and works on `Num`
    -- values by lifting them to `Data.Monoid.Sum` values.
  , dijkstra
  , dijkstraG
  , dijkstraS
  , dijkstraP
  ) where

import Data.List ((\\))
import Data.Maybe (isJust)
import Data.Monoid ((<>), Sum(Sum))
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Bifunctor (Bifunctor(..))

-- | A Edge represents a connection between two objects, also called vertices.
-- Any graph can be represented with a collection of edges.
data Edge v e
  = UndirEdge v v e -- ^ Construct a undirected edge between two vertices
  | DirEdge v v e  -- ^ Construct a directed edge from one vertex to another
  deriving (Show, Read)

instance Bifunctor Edge where
  -- Map over the vertices
  first f (UndirEdge v w e) = UndirEdge (f v) (f w) e
  first f (DirEdge v w e) = DirEdge (f v) (f w) e
  -- Map over the edge values
  second f (UndirEdge v w e) = UndirEdge v w (f e)
  second f (DirEdge v w e) = DirEdge v w (f e)

-- | Check if an edge is directed
isDir :: Edge v e -> Bool
isDir DirEdge{} = True
isDir _ = False

-- | Check if an edge is undirected
isUndir :: Edge v e -> Bool
isUndir UndirEdge{} = True
isUndir _ = False

infix 7 ~~>
-- | The `~~>` operator constructs an directed edge from the first argument to
-- the second argument.
(~~>) :: v -> v -> Edge v ()
v ~~> w = DirEdge v w ()

infix 7 <~~
-- | The `<~~` operator is the same as the `~~>`, but with its arguments
-- flipped.
(<~~) :: v -> v -> Edge v ()
(<~~) = flip (~~>)

infix 7 <~>
-- | The `<~>` operator constructs an undirected edge.
(<~>) :: v -> v -> Edge v ()
v <~> w = UndirEdge v w ()

infixl 6 %
-- | The `%` operator colors an edge with a given color.
(%) :: Edge v a -> e -> Edge v e
UndirEdge v1 v2 _ % e = UndirEdge v1 v2 e
DirEdge v1 v2 _ % e = DirEdge v1 v2 e

-- | Extract the left vertex in an edge.
leftVertex :: Edge v e -> v
leftVertex (UndirEdge v _ _) = v
leftVertex (DirEdge v _ _) = v

-- | Extract the right vertex in an edge.
rightVertex :: Edge v e -> v
rightVertex (UndirEdge _ v _) = v
rightVertex (DirEdge _ v _) = v

-- | Return @True@, when the given vertex is a destination of the given `Edge`.
isDestination :: Eq v => Edge v e -> v -> Bool
isDestination (UndirEdge v w _) v' = v' == w || v' == v
isDestination (DirEdge _ v _) v' = v == v'

-- | Return @True@, when the given vertex is a source of the given `Edge`.
isSource :: Eq v => Edge v e -> v -> Bool
isSource (UndirEdge v w _) v' = v' == w || v' == v
isSource (DirEdge v _ _) v' = v == v'

-- | Return @True@, when the given `Edge` has same source and destination.
isLoop :: Eq v => Edge v e -> Bool
isLoop e = leftVertex e == rightVertex e

-- | Extract the edge color/weight from an edge.
color :: Edge v e -> e
color (UndirEdge _ _ e) = e
color (DirEdge _ _ e) = e

-- | Create edge value from tuple.
fromTuple :: (v, v) -> Edge v ()
fromTuple (v, w) = DirEdge v w ()

-- | showS converts a showable value to an escaped String with quotes
showS :: (Show a) => a -> String
showS = show . show

-- | Reverse directed edges
ereverse :: Edge v e -> Edge v e
ereverse (DirEdge v w e) = DirEdge w v e
ereverse e = e

-- | Convert directed edge to undirected edge
undir :: Edge v e -> Edge v e
undir (DirEdge v w e) = UndirEdge v w e
undir e = e

-- | Convert an undirected edge to a directed edge by determining the order of
-- the vertices. This version is similar to `dirEdge`, but it takes a custom
-- ordering function instead of `compare`.
dirEdgeBy :: (v -> v -> Ordering) -> Edge v e -> Edge v e
dirEdgeBy cmp (UndirEdge v w e) = (case v `cmp` w of
  GT -> (~~>)
  LT -> (<~~)
  EQ -> (~~>)) v w % e
dirEdgeBy _ e = e

-- | Convert an undirected edge to a directed edge by determining the order of
-- the vertices. This function is only applicable on totally-ordered data types.
dirEdge :: Ord v => Edge v e -> Edge v e
dirEdge = dirEdgeBy compare

-- | Convert a showable @Edge@ value to a GraphViz representation
edgeToDot :: (Show v, Show e) => Edge v e -> String
edgeToDot (UndirEdge v1 v2 e)
  = showS v1 ++ " -- " ++ showS v2 ++ " [label=" ++ showS e ++ "];"
edgeToDot (DirEdge v1 v2 e)
  = showS v1 ++ " -> " ++ showS v2 ++ " [label=" ++ showS e ++ "];"

-- | Convert the vertices of an edge to a list.
edgeToList :: Edge v e -> [v]
edgeToList (UndirEdge v w _) = [v, w]
edgeToList (DirEdge v w _) = [v, w]

-- | Remove color information from an edge by replacing the color by an empty
-- tuple.
decolor :: Edge v e -> Edge v ()
decolor (UndirEdge v1 v2 _) = UndirEdge v1 v2 ()
decolor (DirEdge v1 v2 _) = DirEdge v1 v2 ()

-- | Return true, when the given edge contains a vertex, otherwise return False.
-- Specifically, this function only returns True for directed edges, when the
-- first vertex of the edge is the given edge.
edgeContainsVertex :: Eq v => Edge v e -> v -> Bool
edgeContainsVertex (UndirEdge v w _) x
  = v == x || w == x
edgeContainsVertex (DirEdge v _ _) w
  = v == w

instance (Eq v, Eq e) => Eq (Edge v e) where
  (DirEdge a b e) == (DirEdge c d f)
    = a == c && b == d && e == f
  (UndirEdge a b e) == (UndirEdge c d f)
    = DirEdge a b e == DirEdge c d f || DirEdge a b e == DirEdge d c f
  _ == _ = False

instance (Eq v, Ord e) => Ord (Edge v e) where
  e <= v = color e <= color v

-- | The Graph typeclass provides basic functions defined on graphs. Each graph
-- data type has to implement an instance of this Graph with at least the
-- functions 'empty', 'edges', and 'fromEdges'
class Graph gr where
  {-# MINIMAL empty, edges, fromEdges #-}
  -- | Empty graph
  empty :: gr v e
  -- | Return list of edges
  edges :: gr v e -> [Edge v e]
  -- | Convert a list of edges with equality comparable vertices to a graph
  fromEdges :: Eq v => [Edge v e] -> gr v e
  -- | Collect a list of vertices of a graph. This function may return
  -- duplicate vertices.
  vertices :: gr v e -> [v]
  vertices gr
    = concatMap edgeToList $ edges gr
  -- | Check if two vertices are adjacent in a graph
  adjacent :: (Eq v) => gr v e -> v -> v -> Bool
  adjacent gr a b
    = DirEdge a b () `elem` e || UndirEdge a b () `elem` e
    where
      e = decolor <$> edges gr
  -- | Find all neighbors of an vertex. This may return duplicate vertices.
  neighbors :: (Eq v) => gr v e -> v -> [v]
  neighbors gr a
    = filter (adjacent gr a) $ vertices gr

-- | The GraphFunctor type class is comparable to the simpler Functor type class
-- and provides map functions over graphs.
--
-- Instances of `GraphFunctor` should satisfy the following laws, which are
-- similar to the `Functor` laws:
--
-- > vemap id == id
-- > vmap  id == id
-- > emap  id == id
-- > vemap (f . g) == vemap f . vemap g
-- > vmap  (f . g) == vmap  f . vmap  g
-- > emap  (f . g) == emap  f . emap  g
class Graph gr => GraphFunctor gr where
  {-# MINIMAL vemap #-}
  -- | Map a function over the edges in a graph.
  vemap :: (Edge v e -> Edge w f) -> gr v e -> gr w f
  -- vemap f g = fromEdges $ f <$> edges g
  -- | Map a function over the vertices in a graph.
  vmap :: (v -> w) -> gr v e -> gr w e
  vmap f = vemap f'
    where
      f' (UndirEdge l r e) = UndirEdge (f l) (f r) e
      f' (DirEdge l r e) = DirEdge (f l) (f r) e
  -- | Map a function over the edge values in a graph.
  emap :: (e -> f) -> gr v e -> gr v f
  emap f = vemap f'
    where
      f' (UndirEdge l r e) = UndirEdge l r (f e)
      f' (DirEdge l r e) = DirEdge l r (f e)

infixl 6 <+>
-- | Join all edges of two graph into a new graph
(<+>) :: (Eq v, Graph gr) => gr v e -> gr v e -> gr v e
x <+> y = fromEdges $ edges x ++ edges y

infixl 6 <->
-- | Remove all edges of the second graph from the first graph
(<->) :: (Eq v, Eq e, Graph gr) => gr v e -> gr v e -> gr v e
x <-> y = fromEdges $ edges x \\ edges y

-- | Find all outgoing edges from a given vertex
outgoing :: (Graph gr, Eq v) => gr v e -> v -> [Edge v e]
outgoing gr v
  = filter (`isSource` v) $ edges gr

-- | Find all incoming edges to a given vertex
incoming :: (Graph gr, Eq v) => gr v e -> v -> [Edge v e]
incoming gr v
  = filter (`isDestination` v) $ edges gr

-- | Type of the graph, specifically whether the graph is `Directed`,
-- `Undirected`, `Mixed`, or `Empty`.
data GraphType
  = Directed | Undirected | Mixed | Empty
  deriving (Show, Read, Eq)

-- | Classify a given graph in terms of a `GraphType`
graphType
  :: Graph gr => gr v e -> GraphType
graphType gr
  = case (hasDir, hasUndir) of
    (True , True ) -> Mixed
    (True , False) -> Directed
    (False, True ) -> Undirected
    (False, False) -> Empty
  where
    hasDir = any isDir $ edges gr
    hasUndir = any isUndir $ edges gr

-- | Decide whether the graph has no edges and is considered empty
isEmpty :: Graph gr => gr v e -> Bool
isEmpty = (== Empty) . graphType

-- | Return @True@ when the given graph has the `Directed` `GraphType`.
isDirected :: Graph gr => gr v e -> Bool
isDirected = (== Directed) . graphType

-- | Return @True@ when the given graph has the `Undirected` `GraphType`.
isUndirected :: Graph gr => gr v e -> Bool
isUndirected = (== Undirected) . graphType

-- | Return @True@ when the given graph has the `Mixed` `GraphType`.
isMixed :: Graph gr => gr v e -> Bool
isMixed = (== Mixed) . graphType

-- | @Gr v e@ is a graph implemented by an adjacency list, where @v@ is the type
-- of the vertices and @e@ is the type of the edge values.
newtype Gr v e
  = Gr [Edge v e]
  deriving (Show, Read)

-- | Similar to `fromEdges`, but also accepts vertices without `Eq` instance.
mkGr :: [Edge v e] -> Gr v e
mkGr = Gr

instance (Eq v, Eq e) => Eq (Gr v e) where
  (Gr v) == (Gr w) = v == w

instance Graph Gr where
  -- | O(1)
  empty = Gr []
  -- | O(1)
  edges (Gr x) = x
  -- | O(1)
  fromEdges = Gr

instance GraphFunctor Gr where
  vemap f g = Gr $ f <$> edges g

instance Bifunctor Gr where
  first = vmap
  second = emap

instance Monoid (Gr v e) where
  mempty = empty
  Gr x `mappend` Gr y = Gr $ x ++ y

-- | Mark each edge with its topological weight
tweight :: (Eq v, GraphFunctor gr) => gr v e -> gr (v, Int) e
tweight g = vmap (\v -> (v, length $ n v)) g
  where
    n v = filter (/= v) $ neighbors g v

-- | Color each vertex with its distance to a given vertex. This will color the
-- given vertex with @Just mempty@, while the other vertices will get colored
-- with @Nothing@.
distanceColor
  :: (GraphFunctor gr, Eq v, Monoid e)
  => gr v e -> v -> gr (v, Maybe e) e
distanceColor gr v
  = let f v' = (v', if v' == v then Just mempty else Nothing)
    in vmap f gr

-- | Weight the next generation of vertices in a graph. This function is
-- part of the fix point process.
dijkstraS
  :: (GraphFunctor gr, Eq v, Monoid e, Ord e)
  => gr (v, Maybe e) e -> gr (v, Maybe e) e
dijkstraS g = vmap (weight g) g
  where
    -- | Calculate costs for a given edge when possible, otherwise return
    -- @Nothing@.
    --
    -- The `cost` function has a time complexity of O(1)
    cost
      :: (Monoid e, Ord e)
      => Edge (v, Maybe e) e -> Maybe e
    cost e = (<> color e) <$> best (snd <$> edgeToList e)
    -- | Return best @Just@ value of a list when possible, otherwise return
    -- @Nothing@.
    --
    -- The `best` function has a time complexity of O(n)
    best
      :: Ord e
      => [Maybe e] -> Maybe e
    best l = case filter isJust l of
      [] -> Nothing
      xs -> minimum xs
    -- | Weight a given vertex with the best possible weight. This is part of
    -- the second step of the Dijkstra algorithm.
    --
    -- The weight function has a time complexity of @O(n + T[outgoing])@
    weight
      :: (GraphFunctor gr, Eq v, Monoid e, Ord e)
      => gr (v, Maybe e) e -> (v, Maybe e) -> (v, Maybe e)
    weight gr t@(v, w)
      = (,) v (best . (w:) $ cost <$> outgoing gr t)

-- | Convert distance-colored graph to path from destination to source
dijkstraP
  :: (GraphFunctor gr, Monoid e, Eq v, Ord e)
  => gr (v, Maybe e) e -> v -> [v]
dijkstraP gr dst
  = dst : case prev of
    Nothing -> []
    Just p  -> dijkstraP gr p
  where
    dst'@(_, dst'w) = head $ filter ((== dst) . fst) (vertices gr)
    gr' = vemap (dirEdgeBy (compare `on` snd)) gr
    -- Previous vertex
    prev = case filter f $ outgoing gr' dst' of
      [] -> Nothing
      xs -> Just $ fst.rightVertex $ minimumBy (compare `on` (snd . rightVertex)) xs
    -- Filter function
    f (DirEdge _ r c) = case snd r of
      Nothing -> False
      Just r' -> Just (r' <> c) <= dst'w
    f u = f $ dirEdgeBy (compare `on` snd) u

-- | Calculate graph with associated absolute distances on vertices to a given
-- vertex. This is the core of the Dijkstra algorithm.
dijkstraG
  :: (GraphFunctor gr, Eq v, Monoid e, Ord e)
  => gr v e -> v -> gr (v, Maybe e) e
dijkstraG g s
  = let
      i gr
        | edges gr == edges ap' = gr -- Terminate when fix point reached
        | otherwise = i ap' -- Apply and repeat
        where
          ap' = dijkstraS gr
      g' = distanceColor (vemap ereverse g) s -- Prepare graph
    in i g'

-- | Perform shortest-path search in a graph from a source to a destination and
-- return the path as list
dijkstra
  :: (GraphFunctor gr, Num e, Eq v, Ord e)
  => gr v e -> v -> v -> [v]
dijkstra gr src dst
  = reverse $ dijkstraP (dijkstraG gr' src) dst
  where
    gr' = emap Sum gr

-- | Edge constructor for outgoing directed edges
--
-- > eOutgoing v e w = w ~~> v % e
eOutgoing :: v -> e -> v -> Edge v e
eOutgoing v e w = w ~~> v % e

-- | Edge constructor for incoming directed edges
--
-- > eIncoming v e w = v ~~> w % e
eIncoming :: v -> e -> v -> Edge v e
eIncoming v e w = v ~~> w % e

-- | Edge constructor for undirected edges
--
-- > eTouching v e w = v <~> w % e
eTouching :: v -> e -> v -> Edge v e
eTouching v e w = v <~> w % e

-- | Edge constructor for loop edges
--
-- >>> eLoop e v
-- v <~> v % e
eLoop :: e -> v -> Edge v e
eLoop e v = v <~> v % e
