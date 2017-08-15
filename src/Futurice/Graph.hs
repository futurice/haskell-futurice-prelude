{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Futurice.Graph
-- Copyright   :  (c) Oleg Grenrus, Edward Z. Yang 2016
-- License     :  BSD3
--
-- This module is heavily based on "Distribution.Compat.Graph" from @Cabal@ package
-- licenses unser BSD-3-Clause license
--
-- A data type representing directed graphs, backed by "Data.Graph".
-- It is strict in the node type.
--
-- This is an alternative interface to "Data.Graph".  In this interface,
-- nodes (identified by the 'IsNode' type class) are associated with a
-- key and record the keys of their neighbors.  This interface is more
-- convenient than 'Data.Graph.Graph', which requires vertices to be
-- explicitly handled by integer indexes.
--
-- The current implementation has somewhat peculiar performance
-- characteristics.  The asymptotics of all map-like operations mirror
-- their counterparts in "Data.Map".  However, to perform a graph
-- operation, we first must build the "Data.Graph" representation, an
-- operation that takes /O(V + E log V)/.  However, this operation can
-- be amortized across all queries on that particular graph.
--
-- Some nodes may be broken, i.e., refer to neighbors which are not
-- stored in the graph.  In our graph algorithms, we transparently
-- ignore such edges; however, you can easily query for the broken
-- vertices of a graph using 'broken' (and should, e.g., to ensure that
-- a closure of a graph is well-formed.)  It's possible to take a closed
-- subset of a broken graph and get a well-formed graph.
--
-----------------------------------------------------------------------------

module Futurice.Graph (
    -- * Graph type
    Graph,
    HasKey (..),
    IsNode (..),
    -- * Query
    null,
    size,
    member,
    lookup,
    -- * Construction
    empty,
    insert,
    deleteKey,
    deleteLookup,
    -- * Combine
    unionLeft,
    unionRight,
    -- * Graph algorithms
    stronglyConnComp,
    SCC(..),
    cycles,
    broken,
    neighbors,
    revNeighbors,
    closure,
    revClosure,
    topSort,
    revTopSort,
    -- * Conversions
    -- ** Maps
    graphMapIso,
    fromIdMap,
    toMap,
    toIdMap,
    -- ** Lists
    fromList,
    toList,
    keys,
    -- ** Sets
    keysSet,
    -- ** Graphs
    toGraph,
    -- * Node type
    Node(..),
    nodeValue,
) where

import Prelude ()
import Futurice.Prelude hiding (empty, lookup, null, toList)
import Control.Lens     (At (..), Index, IxValue, Ixed (..))

import qualified Futurice.Prelude as P

import           Futurice.IdMap (HasKey (..), IdMap)
import qualified Futurice.IdMap as IdMap

import           Data.Array  ((!))
import qualified Data.Array  as Array
import           Data.Either (partitionEithers)
import           Data.Graph  (SCC (..))
import qualified Data.Graph  as G
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.Tree   as Tree

-- | A graph of nodes @a@.  The nodes are expected to have instance
-- of class 'IsNode'.
data Graph a = Graph
    { graphMap          :: !(IdMap a)
     -- Lazily cached graph representation
    , graphForward      :: G.Graph
    , graphAdjoint      :: G.Graph
    , graphVertexToNode :: G.Vertex -> a
    , graphKeyToVertex  :: Key a -> Maybe G.Vertex
    , graphBroken       :: [(a, [Key a])]
    }
  deriving (Typeable)

-- NB: Not a Functor! (or Traversable), because you need
-- to restrict Key a ~ Key b.  We provide our own mapping
-- functions.

-- General strategy is most operations are deferred to the
-- Map representation.

instance Show a => Show (Graph a) where
    show = show . P.toList

instance (IsNode a, Read a) => Read (Graph a) where
    readsPrec d s = map (\(a,r) -> (fromList a, r)) (readsPrec d s)

{-
instance (IsNode a, Binary a) => Binary (Graph a) where
    put x = put (toList x)
    get = fmap fromList get
-}

instance (Eq (Key a), Eq a) => Eq (Graph a) where
    g1 == g2 = graphMap g1 == graphMap g2

instance Foldable Graph where
    fold = fold . graphMap
    foldr f z = foldr f z . graphMap
    foldl f z = foldl f z . graphMap
    foldMap f = foldMap f . graphMap
    --foldl' f z = foldl' f z . graphMap
    --foldr' f z = foldr' f z . graphMap
    length = length . graphMap
    null   = P.null   . graphMap
    toList = P.toList . graphMap
    elem x = elem x . graphMap
    maximum = maximum . graphMap
    minimum = minimum . graphMap
    sum     = sum     . graphMap
    product = product . graphMap

instance (NFData a, NFData (Key a)) => NFData (Graph a) where
    rnf Graph {
        graphMap = m,
        graphForward = gf,
        graphAdjoint = ga,
        graphVertexToNode = vtn,
        graphKeyToVertex = ktv,
        graphBroken = b
    } = gf `seq` ga `seq` vtn `seq` ktv `seq` b `seq` rnf m

-- TODO: Data instance?

instance IsNode a => Semigroup (Graph a) where
    a <> b = fromMap (toMap a <> toMap b)

instance IsNode a => Monoid (Graph a) where
    mempty  = empty
    mappend = (<>)

type instance Index (Graph a)   = Key a
type instance IxValue (Graph a) = a

graphMapIso :: IsNode b => Iso (Graph a) (Graph b) (IdMap a) (IdMap b)
graphMapIso = iso toIdMap fromIdMap

instance IsNode a => Ixed (Graph a) where
    ix i = graphMapIso . ix i

instance IsNode a => At (Graph a) where
    at i = graphMapIso . at i

-- | The 'IsNode' class is used for datatypes which represent directed
-- graph nodes.  A node of type @a@ is associated with some unique key of
-- type @'Key' a@; given a node we can determine its key ('nodeKey')
-- and the keys of its neighbors ('nodeNeighbors').
class HasKey a => IsNode a where
    nodeNeighbors :: a -> [Key a]

nodeKey :: IsNode a => a -> Key a
nodeKey = view key

instance (IsNode a, IsNode b, Key a ~ Key b) => IsNode (Either a b) where
    nodeNeighbors (Left x)  = nodeNeighbors x
    nodeNeighbors (Right x) = nodeNeighbors x

-- | A simple, trivial data type which admits an 'IsNode' instance.
data Node k a = N a k [k]
    deriving (Show, Eq)

-- | Get the value from a 'Node'.
nodeValue :: Node k a -> a
nodeValue (N a _ _) = a

instance Functor (Node k) where
    fmap f (N a k ks) = N (f a) k ks

instance Ord k => HasKey (Node k a) where
    type Key (Node k a) = k
    key = lens g s
      where
        g (N _ k _)    = k
        s (N a _ ks) k = N a k ks

instance Ord k => IsNode (Node k a) where
    nodeNeighbors (N _ _ ks) = ks

-- Query

-- | /O(1)/. Is the graph empty?
null :: Graph a -> Bool
null = P.null . toIdMap

-- | /O(1)/. The number of nodes in the graph.
size :: Graph a -> Int
size = P.length . toIdMap

-- | /O(log V)/. Check if the key is in the graph.
member :: HasKey a => Key a -> Graph a -> Bool
member k g = Map.member k (toMap g)

-- | /O(log V)/. Lookup the node at a key in the graph.
lookup :: HasKey a => Key a -> Graph a -> Maybe a
lookup k g = Map.lookup k (toMap g)

-- Construction

-- | /O(1)/. The empty graph.
empty :: IsNode a => Graph a
empty = fromMap Map.empty

-- | /O(log V)/. Insert a node into a graph.
insert :: IsNode a => a -> Graph a -> Graph a
insert !n g = fromMap (Map.insert (nodeKey n) n (toMap g))

-- | /O(log V)/. Delete the node at a key from the graph.
deleteKey :: IsNode a => Key a -> Graph a -> Graph a
deleteKey k g = fromMap (Map.delete k (toMap g))

-- | /O(log V)/. Lookup and delete.  This function returns the deleted
-- value if it existed.
deleteLookup :: IsNode a => Key a -> Graph a -> (Maybe a, Graph a)
deleteLookup k g =
    let (r, m') = Map.updateLookupWithKey (\_ _ -> Nothing) k (toMap g)
    in (r, fromMap m')

-- Combining

-- | /O(V + V')/. Right-biased union, preferring entries
-- from the second map when conflicts occur.
-- @'nodeKey' x = 'nodeKey' (f x)@.
unionRight :: IsNode a => Graph a -> Graph a -> Graph a
unionRight g g' = fromMap (Map.union (toMap g') (toMap g))

-- | /O(V + V')/. Left-biased union, preferring entries from
-- the first map when conflicts occur.
unionLeft :: IsNode a => Graph a -> Graph a -> Graph a
unionLeft = flip unionRight

-- Graph-like operations

-- | /Ω(V + E)/. Compute the strongly connected components of a graph.
-- Requires amortized construction of graph.
stronglyConnComp :: Graph a -> [SCC a]
stronglyConnComp g = map decode forest
  where
    forest = G.scc (graphForward g)
    decode (Tree.Node v [])
        | mentions_itself v = CyclicSCC  [graphVertexToNode g v]
        | otherwise         = AcyclicSCC (graphVertexToNode g v)
    decode other = CyclicSCC (dec other [])
        where dec (Tree.Node v ts) vs
                = graphVertexToNode g v : foldr dec vs ts
    mentions_itself v = v `elem` (graphForward g ! v)
-- Implementation copied from 'stronglyConnCompR' in 'Data.Graph'.

-- | /Ω(V + E)/. Compute the cycles of a graph.
-- Requires amortized construction of graph.
cycles :: Graph a -> [[a]]
cycles g = [ vs | CyclicSCC vs <- stronglyConnComp g ]

-- | /O(1)/.  Return a list of nodes paired with their broken
-- neighbors (i.e., neighbor keys which are not in the graph).
-- Requires amortized construction of graph.
broken :: Graph a -> [(a, [Key a])]
broken g = graphBroken g

-- | Lookup the immediate neighbors from a key in the graph.
-- Requires amortized construction of graph.
neighbors :: Graph a -> Key a -> Maybe [a]
neighbors g k = do
    v <- graphKeyToVertex g k
    return (map (graphVertexToNode g) (graphForward g ! v))

-- | Lookup the immediate reverse neighbors from a key in the graph.
-- Requires amortized construction of graph.
revNeighbors :: Graph a -> Key a -> Maybe [a]
revNeighbors g k = do
    v <- graphKeyToVertex g k
    return (map (graphVertexToNode g) (graphAdjoint g ! v))

-- | Compute the subgraph which is the closure of some set of keys.
-- Returns @Nothing@ if one (or more) keys are not present in
-- the graph.
-- Requires amortized construction of graph.
closure :: Graph a -> [Key a] -> Maybe [a]
closure g ks = do
    vs <- traverse (graphKeyToVertex g) ks
    return (decodeVertexForest g (G.dfs (graphForward g) vs))

-- | Compute the reverse closure of a graph from some set
-- of keys.  Returns @Nothing@ if one (or more) keys are not present in
-- the graph.
-- Requires amortized construction of graph.
revClosure :: Graph a -> [Key a] -> Maybe [a]
revClosure g ks = do
    vs <- traverse (graphKeyToVertex g) ks
    return (decodeVertexForest g (G.dfs (graphAdjoint g) vs))

flattenForest :: Tree.Forest a -> [a]
flattenForest = concatMap Tree.flatten

decodeVertexForest :: Graph a -> Tree.Forest G.Vertex -> [a]
decodeVertexForest g = map (graphVertexToNode g) . flattenForest

-- | Topologically sort the nodes of a graph.
-- Requires amortized construction of graph.
topSort :: Graph a -> [a]
topSort g = map (graphVertexToNode g) $ G.topSort (graphForward g)

-- | Reverse topologically sort the nodes of a graph.
-- Requires amortized construction of graph.
revTopSort :: Graph a -> [a]
revTopSort g = map (graphVertexToNode g) $ G.topSort (graphAdjoint g)

-- Conversions

-- | /O(1)/. Convert an 'IdMap' into a graph.
fromIdMap :: IsNode a => IdMap a -> Graph a
fromIdMap m = fromMap (IdMap.toMap m)

-- | /O(1)/. Convert a map from keys to nodes into a graph.
-- The map must satisfy the invariant that
-- @'fromMap' m == 'fromList' ('Data.Map.elems' m)@;
-- if you can't fulfill this invariant use @'fromList' ('Data.Map.elems' m)@
-- instead.  The values of the map are assumed to already
-- be in WHNF.
fromMap :: IsNode a => Map (Key a) a -> Graph a
fromMap m = Graph
    { graphMap          = IdMap.unsafeFromMap m
    -- These are lazily computed!
    , graphForward      = g
    , graphAdjoint      = G.transposeG g
    , graphVertexToNode = vertex_to_node
    , graphKeyToVertex  = key_to_vertex
    , graphBroken       = broke
    }
  where
    try_key_to_vertex k = maybe (Left k) Right (key_to_vertex k)

    (brokenEdges, edges)
        = unzip
        $ [ partitionEithers (map try_key_to_vertex (nodeNeighbors n))
          | n <- ns ]
    broke = filter (not . P.null . snd) (zip ns brokenEdges)

    g = Array.listArray bounds edges

    ns              = Map.elems m -- sorted ascending
    vertices        = zip (map nodeKey ns) [0..]
    vertex_map      = Map.fromAscList vertices
    key_to_vertex k = Map.lookup k vertex_map

    vertex_to_node vertex = nodeTable ! vertex

    nodeTable   = Array.listArray bounds ns
    bounds = (0, Map.size m - 1)

-- | /O(V log V)/. Convert a list of nodes into a graph.
fromList :: IsNode a => [a] -> Graph a
fromList = fromIdMap . IdMap.fromFoldable

-- Map-like operations

-- | /O(V)/. Convert a graph into a list of nodes.
toList :: Graph a -> [a]
toList g = Map.elems (toMap g)

-- | /O(V)/. Convert a graph into a list of keys.
keys :: Graph a -> [Key a]
keys g = Map.keys (toMap g)

-- | /O(V)/. Convert a graph into a set of keys.
keysSet :: Graph a -> Set.Set (Key a)
keysSet g = Map.keysSet (toMap g)

-- | /O(1)/. Convert a graph into a map from keys to nodes.
-- The resulting map @m@ is guaranteed to have the property that
-- @'P.all' (\(k,n) -> k == 'nodeKey' n) ('Data.Map.toList' m)@.
toMap :: Graph a -> Map (Key a) a
toMap = IdMap.toMap . graphMap

toIdMap :: Graph a -> IdMap a
toIdMap = graphMap

-- Graph-like operations

-- | /O(1)/. Convert a graph into a 'Data.Graph.Graph'.
-- Requires amortized construction of graph.
toGraph :: Graph a -> (G.Graph, G.Vertex -> a, Key a -> Maybe G.Vertex)
toGraph g = (graphForward g, graphVertexToNode g, graphKeyToVertex g)
