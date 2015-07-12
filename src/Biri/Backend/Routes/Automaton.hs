{-# LANGUAGE OverloadedStrings #-}

module Biri.Backend.Routes.Automaton
( Automaton(..)
, transform
) where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Arrow (first, second, (&&&))
import Control.Exception (assert)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (foldl', nub, partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import Biri.Language

-- | Transforms a list of resources into an automaton.
--
-- The overall structure is as follows:
-- 1. The resources a grouped by common prefixes, which yields
--    a tree representing the branching points.
--    This ensures that each segment is compiled once.
-- 2. The tree is then converted into an automaton.
-- 3. The automaton is then turned into a deterministic automaton.
transform :: [Resource] -> Automaton
transform = determinize . fromTree . mkTree

-- | A representation of an automaton.
-- 
-- The set of states of an automaton with @n@ states equals {0,...,n-1}.
data Automaton = Automaton
               { states :: Int
               , delta :: V.Vector (Map Char [Int])
               , matching :: IntSet
               , handlers :: IntMap [Handler]
               } deriving (Show, Eq, Ord)

-- | Returns true iff the given state is a matching state.
isMatching :: Int -> Automaton -> Bool
isMatching k = IS.member k . matching

-- | Returns the list of handlers associated with given state.
getHandlers :: Int -> Automaton -> [Handler]
getHandlers k = IM.findWithDefault [] k . handlers

-- | A tree represents a branching point.
data Tree = Tree [(Automaton, Tree)]
          deriving (Show, Eq, Ord)

-- | Compiles a segment into an automaton.
--
-- Currently, the following patterns can be compiled: Int, String.
mkAutomaton :: Segment -> Automaton
mkAutomaton segment = case segment of
  Fixed str -> Automaton
               { states = T.length str + 1
               , delta = V.fromList (zipWith M.singleton (T.unpack str) (map (:[]) [1..]) ++ [M.empty])
               , matching = IS.singleton (T.length str)
               , handlers = IM.empty
               }
  Pattern (Constructor typ) -> case typ of
      "Int" -> Automaton
               { states = 3
               , delta = V.fromList
                       [ M.insert '0' [1] (M.fromList $ zip ['1'..'9'] (repeat [2]))
                       , M.empty
                       , M.fromList $ zip ['0'..'9'] (repeat [2])
                       ]
               , matching = IS.fromList [1,2]
               , handlers = IM.empty
               }
      "String" -> Automaton
                  { states = 1
                  , delta = V.singleton . M.delete '/' . M.fromList $ zip (take 128 ['\0'..]) (repeat [0])
                  , matching = IS.singleton 0
                  , handlers = IM.empty
                  }

-- | @concats [a1, ..., an]@ represents the concatenation of the automata @a1, ..., an@.
concats :: [Automaton] -> Automaton
concats auts
    | null auts = error "Biri.Routes.Automaton.concats: empty list"
    | null (tail auts) = head auts
    | otherwise = Automaton
                { states = last offsets
                , delta = V.concat (zipWith cross auts offsets)
                , matching = let finalAut = last auts
                             in  IS.map (subtract (states finalAut) . (+ last offsets)) (matching finalAut)
                , handlers = IM.unions (zipWith (IM.mapKeys . (+)) offsets (map handlers auts))
                }
  where
    offsets :: [Int]
    offsets = scanl (+) 0 (map states auts)

    cross :: Automaton -> Int -> V.Vector (Map Char [Int])
    cross aut offset = V.imap (\i -> epsilon i . M.map (map (+ offset))) (delta aut)
      where
        epsilon :: Int -> Map Char [Int] -> Map Char [Int]
        epsilon i | offset + states aut >= last offsets = id
                  | IS.member i (matching aut) = M.insertWith (++) '\0' [offset + states aut]
                  | otherwise = id

-- | @unions [a1, ..., an]@ represents the union of the automata @a1, ..., an@.
unions :: [Automaton] -> Automaton
unions auts
    | null auts = error "Biri.Routes.Automaton.unions: empty list"
    | null (tail auts) = head auts
    | otherwise = Automaton
                { states = last offsets
                , delta = V.concat (bifurcation : deltas)
                , matching = IS.unions (zipWith (IS.map . (+)) offsets (map matching auts))
                , handlers = IM.unions (zipWith (IM.mapKeys . (+)) offsets (map handlers auts))
                }
  where
    offsets :: [Int]
    offsets = scanl (+) 1 (map states auts)
    
    bifurcation :: V.Vector (Map Char [Int])
    bifurcation = V.singleton (M.singleton '\0' (init offsets))
    
    deltas :: [V.Vector (Map Char [Int])]
    deltas = zipWith (V.map . M.map . map . (+)) offsets (map delta auts)

-- | Turns a list of resources into a tree by grouping common prefixes.
mkTree :: [Resource] -> Tree
mkTree = uncurry root . first (concatMap (\(Resource _ hndls) -> hndls))
       . partition (\(Resource (URI segs) _) -> null segs)
  where
    root :: [Handler] -> [Resource] -> Tree
    root hndls = Tree . (:[]) . (,) (install (mkAutomaton (Fixed "/")) hndls) . go
    
    go :: [Resource] -> Tree
    go = Tree . map (\(seg, (hndls, rs)) -> (install (mkAutomaton seg) hndls, go rs))
       . M.toList . subsume
    
    install :: Automaton -> [Handler] -> Automaton
    install aut hndls
        | null hndls = aut
        | otherwise = aut { handlers = IM.fromAscList . map (flip (,) hndls) $ IS.toAscList (matching aut) }
    
    subsume :: [Resource] -> Map Segment ([Handler], [Resource])
    subsume = foldl (\m -> M.insertWith merge <$> key <*> value <*> pure m) M.empty
      where
        merge :: ([a],[b]) -> ([a],[b]) -> ([a],[b])
        merge (xs1, ys1) (xs2, ys2) = (xs1 ++ xs2, ys1 ++ ys2)
        
        key :: Resource -> Segment
        key (Resource (URI segs) _)
            | null segs = error "Biri.Routes.Automaton.mkTree: empty list"
            | otherwise = head segs
        
        value :: Resource -> ([Handler], [Resource])
        value (Resource (URI segs) hndls)
            | null segs = error "Biri.Routes.Automaton.mkTree: empty list"
            | tail segs == [] = (hndls, [])
            | otherwise = ([], [Resource (URI (tail segs)) hndls])

-- | Unfolds the branching structure the resources.
fromTree :: Tree -> Automaton
fromTree (Tree rs) = case rs of
    [(aut, Tree list)] -> concats [aut, unions (map (uncurry go) list)]
    _                  -> undefined
  where
    go :: Automaton -> Tree -> Automaton
    go aut (Tree list) = case list of
        [] -> aut
        xs -> concats [aut, mkAutomaton (Fixed "/"), unions (map (uncurry go) xs)]

-- | Converts an automaton into a deterministic automaton.
determinize :: Automaton -> Automaton
determinize (Automaton { delta = delta, handlers = handlers }) = convert (go (closure (IS.singleton 0)) M.empty)
  where
    convert :: Map IntSet (Map Char IntSet) -> Automaton
    convert deltas = Automaton
                   { states = M.size deltas
                   , delta = V.fromList . map (M.map (return . toState) . snd) $ M.toAscList deltas
                   , matching = IM.keysSet hndls
                   , handlers = hndls
                   }
      where
        hndls :: IntMap [Handler]
        hndls = IM.fromAscList . filter (not . null . snd) . map (toState &&& collectHandlers) $ M.keys deltas
        
        toState :: IntSet -> Int
        toState = (M.!) (M.fromAscList $ zip (map fst (M.toAscList deltas)) [0..])
        
        collectHandlers :: IntSet -> [Handler]
        collectHandlers = concatMap (flip (IM.findWithDefault []) handlers) . IS.toList
    
    go :: IntSet -> Map IntSet (Map Char IntSet) -> Map IntSet (Map Char IntSet)
    go state deltas = foldl' (flip traverse) (M.insert state table deltas) (nub . map snd $ M.toList table)
      where
        traverse :: IntSet -> Map IntSet (Map Char IntSet) -> Map IntSet (Map Char IntSet)
        traverse state' deltas' | M.notMember state' deltas' = go state' deltas'
                                | otherwise = deltas'
        
        table :: Map Char IntSet
        table = M.fromList (filter (not . IS.null . snd) $ zip chars (map (flip step state) chars))
          where
            chars :: [Char]
            chars = tail (take 128 ['\0'..])
        
        step :: Char -> IntSet -> IntSet
        step c = closure . IS.fromList . concatMap (M.findWithDefault [] c . (V.!) delta) . IS.toList
    
    closure :: IntSet -> IntSet
    closure = go <*> IS.toList
      where
        go :: IntSet -> [Int] -> IntSet
        go state list = case list of
            []   -> state
            x:xs -> let new = filter (flip IS.notMember state) (M.findWithDefault [] '\0' (delta V.! x))
                    in  go (IS.union state (IS.fromList new)) (xs ++ new)
