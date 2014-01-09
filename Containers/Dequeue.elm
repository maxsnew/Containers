module Containers.Dequeue (empty, isEmpty, fromList, toList,
                           cons, snoc, head, last, tail, init, viewLeft, viewRight)
       where

{-| An implementation of a double-ended queue based on an exercise from Chris Okasaki's "Purely Functional Data Structures" -}

import List
import Native.Error

-- | Dequeue invariant: each list is non-empty when the other list has >= 2 elements
data Dequeue a = DQ [a] [a]

empty : Dequeue a
empty = DQ [] []

isEmpty : Dequeue a -> Bool
isEmpty (DQ f r) = List.isEmpty f && List.isEmpty r

fromList : [a] -> Dequeue a
fromList xs = fixUp xs []

toList : Dequeue a -> [a]
toList (DQ f r) = f ++ List.reverse r

{-| Unlike lists, this is an O(1) operation -}
reverse : Dequeue a -> Dequeue a
reverse (DQ f r) = DQ r f

cons : a -> Dequeue a -> Dequeue a
cons x (DQ f r) = fixUp (x :: f) r

snoc : a -> Dequeue a -> Dequeue a
snoc x (DQ f r) = fixUp f (x :: r)

head : Dequeue a -> a
head (DQ f r) = case (f, r) of
  ([], []) -> Native.Error.raise "Can't take the head of an empty Dequeue"
  ([], [x]) -> x
  (x::_, _) -> x
  
last : Dequeue a -> a
last = head . reverse

tail : Dequeue a -> Dequeue a
tail (DQ f r) = case (f, r) of
  ([], []) -> Native.Error.raise "Can't take the tail of an empty Dequeue"
  ([], [_]) -> empty
  (_::f', r') -> fixUp f' r'
               
init : Dequeue a -> Dequeue a
init (DQ f r) = case (f, r) of
  ([], []) -> Native.Error.raise "Can't take the init of an empty Dequeue"
  ([_], []) -> empty
  (f', _::r') -> fixUp f' r'

viewLeft  : Dequeue a -> Maybe (a, Dequeue a)
viewLeft (DQ f r) = case (f, r) of
  ([], []) -> Nothing
  ([], [x]) -> Just (x, empty)
  (x::f', r') -> Just (x, fixUp f' r')

viewRight : Dequeue a -> Maybe (a, Dequeue a)
viewRight (DQ f r) = case (f, r) of
  ([], []) -> Nothing
  ([x], []) -> Just (x, empty)
  (f', x::r') -> Just (x, fixUp f' r')

fixUp : [a] -> [a] -> Dequeue a
fixUp f r = case (f, r) of
  ([], _::_::_) -> let (r', f') = revSplit r
                   in DQ f' r'
  (_::_::_, []) -> let (f', r') = revSplit f
                   in DQ f' r'
  _ -> DQ f r

-- split a list in half and reverse the second half
revSplit : [a] -> ([a], [a])
revSplit xs = let len = List.length xs
                  half = len `div` 2
                  f = List.take half xs
                  r = List.drop half xs
              in (f, List.reverse r)
