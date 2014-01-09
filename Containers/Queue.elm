module Containers.Queue (empty, isEmpty, fromList, toList,
                         snoc, head, tail, view)
       where

import List

data Queue a = Q [a] [a]

empty : Queue a
empty = Q [] []

isEmpty : Queue a -> Bool
isEmpty (Q front _) = List.isEmpty front

fromList : [a] -> Queue a
fromList xs = Q xs []

toList : Queue a -> [a]
toList (Q front rear) = front ++ List.reverse rear

snoc : a -> Queue a -> Queue a
snoc x (Q front rear) = checkf (Q front (x :: rear))

head : Queue a -> a
head (Q xs _) = case xs of
  [] -> Native.Error.raise "Can't take the head of an empty Queue!"
  (x :: _) -> x

view : Queue a -> Maybe (a, Queue a)
view (Q front rear) = case front of 
  []        -> Nothing
  (x :: xs) -> Just (x, checkf <| Q xs rear)

tail : Queue a -> Queue a
tail q = case view q of
  Nothing -> Native.Error.raise "Can't take the tail of an empty Queue!"
  Just (_, xs) -> xs

remember : Signal a -> Signal (Queue a)
remember = foldp snoc empty

checkf : Queue a -> Queue a
checkf q = case q of
  Q front rear ->
    case front of
      [] -> Q (List.reverse rear) []
      _  -> q
