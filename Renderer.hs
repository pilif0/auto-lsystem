-- Filip Smola
-- Automatic LSystem renderer

module Renderer (Key, construct, makeKeyEntries, rotations, branches) where

import LSystem

-- The key type
type Key = [(Char, String, Command)]

-- Constructs the command sequence for the LSystem at the given iteration
-- params: iterations, base state, key
construct :: Int -> String -> Key -> Command
construct n st key = translate key (iterate' n key st)

-- Translates the generated string to a sequence of commands
-- params: key, string to translate
translate :: Key -> String -> Command
translate key [] = Sit
translate key (x:xs) | x == '[' = (Branch (translate key (take (n - 1) xs)))
                                      :#: translate key (drop n xs)
                      | otherwise = findComm x :#: translate key xs
    where
      findComm c = the [com | (a, _, com) <- key, a == c]
      the [x] = x
      n = untilMatchingBr 1 0 xs

-- Finds the number of characters until the matching closing bracket
-- params: current level, current position, remaining string
-- error: when there is no matching bracket in the remaining string
untilMatchingBr :: Int -> Int -> String -> Int
untilMatchingBr 0 c _ = c
untilMatchingBr _ _ [] = error "no matching bracket"
untilMatchingBr l c (']':xs) = untilMatchingBr (l-1) (c+1) xs
untilMatchingBr l c ('[':xs) = untilMatchingBr (l+1) (c+1) xs
untilMatchingBr l c (x:xs) = untilMatchingBr l (c+1) xs

-- Iterates over the string using the key, replacing each character with its
--   associated next evolution
-- params: iterations, key, string to iterate over
iterate' :: Int -> Key -> String -> String
iterate' 0 _ s = s
iterate' n k s = iterate' (n - 1) k (help s)
    where
      help s = concat [find c k | c <- s]
      find c k = the [s | (a, s, _) <- k, a == c]
      the [x] = x

-- Additional (frequent) key entries

-- Constructs key entries for the clockwise and anticlockwise rotation by an
--   angle under the '+' and '-' characters respectively
rotations :: Angle -> Key
rotations a = makeKeyEntry '+' "+" (Turn (-1 * a)) : [makeKeyEntry '-' "-" (Turn a)]

-- Makes the '[' and ']' characters not do anything
branches :: Key
branches = makeKeyEntry '[' "[" Sit : [makeKeyEntry ']' "]" Sit]

-- Joins the three components of a key entry into a single key entry
makeKeyEntry :: Char -> String -> Command -> (Char, String, Command)
makeKeyEntry ch s co = (ch, s, co)

-- Joins the three lists of components of a key into a key
makeKeyEntries :: [Char] -> [String] -> [Command] -> Key
makeKeyEntries = zipWith3 makeKeyEntry
