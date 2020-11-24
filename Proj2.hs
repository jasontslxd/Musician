-- Author: Shu Lok Tsang <tsangs2@student.unimelb.edu.au>
-- Purpose: Simulate the game of musician
--
-- The aim of this game is to guess the chord chosen by the composer with the
-- least amount of guesses. A chord is made up of three pitches and a pitch is 
-- made up of a note and an octave. After every guess, the composer will give a 
-- feedback on the number of correct pitches, notes and octaves. Correct pitches
-- are not counted towards the corret notes and octaves. The player can use this
-- information to guess the next chord. The game ends when three correct pitches 
-- are given in the feedback.

module Proj2 where
              
import Data.List
import Data.Maybe

-- Defining different data types and show functions for this game

-- GameState stores all possible chords that havent been ruled out by feedback
type GameState = [[Pitch]]

-- Valid notes
data Note = A | B | C | D | E | F | G
    deriving (Show, Eq, Ord)
    
-- Valid octaves
data Octave = One | Two | Three
    deriving (Eq, Ord)
instance Show Octave where show = showOctave
    
-- String representation of an octave
showOctave :: Octave -> String
showOctave One = "1"
showOctave Two = "2"
showOctave Three = "3"

-- Valid pithes
data Pitch = Pitch Note Octave
    deriving (Eq, Ord)
instance Show Pitch where show = showPitch
    
-- String representation of a pitch
showPitch :: Pitch -> String
showPitch (Pitch note octave) = show note ++ show octave

-- Valid charaters to represent a note
parseNote :: Char -> Maybe Note
parseNote 'A' = Just A
parseNote 'B' = Just B
parseNote 'C' = Just C
parseNote 'D' = Just D
parseNote 'E' = Just E
parseNote 'F' = Just F
parseNote 'G' = Just G
parseNote _ = Nothing

-- Valid characters to represent a pitch
parseOctave :: Char -> Maybe Octave
parseOctave '1' = Just One
parseOctave '2' = Just Two
parseOctave '3' = Just Three
parseOctave _ = Nothing
    
-- Converts a string respresentation of a pitch into the Pitch type if the pitch
-- given is valid. Returns Nothing if the pitch is not valid.
toPitch :: String -> Maybe Pitch
toPitch (x:y:rest)
    | parseNote x == Nothing || parseOctave y == Nothing = Nothing
    | rest == "" = Just (Pitch note octave)
    | otherwise = Nothing
        where Just note = parseNote x
              Just octave = parseOctave y
              
-- Returns the feedback as specified in the project description given a target
-- chord and a guess chord. Correct pitches in the chord are not used in finding
-- correct notes and correct octaves.
feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
feedback targets guesses = (length cg, length cn, length co)
    where cg = findCorrectStuff containsPitch samePitch targets guesses
          remTargets = targets \\ cg
          remGuesses = guesses \\ cg
          cn = findCorrectStuff containsNote sameNote remTargets remGuesses
          co = findCorrectStuff containsOctave sameOctave remTargets remGuesses
          
-- Higher order function to generalise the procedure to find the correct parts
-- of the guess given a target. Takes a function that returns whether a guess
-- contains the correct parts of the target, and a function that finds instances
-- which matched the correct part, then uses them to find the pitches in the
-- guess that matched.
findCorrectStuff :: (Pitch -> [Pitch] -> Bool) -> (Pitch -> [Pitch] -> Pitch) 
                    -> [Pitch] -> [Pitch] -> [Pitch]
findCorrectStuff _ _ _ [] = []
findCorrectStuff contains same targets (guess:guesses)
    | contains guess targets = guess : 
        findCorrectStuff contains same (targets \\ [same guess targets]) guesses
    | otherwise = findCorrectStuff contains same targets guesses

-- Checks if the pitch is contained in a list of pitches.
containsPitch :: Pitch -> [Pitch] -> Bool
containsPitch pitch list = not (length (filter (== pitch) list) == 0)

-- Finds a pitch in a list of pitches with the same pitch given. It is in this
-- format to fit into the findCorrectStuff function.
samePitch :: Pitch -> [Pitch] -> Pitch
samePitch pitch1 _ = pitch1

-- Checks if the pitch given has the same octave of some other pitch in a list
-- of pitches.
containsOctave :: Pitch -> [Pitch] -> Bool
containsOctave _ [] = False
containsOctave (Pitch note oct) ((Pitch _ oct2):rest)
    | oct == oct2 = True
    | otherwise = containsOctave (Pitch note oct) rest
    
-- Finds a pitch in a list of pitches with the same octave. Dont need base case 
-- as this is used only when there is a same octave.
sameOctave :: Pitch -> [Pitch] -> Pitch
sameOctave (Pitch note1 oct1) ((Pitch note2 oct2):rest)
    | oct1 == oct2 = Pitch note2 oct2
    | otherwise = sameOctave (Pitch note1 oct1) rest
    
-- Checks if the pitch given has the same note of some other pitch in a list
-- of pitches.
containsNote :: Pitch -> [Pitch] -> Bool
containsNote _ [] = False
containsNote (Pitch note oct) ((Pitch note2 _):rest)
    | note == note2 = True
    | otherwise = containsNote (Pitch note oct) rest

-- Finds a pitch in a list of pitches with the same note. Dont need base case 
-- as this is used only when there is a same note.
sameNote :: Pitch -> [Pitch] -> Pitch
sameNote (Pitch note1 oct1) ((Pitch note2 oct2):rest)
    | note1 == note2 = Pitch note2 oct2
    | otherwise = sameNote (Pitch note1 oct1) rest
    
-- Start with random guess A1 B2 C3 and initial game state with all possible 
-- chords. Generates subsequences by using the function defined in Data.List.
-- This initial guess is chosen since it covers all octaves and three possible
-- notes, which would have the highest chance of giving a feedback that is not
-- (0,0,0) to speed up guesses.
initialGuess :: ([Pitch],GameState)
initialGuess = ([Pitch A One, Pitch B Two, Pitch C Three], 
                 filter ((== 3).length) (subsequences pitches))
    where pitches = [Pitch note octave | note <- [A,B,C,D,E,F,G], 
                                         octave <- [One, Two, Three]]

-- Generates a next guess based on the gamestate and the feedback given for the 
-- guess. The next guess is chosen so that the guess would generate the smallest
-- expected number of remaining possible targets.
nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess (prevGuess, prevState) (cg, cn, co) = (bestTarget possible, possible)
    where possible = possibleTargets (prevGuess, prevState) (cg, cn, co)

-- Finds all possible targets from the previous gamestate. A possible target is 
-- a chord that would generate the same feedback from the previous guess.
possibleTargets :: ([Pitch],GameState) -> (Int,Int,Int) -> GameState
possibleTargets (prevGuess, prevState) (cg, cn, co)
    = filter ((== (cg, cn, co)).(feedback prevGuess)) prevState
    
-- Finds the best target to guess given the gamestate. The best target is the 
-- target that would generate the smallest expected number of remaining possible
-- targets if we guessed that target. This is done by finding the feedback for
-- all chords left in the gamestate for all possible targets. Then we group the
-- feedback and count the number of each unique feedback score. Then we get the
-- expected number of chords left by assuming that the probability of each
-- chord being the target is the same. Then we choose the target with the lowest
-- expected number of targets remaining.
bestTarget :: GameState -> [Pitch]
bestTarget state = state !! bestChoice
    where bestChoice = (minIndex.expNumOfFeedback.groupNCountFeedback.
                        mapFeedback) state

-- Maps all the feedback scores for each possible target in the gamestate to all
-- other possible targets.
mapFeedback :: GameState -> [[(Int, Int, Int)]]
mapFeedback [] = []
mapFeedback (target:targets)
    = [map (feedback target) (target:targets)] ++ mapFeedback targets

-- Sorts the feedback of the possible targets and group and count the number of 
-- same feedbacks returned.
groupNCountFeedback :: [[(Int, Int, Int)]] -> [[((Int, Int, Int), Int)]]
groupNCountFeedback feedbackList
    = map ((map getFeedbackCount).groupBy (==).sort) feedbackList

-- Gets the feedback value and number of each unique feedback. The input to this
-- function is a list of some number of the same feedback.
getFeedbackCount :: [(Int, Int, Int)] -> ((Int, Int, Int), Int)
getFeedbackCount list = (head list,length list)

-- Finds the expected number of targets for each possible target in the
-- gamestate.
expNumOfFeedback :: [[((Int, Int, Int),Int)]] -> [Float]
expNumOfFeedback list = map getExpectedValue list

-- Finds the expected value of remaining targets for a possible guess. The
-- expected value of a guess G = (N1 * N1 + N2 * N2 + ...) / M where N1 is the
-- number of targets that give a certain feedback and M is the total number of
-- possible targets.
getExpectedValue :: [((Int, Int, Int),Int)] -> Float
getExpectedValue [] = 0
getExpectedValue ((feedback,count):rest) = 
    (fromIntegral (count*count) + (getExpectedValue rest)) / (fromIntegral len)
        where len = getFeedbackLen ((feedback,count):rest)

-- Gets the total number of possible targets remaining. Probably didnt need to
-- do it this way but im already in too deep to change the rest of the code.
getFeedbackLen :: [((Int, Int, Int),Int)] -> Int
getFeedbackLen [] = 0
getFeedbackLen (((_),count):rest) = count + getFeedbackLen rest

-- Finds the index of the minimum element in the list. Used to find the minimum
-- element of the list of expected value.
minIndex :: Ord a => [a] -> Int
minIndex list = head (filter ((== minimum list).(list !!)) [0..length list])

-- Test shit below

{- guessTest :: [Pitch] -> Int
guessTest target = do
      let (guess,other) = initialGuess
      loop target guess other 1
      
loop :: [Pitch] -> [Pitch] -> GameState -> Int -> Int
loop target guess other guesses = do
  let answer = feedback target guess
  if answer == (3,0,0)
    then do
      guesses
    else do
      let (guess',other') = nextGuess (guess,other) answer
      loop target guess' other' (guesses+1)
      
toChord :: String -> [Pitch]
toChord = (fromJust . mapM toPitch . words)

main1 :: [Pitch] -> Int
main1 target = do
  guessTest $ target
  
test :: ([Pitch],GameState) -> [Int]
test (start,xs) = map main1 xs

main :: IO ()
main = do
  putStrLn $ "Average guesses: " ++ show avg 
    where list = test initialGuess
          total = sum list
          len = length list
          avg = (fromIntegral total) / (fromIntegral len) -}