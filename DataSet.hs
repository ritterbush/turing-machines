module Main where

import qualified Data.Set as Set
import Text.Read
import System.IO

data Cmd = L | M | R | S
    deriving (Eq, Show, Read)
type State = Int
type Cmds = [(Cmd, State, Cmd, State)]
type Track = Set.Set Int
type CartPos = Int

--type AtZero = (Cmd, State)
--type AtOne =  (Cmd, State)
--type Cmds2 = [(AtZero, AtOne)]

runCmd :: Bool -> Cmd -> CartPos -> Track -> (CartPos, Track)
runCmd atZero cmd cartPos track = if atZero
    then case cmd of
        L -> (cartPos - 1, track)
        M -> (cartPos - 1, Set.insert cartPos track)
        R -> (cartPos + 1, track)
        S -> (cartPos + 1, Set.insert cartPos track)
    else case cmd of 
        L -> (cartPos - 1, Set.delete cartPos track)
        M -> (cartPos - 1, track)
        R -> (cartPos + 1, Set.delete cartPos track)
        S -> (cartPos + 1, track)

runCmds:: Cmds -> State -> (CartPos,Track) -> IO ()
runCmds _ 0 ct = do print ct
                    print "Computation Successful!"
runCmds [] stn ct = do print ct
                       print "Error: No commands found."
runCmds cmds stn ct@(cartPos, track) = if stn < 0 then do print ct
                                                          print "Error: State number must be positive."
                      else let (cmd0, stn0, cmd1, stn1) = cmds !! (stn - 1)
                           in
                           if atZero
                           then do let res = runCmd True  cmd0 cartPos track
                                   print res
                                   runCmds cmds stn0 res
                           else do let res = runCmd False cmd1 cartPos track
                                   print res
                                   runCmds cmds stn1 res
                           where
                           atZero = Set.notMember cartPos track

easy :: Cmds
easy = [
    (L, 2, L, 2),
    (M, 3, M, 3),
    (L, 0, L, 0)
    ]

-- s/'//g, s/\[/(/, s/\]/)/. From pow.tm

mul :: Cmds
mul = [(R, 2, R, 2),(R, 13, S, 3),(R, 4, S, 3),(R, 4, R, 5),(M, 11, S, 6),(R, 7, S, 6),(M, 9, S, 8),(M, 9, S, 8),(L, 10, M, 9),(R, 4, M, 10),(L, 11, M, 12),(R, 1, M, 12),(S, 13, M, 14),(R, 0, M, 14)]

pow :: Cmds
pow = [
    (S, 2, S, 6),
    (L, 3, R, 4),
    (R, 0, L, 3),
    (L, 5, S, 4),
    (L, 0, L, 5),
    (R, 7, S, 6),
    (L, 2, S, 8),
    (R, 9, S, 8),
    (S, 10, L, 0),
    (R, 11, L, 0),
    (M, 12, L, 0),
    (L, 12, M, 13),
    (L, 13, L, 14),
    (S, 15, S, 52),
    (R, 15, S, 16),
    (R, 17, S, 16),
    (R, 18, S, 17),
    (M, 19, L, 36),
    (L, 19, M, 20),
    (L, 21, S, 34),
    (L, 21, L, 22),
    (S, 23, S, 29),
    (R, 23, R, 24),
    (R, 24, S, 25),
    (L, 26, S, 25),
    (L, 36, L, 27),
    (M, 28, M, 27),
    (R, 25, R, 36),
    (R, 29, S, 30),
    (S, 30, M, 31),
    (L, 36, R, 32),
    (M, 33, S, 32),
    (L, 19, M, 33),
    (L, 36, R, 35),
    (R, 35, S, 32),
    (L, 37, M, 36),
    (M, 37, L, 38),
    (L, 38, L, 39),
    (S, 40, S, 46),
    (R, 40, R, 41),
    (R, 42, R, 41),
    (L, 43, S, 42),
    (L, 0, L, 44),
    (M, 45, M, 44),
    (R, 42, R, 0),
    (R, 46, S, 47),
    (R, 48, S, 47),
    (R, 49, S, 48),
    (M, 50, S, 49),
    (L, 51, M, 50),
    (L, 13, M, 51),
    (R, 52, S, 48)
    ]

bb2Program :: Cmds
bb2Program = [(S, 2, M, 2),(M, 1, S, 0)]

bb3Program :: Cmds
bb3Program = [(S, 2, S, 0),(R, 3, S, 2), (M, 3, M, 1)]

bb3D2Program :: Cmds
bb3D2Program = [(S, 2, S, 0),(M, 2, R, 3), (M, 3, M, 1)]

bb4Program :: Cmds
bb4Program = [(S, 2, M, 2),(M, 1, L, 3), (S, 0, M, 4), (S, 4, R, 1)]

bb5Program :: Cmds
bb5Program = [(S, 2, M, 3),(S, 3, S, 2), (S, 4, L, 5), (M, 1, M, 4), (S, 0, L, 1)]

type LineNum = Int
type Line = String

lineToCmds :: LineNum -> Line -> Either String Cmds
lineToCmds ln [] = Left ("No commands found on line " ++ show ln)
lineToCmds ln l = case (readMaybe $ cmdsLst wordsLst :: Maybe Cmds) of
                            Nothing -> Left ("Invalid program on line " ++ show ln)
                            Just x  -> Right x
                        where
                        wordsLst = words l -- gets words separated by whitespace
                        cmdsLst (cmd0:cmd1:nst0:nst1:ys) = "[(" ++ cmd0 ++ ", " ++ cmd1 ++ ", " ++ nst0 ++ ", " ++ getLeadNumerals nst1 ++ ")]"
                        cmdsLst _ = ""

-- Keep any leading numerals of a String; to drop any comments that follow
getLeadNumerals :: String -> String
getLeadNumerals []     = ""           -- x is a Char, so convert to String just by enclosing it in a list
getLeadNumerals (x:ys) = case readMaybe [x] :: Maybe Int of -- Safe to use Int since x cannot be > a single digit
                            Just x -> show x ++ getLeadNumerals ys
                            Nothing -> ""

-- Helper to pair line number with line contents in a list,
-- so we can map to it uncurried lineToCmds, which takes (LineNum, Line).
linesToTuple :: LineNum -> [Line] -> [(LineNum, Line)]
linesToTuple _ [] = [(0, [])]
linesToTuple int (x:ys) = (int, x) : linesToTuple (int + 1) ys

-- File contents to Either String Cmds for each line
contentsToECmds :: String -> [Either String Cmds]
contentsToECmds contents = map (uncurry lineToCmds) (init (linesToTuple 1 (lines contents)))

foundCmds :: [Either String Cmds] -> [Cmds]
foundCmds [] = []
foundCmds (x:ys) = case x of
                           Right x -> x : foundCmds ys
                           Left  _ -> foundCmds ys

-- Change entered filename string to end with .tm, given a () or a lack of .tm
checkEndChars :: String -> String
checkEndChars []     = ""
checkEndChars str = go (reverse str)
                  where
                  go ('m':'t':'.':xs) = str
                  go (')':'(':xs) = (reverse xs) ++ "." ++ "t" ++ "m"
                  go _  = str ++ "." ++ "t" ++ "m"

-- Helper to get cmds via a String entered in main
-- Cmds -> State -> (CartPos,Track) 
runFunc :: String -> (CartPos, Track) -> IO ()
runFunc "mul" ct = runCmds mul 1 ct
runFunc "pow" ct = runCmds pow 1 ct
runFunc "bb2" ct = runCmds bb2Program 1 ct
runFunc "bb3" ct = runCmds bb3Program 1 ct
runFunc "bb3D2" ct = runCmds bb3D2Program 1 ct
runFunc "bb4" ct = runCmds bb4Program 1 ct
runFunc "bb5" ct = runCmds bb5Program 1 ct
runFunc "easy" ct = runCmds easy 1 ct
runFunc str ct = do fileContents <- readFile (checkEndChars str)
                    case foundCmds (contentsToECmds fileContents) of
                        []     -> print ("Error: " ++ str ++ " did not have any Turing Program state commands.")
                        cmds@(x:ys) -> runCmds (concat cmds) 1 ct

main = do
    putStrLn "Enter in one of the following commands:"
    putStrLn "mul, pow, bb# (replace # with a number 2-5), bb3D2, or easy."
    putStrLn "Note: mul does not accept any 0 arguments, and"
    putStrLn "      pow's exponent is the 1st arg and base the 2nd."
    putStrLn "Alternatively, enter in the name of a .tm file to run."
    cmdToRun <- getLine
    putStrLn "Enter a cart position on the track. Use 1 for normal use."
    crtPos <- getLine
    putStrLn "Enter a track as a list (i.e, as numbers within square brackets), with each number representing the number of a block that has a 1 symbol. E.g. an argument of 3, 2 would be [1,2,3,5,6]."
    trackList <- getLine
    --runCmds (runFunc cmdToRun) 1 (read crtPos :: Int, Set.fromList (read trackList :: [Int]))
    runFunc cmdToRun (read crtPos :: Int, Set.fromList (read trackList :: [Int]))
