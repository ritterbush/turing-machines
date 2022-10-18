module Main where

import qualified Data.Set as Set
import System.IO
import Cmds
import Programs

type Track = Set.Set Int
type CartPos = Int

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
