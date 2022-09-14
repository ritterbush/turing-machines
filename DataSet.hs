module Main where

import qualified Data.Set as Set
import Text.Read
import System.IO

data Cmd = L | M | R | S
    deriving (Eq, Show, Read)
type State = Int
type Cmds = [(Cmd, Cmd, State, State)]
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
                      else let (cmd0, cmd1, stn0, stn1) = cmds !! (stn - 1)
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

easy :: [(Cmd, Cmd, State, State)]
easy = [
    (L, L, 2, 2),
    (M, M, 3, 3),
    (L, L, 0, 0)
    ]

-- s/'//g, s/\[/(/, s/\]/)/. From pow.tm

mul :: [(Cmd, Cmd, State, State)]
mul = [(R, R, 2, 2),(R, S, 13, 3),(R, S, 4, 3),(R, R, 4, 5),(M, S, 11, 6),(R, S, 7, 6),(M, S, 9, 8),(M, S, 9, 8),(L, M, 10, 9),(R, M, 4, 10),(L, M, 11, 12),(R, M, 1, 12),(S, M, 13, 14),(R, M, 0, 14)]

pow :: [(Cmd, Cmd, State, State)]
pow = [
    (S, S, 2, 6),
    (L, R, 3, 4),
    (R, L, 0, 3),
    (L, S, 5, 4),
    (L, L, 0, 5),
    (R, S, 7, 6),
    (L, S, 2, 8),
    (R, S, 9, 8),
    (S, L, 10, 0),
    (R, L, 11, 0),
    (M, L, 12, 0),
    (L, M, 12, 13),
    (L, L, 13, 14),
    (S, S, 15, 52),
    (R, S, 15, 16),
    (R, S, 17, 16),
    (R, S, 18, 17),
    (M, L, 19, 36),
    (L, M, 19, 20),
    (L, S, 21, 34),
    (L, L, 21, 22),
    (S, S, 23, 29),
    (R, R, 23, 24),
    (R, S, 24, 25),
    (L, S, 26, 25),
    (L, L, 36, 27),
    (M, M, 28, 27),
    (R, R, 25, 36),
    (R, S, 29, 30),
    (S, M, 30, 31),
    (L, R, 36, 32),
    (M, S, 33, 32),
    (L, M, 19, 33),
    (L, R, 36, 35),
    (R, S, 35, 32),
    (L, M, 37, 36),
    (M, L, 37, 38),
    (L, L, 38, 39),
    (S, S, 40, 46),
    (R, R, 40, 41),
    (R, R, 42, 41),
    (L, S, 43, 42),
    (L, L, 0, 44),
    (M, M, 45, 44),
    (R, R, 42, 0),
    (R, S, 46, 47),
    (R, S, 48, 47),
    (R, S, 49, 48),
    (M, S, 50, 49),
    (L, M, 51, 50),
    (L, M, 13, 51),
    (R, S, 52, 48)
    ]

bb2Program :: Cmds
bb2Program = [(S, M, 2, 2),(M, S, 1, 0)]

bb3Program :: Cmds
bb3Program = [(S, S, 2, 0),(R, S, 3, 2), (M, M, 3, 1)]

bb3D2Program :: Cmds
bb3D2Program = [(S, S, 2, 0),(M, R, 2, 3), (M, M, 3, 1)]

bb4Program :: Cmds
bb4Program = [(S, M, 2, 2),(M, L, 1, 3), (S, M, 0, 4), (S, R, 4, 1)]

bb5Program :: Cmds
bb5Program = [(S, M, 2, 3),(S, S, 3, 2), (S, L, 4, 5), (M, M, 1, 4), (S, L, 0, 1)]

-- Helper to get cmds via a String entered in main
runFunc :: String -> Cmds
runFunc "mul" = mul
runFunc "pow" = pow
runFunc "bb2" = bb2Program
runFunc "bb3" = bb3Program
runFunc "bb3D2" = bb3D2Program
runFunc "bb4" = bb4Program
runFunc "bb5" = bb5Program
runFunc "easy" = easy
runFunc _ = easy

type Line = Int

lineToCmds :: Line -> String -> Either String (Maybe Cmds)
lineToCmds ln [] = Left ("No commands found on line " ++ show ln)
lineToCmds ln cmdsStr = if length cmdsLst /= 4
                        then Left ("Invalid program on line " ++ show ln)
                        else case (readMaybe fStr :: Maybe Cmds) of
                            Nothing -> Left ("Invalid program on line " ++ show ln)
                            Just x  -> Right (Just x)
                        where
                        cmdsLst = words cmdsStr
                        fStr = "[(" ++ cmd0 ++ ", " ++ cmd1 ++ ", " ++ nst0 ++ ", " ++ nst1 ++ ")]"
                        cmd0 = cmdsLst !! 0
                        nst0 = cmdsLst !! 1
                        cmd1 = cmdsLst !! 2
                        nst1 = cmdsLst !! 3

main = do
    putStrLn "Enter in one of the following commands:"
    putStrLn "mul, pow, bb# (replace # with a number 2-5), bb3D2, or easy"
    cmdToRun <- getLine
    putStrLn "Enter a cart position on the track"
    crtPos <- getLine
    putStrLn "Enter a track as a list, with each entry the number of a block that has a 1 symbol."
    trackList <- getLine
    runCmds (runFunc cmdToRun) 1 (read crtPos :: Int, Set.fromList (read trackList :: [Int]))
