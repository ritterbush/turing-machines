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

type LineNum = Int

lineToCmds :: LineNum -> String -> Either String Cmds
lineToCmds ln [] = Left ("No commands found on line " ++ show ln)
lineToCmds ln cmdsStr = case (readMaybe $ cmdsLst fStr :: Maybe Cmds) of
                            Nothing -> Left ("Invalid program on line " ++ show ln)
                            Just x  -> Right x
                        where
                        fStr = words cmdsStr
                        cmdsLst (cmd0:cmd1:nst0:nst1:ys) = "[(" ++ cmd0 ++ ", " ++ cmd1 ++ ", " ++ nst0 ++ ", " ++ nst1 ++ ")]"
                        cmdsLst _ = ""

linesToTuple :: Int -> [String] -> [(Int, String)]
linesToTuple _ [] = [(0, [])]
linesToTuple int (x:ys) = (int, x) : linesToTuple (int + 1) ys

contentsToECmds :: String -> [Either String Cmds]
contentsToECmds contents = map (uncurry lineToCmds) (init (linesToTuple 1 (lines contents)))

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

validationFailed :: [Either String Cmds] -> [Either String Cmds]
validationFailed = filter isLeft

validationSucceeded :: [Either String Cmds] -> [Either String Cmds]
validationSucceeded = filter isRight

main = do
    putStrLn "Enter in one of the following commands:"
    putStrLn "mul, pow, bb# (replace # with a number 2-5), bb3D2, or easy."
    putStrLn "Note: mul does not accept any 0 arguments, and"
    putStrLn "      pow's exponent is the 1st arg and base the 2nd."
    cmdToRun <- getLine
    putStrLn "Enter a cart position on the track. Use 1 for normal use."
    crtPos <- getLine
    putStrLn "Enter a track as a list (i.e, as numbers within square brackets), with each number representing the number of a block that has a 1 symbol. E.g. an argument of 3, 2 would be [1,2,3,5,6]."
    trackList <- getLine
    runCmds (runFunc cmdToRun) 1 (read crtPos :: Int, Set.fromList (read trackList :: [Int]))
