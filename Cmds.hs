module Cmds where

import Text.Read

data Cmd = L | M | R | S
    deriving (Eq, Show, Read)
type State = Int
type Cmds = [(Cmd, State, Cmd, State)]

type LineNum = Int
type Line = String

lineToECmds :: LineNum -> Line -> Either String Cmds
lineToECmds ln [] = Left ("No commands found on line " ++ show ln)
lineToECmds ln l = case (readMaybe $ cmdsLst wordsLst :: Maybe Cmds) of
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
-- so we can map to it uncurried lineToECmds, which takes (LineNum, Line).
linesToTuple :: LineNum -> [Line] -> [(LineNum, Line)]
linesToTuple _ [] = [(0, [])]
linesToTuple int (x:ys) = (int, x) : linesToTuple (int + 1) ys

-- File contents to Either String Cmds for each line
contentsToECmds :: String -> [Either String Cmds]
contentsToECmds contents = map (uncurry lineToECmds) (init (linesToTuple 1 (lines contents)))

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
