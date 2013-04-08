-- | Main module for GFGrammarGenerator
module Main where

import GFGrammarGenerator

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Console.GetOpt

main = do
    args <- getArgs
    (umltypes, mOutputDir, mInclude, discardJava) <- processOpts args
    umltypes2grammar umltypes discardJava (fromMaybe "" mOutputDir) mInclude [] -- assume no let-defs

data Flag = 
    OutputPath String | 
    IncludeDir String |
    IgnoreJava          deriving (Eq,Ord)

options :: [OptDescr Flag]
options = [
        Option ['o'] ["output-dir"] (ReqArg OutputPath "DIRECTORY") 
            "path (directory) for where to put generated gf grammars",
        Option ['i'] ["include-dir"] (ReqArg IncludeDir "DIRECTORY")
            "path to directory containing OCLNL-GF grammars to be included",
        Option ['j'] ["ignore-java"] (NoArg IgnoreJava) 
            "ignore all java library packages"
    ]

processOpts :: [String] -> IO (FilePath, Maybe FilePath, Maybe String, Bool)
processOpts argv =
    case (getOpt RequireOrder options argv) of
        (opts, [umltypes], []) -> return 
            (umltypes, getOutputDir opts, getIncludeDir opts, getDiscardJava opts)
        (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo header options
    where
        header = "Usage: umltypes2gf [OPTION*] modelinfo.umltypes"
        getOutputDir = foldl 
            (\r x -> case x of {OutputPath s -> Just s; _ -> r})
            Nothing
        getIncludeDir = foldl
            (\r x -> case x of {IncludeDir s -> Just s; _ -> r})
            Nothing
        getDiscardJava opts = IgnoreJava `elem` opts
