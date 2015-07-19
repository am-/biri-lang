{-# LANGUAGE LambdaCase #-}

module Main
( main
) where

import Control.Applicative ((<$>))
import Control.Monad (forM, when)
import Data.List (intercalate, unzip3)
import qualified Data.Text.IO as T
import Options.Applicative
import System.Directory (removeFile)
import System.Environment (getArgs)
import System.FilePath ((<.>))
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess)

import qualified Biri.Backend.Routes as Routes
import qualified Biri.Backend.Target as Target
import qualified Biri.Backend.Rts as Rts
import Biri.Language

main :: IO ()
main = execParser cliInfo >>= compile

--------------------------------------------------------------------------------
-- Compilation
--------------------------------------------------------------------------------

compile :: Cli -> IO ()
compile (Cli { output = output, target = target, input = files, retain = retain }) = do
    programs <- forM files $ \path -> T.readFile path >>= \file -> case parse file of
        Left err -> do
            putStrLn $ "Parsing " ++ path ++ ": FAILED (" ++ err ++ ")"
            return Nothing
        Right (Program resources datas functions) -> do
            putStrLn $ "Parsing " ++ path ++ ": OK ("
                    ++ show (length resources) ++ " routes, " 
                    ++ show (length datas) ++ " data declarations, "
                    ++ show (length functions) ++ " functions)"
            return (Just (resources, datas, functions))
    
    case fmap unzip3 (sequence programs) of
      Nothing                                   -> putStrLn "Compilation aborted."
      Just (resources,_,_) | all null resources -> putStrLn "Nothing to do."
      Just (resources, datas, functions) -> case target of
         Scgi -> do
             print $ resources
             putStr $ "Generate routes... "
             T.writeFile "routes.c" $ Routes.generate (concat resources)
             putStrLn "Done!"
             
             putStr $ "Generate run-time system... "
             T.writeFile "rts.c" Rts.bootstrap_c
             case Rts.data_h (concat datas) of
                 Left errors -> error (unlines errors)
                 Right t -> T.writeFile "data.h" t
             putStrLn $ "Done!"
             
             putStr $ "Generate target... "
             T.writeFile "scgi.h" Target.scgi_h
             T.writeFile "scgi.c" Target.scgi_c
             callProcess "gcc" ["-o" ++ output, "-std=c99", "scgi.c", "routes.c", "rts.c", "-lev", "-pthread"]
             putStrLn "Done!"
             
             when (not retain) (mapM_ removeFile ["scgi.h", "scgi.c", "routes.c", "data.h", "rts.c"])

--------------------------------------------------------------------------------
-- Command line interface
--------------------------------------------------------------------------------

data Cli = Cli
    { target :: Target
    , output :: FilePath
    , retain :: Bool
    , input :: [FilePath]
    }

data Target = Scgi
            deriving (Show, Eq, Enum)

cliInfo :: ParserInfo Cli
cliInfo = info
    (helper <*> cliParser)
    (fullDesc <> progDesc "Compile..." <> header "biri - a compiler for the Biri programming language")

cliParser :: Parser Cli
cliParser = Cli
    <$> targetOption
    <*> strOption (long "output" <> short 'o' <> help "Write output to ARG")
    <*> switch (long "retain" <> short 'r' <> help "Retain intermediate files" <> hidden)
    <*> some (argument str (metavar "FILES..."))

targetOption :: Parser Target
targetOption = option (str >>= toTarget) (long "target" <> short 't' <> help ("Target to compile to " ++ possibleValues))
  where
    toTarget :: String -> ReadM Target
    toTarget = \case
        "scgi" -> return Scgi
        _      -> readerError $ "Unrecognized target " ++ possibleValues
    
    possibleValues :: String
    possibleValues = ("(possible values=" ++) . (++ ")") . intercalate ", "
                   $ map (('`':) . (++ "'")) ["scgi"]
