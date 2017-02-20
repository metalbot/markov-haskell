module Markov
    ( buildFile,
      buildDirectory,
      generateFixedLen,
      generate,
      saveDictionary,
      loadDictionary
    ) where

import qualified Data.Map as Map
import System.Random
import System.Directory
import Text.JSON
import Data.Text
import qualified Data.Text.IO as IO

type Prefix = (Maybe Text, Maybe Text)
type PrefixDictionary = Map.Map Prefix [Maybe Text]

-- Surely there is a way to determine whether a path is a file or dir, then
-- we could be smart about this instead of having two functions
buildDirectory :: FilePath -> IO (PrefixDictionary)
buildDirectory path = do
  files <- listDirectory path
  dictionary <- mkDictionaryFromFiles (Prelude.map (\f -> path ++ "\\" ++ f) files) Map.empty
  return (fst dictionary)

buildFile :: FilePath -> IO (PrefixDictionary)
buildFile path = do
  file <- IO.readFile path
  return (fst (mkDictionary (Data.Text.words file) Map.empty))

mkDictionary :: [Text] -> PrefixDictionary -> (PrefixDictionary, Prefix)
mkDictionary xs startingDict = Prelude.foldl dbuilder (startingDict, (Nothing, Nothing)) xs

mkDictionaryFromFiles :: [FilePath] -> PrefixDictionary -> IO (PrefixDictionary, Prefix)
mkDictionaryFromFiles [] startingDict = return (startingDict, (Nothing, Nothing))
mkDictionaryFromFiles (x:xs) startingDict = do
  file <- IO.readFile x
  result <- mkDictionaryFromFiles xs (fst (mkDictionary (Data.Text.words file) startingDict)) 
  return result

-- Fold operation to either insert a new prefoix or update the list for an existing one
dbuilder :: (PrefixDictionary, Prefix) -> Text -> (PrefixDictionary, Prefix)
dbuilder (dict, prefix) s =
  if (Map.member prefix dict)
    then (Map.adjust (\words -> (Just s) : words) prefix dict, (snd prefix, Just s))
    else (Map.insert prefix [Just s] dict, (snd prefix, Just s))

generate :: PrefixDictionary -> IO [Text]
generate dict = generate' dict (Nothing, Nothing)
  
generateFixedLen :: Int -> PrefixDictionary -> IO [Text]
generateFixedLen n d = generateFixedLen' n d (Nothing, Nothing)

generate' :: PrefixDictionary -> Prefix -> IO [Text]
generate' dict prefix = do
    next <- nextRandWord dict prefix
    str <- case next of
            (Nothing, _) -> return []
            (Just a, p) -> do
              rest <- (generate' dict p)
              return (a : rest)
    return str

generateFixedLen' :: Int -> PrefixDictionary -> Prefix -> IO [Text]
generateFixedLen' 0 _ _ = return []
generateFixedLen' n dict prefix = do
  next <- nextRandWord dict prefix
  str <- case next of 
          (Nothing, _) -> return []
          (Just a, p) -> do
            rest <- (generateFixedLen' (n-1) dict p)
            return (a : rest)
  return str

-- Fairly sure there's a haskell package that will hide this for me
-- Alternative to the IO would be to pass in a random generator from the top level
nextRandWord :: PrefixDictionary -> Prefix -> IO (Maybe Text, Prefix)
nextRandWord d p = do
  let words = Map.findWithDefault [Nothing] p d
  pickword <- randElement words
  return (pickword, (snd p, pickword))

randElement :: [Maybe Text] -> IO (Maybe Text)
randElement [] = return Nothing
randElement xs = do
  i <- getStdRandom (randomR (0, (Prelude.length xs) - 1))
  return (xs !! i)

saveDictionary :: PrefixDictionary -> FilePath -> IO ()
saveDictionary dict filePath = writeFile filePath (encode dict)

loadDictionary :: FilePath -> IO (PrefixDictionary)
loadDictionary filePath = do
  file <- readFile filePath
  let d = case (decode file) of
            Ok dict -> dict
            Error s -> Map.empty
  return d

