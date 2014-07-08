{-# LANGUAGE BangPatterns #-}

module Main where

import Options.Applicative
import Data.List
import Data.Map (Map, fromList, fromListWith, singleton, unionWith, unionsWith)
import System.Random

data Configuration = Configuration
                   { optN :: Int
                   , optI :: Int
                   , optInput :: FilePath
                   }

configuration :: Parser Configuration
configuration = Configuration
            <$> option
                ( short 'n'
               <> metavar "N"
               <> value 2
               <> help ( "Derive N-grams (bigrams, trigrams, 4-grams...) "
                      ++ "(Default: 2)" ) )
            <*> option
                ( short 'i'
               <> metavar "I"
               <> value 10
               <> help "Number of sequences to generate (Default: 10)" )
            <*> argument str (metavar "FILE")

main :: IO ()
main = execParser opts >>= deriveSequences
    where
        opts = info (helper <*> configuration)
               ( fullDesc
              <> progDesc "Generate I sequences from N-grams derived from FILE"
              <> header "ngram - an n-gram based sequence generator" )

deriveSequences :: Configuration -> IO ()
deriveSequences (Configuration n i f) = do
        corpus <- fmap lines (readFile f)
        stdGen <- getStdGen
        mapM_ putStrLn . take i . markov stdGen . trains n $ corpus
        return ()

type Model a = Map (Sequence a) (Map (Maybe a) Int)
type Sequence a = [a]

train :: (Ord a) => Int -> Sequence a -> Model a
train n = fromListWith (unionWith (+))
          . map (fmap (flip singleton 1))
          . gramsOf n

unify :: (Ord a) => [Model a] -> Model a
unify = unionsWith (unionWith (+))

trains :: (Ord a) => Int -> [Sequence a] -> Model a
trains n = unify . map (train n)

gramsOf :: Int -> Sequence a -> [(Sequence a, Maybe a)]
gramsOf n as = zip prefixes mas
    where prefixes = map (\(i, l) -> drop (i - n) l) (zip [0..] (inits as))
          mas = map Just as ++ [Nothing]

markov :: StdGen -> Model a -> [Sequence a]
markov = undefined

