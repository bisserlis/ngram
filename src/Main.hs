module Main where

import Options.Applicative

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

main = execParser opts >>= undefined
    where
        opts = info (helper <*> configuration)
               ( fullDesc
              <> progDesc "Generate I sequences from N-grams derived from FILE"
              <> header "ngram - an n-gram based sequence generator" )
