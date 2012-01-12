{-
    Zachary Weaver <zearen.wover@gmail.com>
    ConlangTool.hs
    Version 0.1
    
    Does Conlang stuff
-}

{-# LANGUAGE FlexibleContexts #-}

module ConlangTool where
{-
    ( Language (..)
    , Frequency (..)
    , createFrequency
    , randomWord
    )
-}

import IO
import Random

import Control.Monad.State.Class
import Control.Monad.Either

import qualified Data.Map as Map
import qualified Data.Vector.Generic as V

import Text.Parsec
import Text.Parsec.String

import qualified Statistics.Sample as S
import qualified Statistics.Distribution as D
import Statistics.Distribution.Gamma

import Util

-- | A general represenation of a language
data Language = Literal String      -- ""
              | LangLambda String   -- place holder, compiled away
              | Sequence [Language] -- a b c 
              | Choice [Language]   -- a|b|c
              | Option Language     -- a?
              | Repeat Language     -- a*
    deriving (Eq, Ord)

data Counter = CntLit String
              | CntSeq [Counter]
              | CntChoice (Map.Map Counter Integer)
              | CntOpt Integer Integer Counter
              | CntRepeat Integer Counter
    deriving (Eq, Ord, Show)



-- | A representation of a languge based on the chance of
-- a particular structure.
data Frequency = FreqLit ShowS
               | FreqSeq [Frequency]
--             | FreqMarcov -- I'll figure out how to do this later
               | FreqChoice [(Frequency, Double)]
               | FreqOpt Double Frequency
               | FreqRepeat GammaDistribution Frequency

randomReplicate :: D.ContDistr d => d -> a -> IO [a]
randomReplicate dist val = do
    reps <- fmap (round . D.quantile dist) $ randomRIO (0, 1)
    return $ replicate reps val

deriveGammaDist :: V.Vector v Double => v Double -> GammaDistribution
deriveGammaDist vec = gammaDistr k θ
  where k = sqrt $ σ2 * μ
        θ = sqrt $ σ2 / μ
        (μ, σ2) = S.meanVariance vec

-- We expect sum $ map snd lst == 1.0
randomChoice :: [(a, Double)] -> IO a
randomChoice lst = do
    rand <- randomRIO (0, 1)
    return $ fst $ head $ dropWhile ((<rand).snd) $ scanl1 
        (\(_, total) (val, count) -> (val, count + total)) lst

normalizeFrequencies :: (Integral i, RealFrac d) => [(a, i)] -> [(a, d)]
normalizeFrequencies lst = map normalize lst 
  where normalize (val, count) = (val, fromIntegral count / theSum)
        theSum = fromIntegral $ sum $ map snd lst

-- Oh, this felt dirty
randomWord :: Frequency -> IO String
randomWord = fmap ($[]) . randWord
  where randWord :: Frequency -> IO ShowS
        randWord (FreqLit str) = return str
        randWord (FreqSeq sequence) =
            fmap (foldl (.) id) $ mapM randWord sequence
        randWord (FreqChoice choices) = 
            randomChoice choices >>= randWord
        randWord (FreqOpt prob freq) =
            randomRIO (0,1) >>= (randWord freq ?? return id) . (< prob)
        randWord (FreqRepeat dist freq) = 
            randomReplicate dist freq >>= randWord . FreqSeq

type LangParser m a = ParsecT String (Map.Map String Language) m a

instance MonadState ParsecT where
    get = getState
    put = putState

parseLanguage :: FilePath -> String -> Either ParseError Language
parseLanguage fn source = do
    table <- runParser languageParser Map.empty fn source

languageParser :: LangParser m (Map.Map String Language)
languageParser = get
