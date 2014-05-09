{-# LANGUAGE OverloadedStrings #-}


module NLP.FeatureStructure.Parse.Tests
( 
-- -- * Lexicon
-- 
-- -- ** Verbs
--   sleepL
-- , sleepsL
-- , loveL
-- , lovesL
-- , eatL
-- , eatsL
-- , tellL
-- , tellsL
-- 
-- -- ** Other
-- , lambL
-- , lambsL
-- , sheL
-- , herL
-- , rachelL
-- , jacobL
-- , aL
-- , twoL
-- 
-- 
-- -- * Rules
-- -- , ruleSet
-- 
-- 
-- -- * Parsing
-- -- , reidData
-- , parse
-- , parse'
) where


import           Control.Monad (forM, replicateM)
import           Data.Text (Text)
import qualified Data.Vector as V


import           NLP.FeatureStructure.Tree
    (avm, leaf, empty, atom, name, label, feat, nameAVM, list)
import qualified NLP.FeatureStructure.Tree as R
import qualified NLP.FeatureStructure.Graph as G
import qualified NLP.FeatureStructure.Parse as P
import qualified NLP.FeatureStructure.Reid as Reid


--------------------------------------------------------------------
-- Types
--------------------------------------------------------------------


-- Types with instantiated parameters.
type FT = R.FT Text Text Text
type FF = R.FF Text Text Text
type FN = R.FN Text Text Text
type AV = R.AV Text Text Text
type AVM = R.AVM Text Text Text


--------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------


-- | A grammatical class constructor.
cat :: FN -> AVM
cat = feat "cat"
catl :: Text -> AVM
catl = cat . atom

-- -- | Grammatical classes in our toy grammar.
-- sent, verb, determiner, noun, pronoun, nounPhrase, properName :: AVM
-- sent = catl "s"
-- verb = catl "v"
-- noun = catl "n"
-- pronoun = catl "pron"
-- determiner = catl "d"
-- nounPhrase = catl "np"
-- properName = catl "propn"
-- 
-- 
-- -- | Number.
-- num :: FN -> AVM
-- num = feat "num"
-- numl :: Text -> AVM
-- numl = num . atom
-- 
-- singular, plural :: AVM
-- singular = numl "sg"
-- plural = numl "pl"
-- 
-- 
-- -- | Case.
-- cas :: FN -> AVM
-- cas = feat "case"
-- casl :: Text -> AVM
-- casl = cas . atom
-- 
-- nominative, accusative :: AVM
-- nominative = casl "nom"
-- accusative = casl "acc"
-- 
-- 
-- -- | Orth.
-- orth :: Text -> AVM
-- orth x = leaf "orth" x
-- 
-- 
-- --------------------------------------------------------------------
-- -- Subcategorization frame.
-- --------------------------------------------------------------------
-- 
-- 
-- -- | Subcategorization feature.
-- subcat :: FN -> AVM
-- subcat = feat "subcat"
-- 
-- 
-- -- | The first feature of the subcat.
-- first :: FN -> AVM
-- first = feat "first"
-- 
-- 
-- -- | The rest feature of the subcat.
-- rest :: FN -> AVM
-- rest = feat "rest"
-- 
-- 
-- -- | Make subcategorization frame from a list of `AVM`s.
-- subcatFrame :: [AVM] -> AVM
-- subcatFrame = subcat . list "nil" "first" "rest" . map avm
-- 
-- 
-- --------------------------------------------------------------------
-- -- Lexicon: verbs
-- --------------------------------------------------------------------
-- 
-- 
-- sleepL :: FN
-- sleepL = avm $ do
--     orth "sleep"
--     verb >> plural
--     subcatFrame []
-- 
-- 
-- sleepsL :: FN
-- sleepsL = avm $ do
--     orth "sleeps"
--     verb >> singular
--     subcatFrame []
-- 
-- 
-- loveL :: FN
-- loveL = avm $ do
--     orth "love"
--     verb >> plural
--     subcatFrame [nounPhrase >> accusative]
-- 
-- 
-- lovesL :: FN
-- lovesL = avm $ do
--     orth "loves"
--     verb >> singular
--     subcatFrame [nounPhrase >> accusative]
-- 
-- 
-- eatL :: FN
-- eatL = avm $ do
--     orth "eat"
--     verb >> plural
--     subcatFrame [nounPhrase >> accusative]
-- 
-- 
-- eatsL :: FN
-- eatsL = avm $ do
--     orth "eats"
--     verb >> singular
--     subcatFrame [nounPhrase >> accusative]
-- 
-- 
-- tellL :: FN
-- tellL = avm $ do
--     orth "tell"
--     verb >> plural
--     subcatFrame [nounPhrase >> accusative, sent]
-- 
-- 
-- tellsL :: FN
-- tellsL = avm $ do
--     orth "tells"
--     verb >> singular
--     subcatFrame [nounPhrase >> accusative, sent]
-- 
-- 
-- --------------------------------------------------------------------
-- -- Lexicon: other
-- --------------------------------------------------------------------
-- 
-- 
-- lambL :: FN
-- lambL = avm $ noun >> singular >> orth "lamb"
-- 
-- 
-- lambsL :: FN
-- lambsL = avm $ noun >> plural >> orth "lambs"
-- 
-- 
-- sheL :: FN
-- sheL = avm $ pronoun >> singular >> nominative >> orth "she"
-- 
-- 
-- herL :: FN
-- herL = avm $ pronoun >> singular >> accusative >> orth "her"
-- 
-- 
-- rachelL :: FN
-- rachelL = avm $ properName >> singular >> orth "rachel"
-- 
-- 
-- jacobL :: FN
-- jacobL = avm $ properName >> singular >> orth "jacob"
-- 
-- 
-- aL :: FN
-- aL = avm $ determiner >> singular >> orth "a"
-- 
-- 
-- twoL :: FN
-- twoL = avm $ determiner >> plural >> orth "two"
-- 
-- 
-- --------------------------------------------------------------------
-- -- Rules
-- --------------------------------------------------------------------
-- 
-- 
-- -- | Construct a rule with the given head and the given body.
-- mkR :: String -> AVM -> [AVM] -> P.Rule Text Text
-- mkR ruleName x xs = unjust ruleName $ do
--     (is, g) <- R.compiles $ map avm (x:xs)
--     (rh, rb) <- unCons is
--     return $ P.mkRule rh rb g
-- 
-- 
-- -- | A sentence rule.
-- sentR :: P.Rule Text Text
-- sentR = mkR "sentR" sent
--     [ nounPhrase >> nominative >> num "?num"
--     , verb >> subcatFrame [] >> num "?num" ]
-- 
-- 
-- -- | Subcategorization resolution.
-- subcatR :: P.Rule Text Text
-- subcatR = mkR "subcatR"
--     ( verb >> num "?num" >> subcat "?xs" )
--     [ verb >> num "?num" >> subcat (avm $
--         first "?x" >> rest "?xs")
--     , nameAVM "?x" ]
-- 
-- 
-- -- | NP -> D + N
-- npDetNounR :: P.Rule Text Text
-- npDetNounR = mkR "npDetNounR" hd bd where
--     hd = nounPhrase >> num "?num" >> cas "?case"
--     bd = [ determiner >> num "?num"
--          , noun >> num "?num" >> cas "?case" ]
-- 
-- 
-- -- | NP -> N (plural)
-- npPlNounR :: P.Rule Text Text
-- npPlNounR = mkR "npPlNounR"
--     (nounPhrase >> plural >> cas "?case")
--     [noun >> plural >> cas "?case"]
-- 
-- 
-- -- | NP -> Pronoun
-- npPronR :: P.Rule Text Text
-- npPronR = mkR "npPronR" 
--     (nounPhrase >> num "?num" >> cas "?case")
--     [pronoun >> num "?num" >> cas "?case"]
-- 
-- 
-- -- | NP -> Proper name
-- npPropNameR :: P.Rule Text Text
-- npPropNameR = mkR "npPropNameR"
--     (nounPhrase >> numCas)
--     [properName >> numCas]
--     where numCas = num "?num" >> cas "?case"
-- 
-- 
-- -- | All rules of the grammar.
-- ruleSet :: [P.Rule Text Text]
-- ruleSet = [sentR, npDetNounR, npPlNounR, npPronR, npPropNameR, subcatR]
-- 
-- 
-- --------------------------------------------------------------------
-- -- Misc
-- --------------------------------------------------------------------
-- 
-- 
-- -- | Un`Just` with the given error message.
-- unjust :: String -> Maybe a -> a
-- unjust _ (Just x) = x
-- unjust e Nothing  = error e
-- 
-- 
-- -- | Uncons the list.
-- unCons :: [a] -> Maybe (a, [a])
-- unCons (x:xs) = Just (x, xs)
-- unCons []     = Nothing
-- 
-- 
-- --------------------------------------------------------------------
-- -- Testing
-- --------------------------------------------------------------------
-- 
-- 
-- -- | A simplified parsing function.
-- parse :: [FN] -> [P.Rule Text Text]
-- parse sent = parse' sent 0 (length sent)
-- 
-- 
-- -- | A simplified parsing function.
-- parse' :: [FN] -> Int -> Int -> [P.Rule Text Text]
-- -- parse' sent = filter P.isFull $
-- parse' sent =
--     P.parse ruleSet (map compileEntry sent)
--   where
--     compileEntry fn = unjust ("compileEntry: " ++ show fn) $ do
--         (i, g) <- R.compile fn
--         return $ P.mkEntry i g
