{-# LANGUAGE OverloadedStrings #-}


module NLP.FeatureStructure.Parse.Tests
( 
-- * Lexicon

-- ** Verbs
  sleepL
, sleepsL
, loveL
, lovesL
, eatL
, eatsL
, tellL
, tellsL

-- ** Other
, lambL
, lambsL
, sheL
, herL
, rachelL
, jacobL
, aL
, twoL


-- * Rules
-- , ruleSet


-- * Parsing
-- , reidData
, parse
, parse'
) where


import           Control.Monad (forM, replicateM)
import           Data.Text (Text)
import qualified Data.Vector as V


import           NLP.FeatureStructure.AVM
    (avm, empty, atom, label, feat, list)
import qualified NLP.FeatureStructure.AVM as A
import qualified NLP.FeatureStructure.Tree as R
import qualified NLP.FeatureStructure.Graph as G
import qualified NLP.FeatureStructure.Parse as P
import qualified NLP.FeatureStructure.Reid as Reid


--------------------------------------------------------------------
-- Types
--------------------------------------------------------------------


-- Types with instantiated parameters.
-- type FT = A.FT Text Text Text
-- type FF = A.FF Text Text Text
-- type FN = A.FN Text Text Text
-- type AV = A.AV Text Text Text
type AVM = A.AVM Text Text Text


--------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------


-- | A grammatical class constructor.
cat :: AVM -> AVM
cat = feat "cat"
catl :: Text -> AVM
catl = cat . atom

-- | Grammatical classes in our toy grammar.
sent, verb, determiner, noun, pronoun, nounPhrase, properName :: AVM
sent = catl "s"
verb = catl "v"
noun = catl "n"
pronoun = catl "pron"
determiner = catl "d"
nounPhrase = catl "np"
properName = catl "propn"


-- | Number.
num :: AVM -> AVM
num = feat "num"
numl :: Text -> AVM
numl = num . atom

singular, plural :: AVM
singular = numl "sg"
plural = numl "pl"


-- | Case.
cas :: AVM -> AVM
cas = feat "case"
casl :: Text -> AVM
casl = cas . atom

nominative, accusative :: AVM
nominative = casl "nom"
accusative = casl "acc"


-- | Orth.
orth :: Text -> AVM
orth = feat "orth" . atom


--------------------------------------------------------------------
-- Subcategorization frame.
--------------------------------------------------------------------


-- | Subcategorization feature.
subcat :: AVM -> AVM
subcat = feat "subcat"


-- | The first feature of the subcat.
first :: AVM -> AVM
first = feat "first"


-- | The rest feature of the subcat.
rest :: AVM -> AVM
rest = feat "rest"


-- | Make subcategorization frame from a list of `AVM`s.
subcatFrame :: [AVM] -> AVM
subcatFrame = subcat . list "nil" "first" "rest"


--------------------------------------------------------------------
-- Lexicon: verbs
--------------------------------------------------------------------


sleepL :: AVM
sleepL = do
    orth "sleep"
    verb >> plural
    subcatFrame []


sleepsL :: AVM
sleepsL = do
    orth "sleeps"
    verb >> singular
    subcatFrame []


loveL :: AVM
loveL = do
    orth "love"
    verb >> plural
    subcatFrame [nounPhrase >> accusative]


lovesL :: AVM
lovesL = do
    orth "loves"
    verb >> singular
    subcatFrame [nounPhrase >> accusative]


eatL :: AVM
eatL = do
    orth "eat"
    verb >> plural
    subcatFrame [nounPhrase >> accusative]


eatsL :: AVM
eatsL = do
    orth "eats"
    verb >> singular
    subcatFrame [nounPhrase >> accusative]


tellL :: AVM
tellL = do
    orth "tell"
    verb >> plural
    subcatFrame [nounPhrase >> accusative, sent]


tellsL :: AVM
tellsL = do
    orth "tells"
    verb >> singular
    subcatFrame [nounPhrase >> accusative, sent]


--------------------------------------------------------------------
-- Lexicon: other
--------------------------------------------------------------------


lambL :: AVM
lambL = noun >> singular >> orth "lamb"


lambsL :: AVM
lambsL = noun >> plural >> orth "lambs"


sheL :: AVM
sheL = pronoun >> singular >> nominative >> orth "she"


herL :: AVM
herL = pronoun >> singular >> accusative >> orth "her"


rachelL :: AVM
rachelL = properName >> singular >> orth "rachel"


jacobL :: AVM
jacobL = properName >> singular >> orth "jacob"


aL :: AVM
aL = determiner >> singular >> orth "a"


twoL :: AVM
twoL = determiner >> plural >> orth "two"


--------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------


-- | Construct a rule with the given head and the given body.
mkR :: String -> AVM -> [AVM] -> P.Rule Text Text
mkR ruleName x xs = unjust ruleName $ do
    (is, g) <- R.compiles $ map avm (x:xs)
    (rh, rb) <- unCons is
    return $ P.mkRule rh rb g


-- | A sentence rule.
sentR :: P.Rule Text Text
sentR = mkR "sentR" sent
    [ nounPhrase >> nominative >> num "?num"
    , verb >> subcatFrame [] >> num "?num" ]


-- | Subcategorization resolution.
subcatR :: P.Rule Text Text
subcatR = mkR "subcatR"
    ( verb >> num "?num" >> subcat "?xs" )
    [ verb >> num "?num" >> subcat (first "?x" >> rest "?xs")
    , label "?x" ]


-- | NP -> D + N
npDetNounR :: P.Rule Text Text
npDetNounR = mkR "npDetNounR" hd bd where
    hd = nounPhrase >> num "?num" >> cas "?case"
    bd = [ determiner >> num "?num"
         , noun >> num "?num" >> cas "?case" ]


-- | NP -> N (plural)
npPlNounR :: P.Rule Text Text
npPlNounR = mkR "npPlNounR"
    (nounPhrase >> plural >> cas "?case")
    [noun >> plural >> cas "?case"]


-- | NP -> Pronoun
npPronR :: P.Rule Text Text
npPronR = mkR "npPronR" 
    (nounPhrase >> num "?num" >> cas "?case")
    [pronoun >> num "?num" >> cas "?case"]


-- | NP -> Proper name
npPropNameR :: P.Rule Text Text
npPropNameR = mkR "npPropNameR"
    (nounPhrase >> numCas)
    [properName >> numCas]
    where numCas = num "?num" >> cas "?case"


-- | All rules of the grammar.
ruleSet :: [P.Rule Text Text]
ruleSet = [sentR, npDetNounR, npPlNounR, npPronR, npPropNameR, subcatR]


--------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------


-- | Un`Just` with the given error message.
unjust :: String -> Maybe a -> a
unjust _ (Just x) = x
unjust e Nothing  = error e


-- | Uncons the list.
unCons :: [a] -> Maybe (a, [a])
unCons (x:xs) = Just (x, xs)
unCons []     = Nothing


--------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------


-- | A simplified parsing function.
parse :: [AVM] -> [P.Rule Text Text]
parse sent = parse' sent 0 (length sent)


-- | A simplified parsing function.
parse' :: [AVM] -> Int -> Int -> [P.Rule Text Text]
-- parse' sent = filter P.isFull $
parse' sent =
    P.parse ruleSet $ map (compileEntry . avm) sent
  where
    compileEntry fn = unjust ("compileEntry: " ++ show fn) $ do
        (i, g) <- R.compile fn
        return $ P.mkEntry i g
