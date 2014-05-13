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
, lookL
, looksL

-- ** Other
, lambL
, lambsL
, sheL
, herL
, rachelL
, jacobL
, aL
, twoL
, atL
, upL


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
    (avm, empty, atom, label, feat, tryFeat, list, (##), (#?))
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
type Rule = P.Rule Text Text


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
prep = catl "prep"
pronoun = catl "pron"
determiner = catl "d"
nounPhrase = catl "np"
verbPhrase = catl "vp"
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
orth :: AVM -> AVM
orth = feat "orth"


-- | Lemma.
lemma :: AVM -> AVM
lemma = feat "lemma"


-- | Set lemma if not present.
tryLemma :: AVM -> AVM
tryLemma = tryFeat "lemma"


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
frame :: [AVM] -> AVM
frame = subcat . list "nil" "first" "rest"


--------------------------------------------------------------------
-- Lexicon: verbs
--------------------------------------------------------------------


sleepL :: AVM
sleepL = do
    orth "sleep" >> lemma "sleep"
    verb >> plural
    frame []


sleepsL :: AVM
sleepsL = do
    orth "sleeps" >> lemma "sleep"
    verb >> singular
    frame []


loveL :: AVM
loveL = do
    orth "love" >> lemma "love"
    verb >> plural
    frame [nounPhrase >> accusative]


lovesL :: AVM
lovesL = do
    orth "loves" >> lemma "love"
    verb >> singular
    frame [nounPhrase >> accusative]


eatL :: AVM
eatL = do
    orth "eat" >> lemma "eat"
    verb >> plural
    frame [nounPhrase >> accusative]


eatsL :: AVM
eatsL = do
    orth "eats" >> lemma "eat"
    verb >> singular
    frame [nounPhrase >> accusative]


tellL :: AVM
tellL = do
    orth "tell" >> lemma "tell"
    verb >> plural
    frame [nounPhrase >> accusative, sent]


tellsL :: AVM
tellsL = do
    orth "tells" >> lemma "tell"
    verb >> singular
    frame [nounPhrase >> accusative, sent]


-- | An "unidiomatic" interpretation of the word "look".  No required
-- arguments?  See also entries in the MWEs section.
lookL :: AVM
lookL = do
    orth "look" >> lemma "look"
    verb >> plural
    frame []


-- | An "unidiomatic" interpretation of the word "look".  No required
-- arguments?  See also entries in the MWEs section.
looksL :: AVM
looksL = do
    orth "looks" >> lemma "look"
    verb >> singular
    frame []


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


atL :: AVM
atL = prep >> orth "at" >> lemma "at"


upL :: AVM
upL = prep >> orth "up" >> lemma "up"


--------------------------------------------------------------------
-- Abstract rules of the grammar
--------------------------------------------------------------------


-- | Construct a rule with the given head and the given body.
mkR :: String -> AVM -> [AVM] -> Rule
mkR ruleName x xs = unjust ruleName $ do
    (is, g) <- R.compiles $ map avm (x:xs)
    (rh, rb) <- unCons is
    return $ P.mkRule rh rb g


-- | A sentence rule.
sentR :: Rule
sentR = mkR "sentR" sent
    [ nounPhrase >> nominative >> num "?num"
    , verbPhrase >> frame [] >> num "?num" ]


-- | VP -> V
--
-- We need VPs to be able to refer to elementary/token verbs only
-- (see the `lookAtR` rule).
vpVerbR :: Rule
vpVerbR = mkR "vpVerbR" hd bd where
    uni = num "?num" >> subcat "?xs"
    hd = verbPhrase >> uni
    bd = [verb >> uni]


-- | Subcategorization resolution.
subcatR :: Rule
subcatR = mkR "subcatR"
    ( verbPhrase >> num "?num" >> subcat "?xs" )
    [ verbPhrase >> num "?num" >> subcat (first "?x" >> rest "?xs")
    , label "?x" ]


-- | NP -> D + N
npDetNounR :: Rule
npDetNounR = mkR "npDetNounR" hd bd where
    hd = nounPhrase >> num "?num" >> cas "?case"
    bd = [ determiner >> num "?num"
         , noun >> num "?num" >> cas "?case" ]


-- | NP -> N (plural)
npPlNounR :: Rule
npPlNounR = mkR "npPlNounR"
    (nounPhrase >> plural >> cas "?case")
    [noun >> plural >> cas "?case"]


-- | NP -> Pronoun
npPronR :: Rule
npPronR = mkR "npPronR" 
    (nounPhrase >> num "?num" >> cas "?case")
    [pronoun >> num "?num" >> cas "?case"]


-- | NP -> Proper name
npPropNameR :: Rule
npPropNameR = mkR "npPropNameR"
    (nounPhrase >> numCas)
    [properName >> numCas]
    where numCas = num "?num" >> cas "?case"


-- | All rules of the grammar.
ruleSet :: [Rule]
ruleSet = [sentR, vpVerbR, npDetNounR, npPlNounR, npPronR, npPropNameR, subcatR]


--------------------------------------------------------------------
-- MWE rules
--
-- MWEs can be represented at the level of the grammar rules.
--
-- Assumptions:
--
-- * There is a separate level of tokens, with distinct set of classes,
--   to which MWE rules refer.  It simplifies writing rules, because we
--   don't have to worry about potential unification of the body elements
--   with complex phrases.
-- * By refering to lemmas we actually want to refer to specific tokens.
--   At this point we assume, that a lexeme can be uniquely identified
--   given it's base form.
--
-- Comments:
--
-- * MWE rules can be fired up only when corresponding "anchors"
--   are present in the text and satisfy particular conditions
--   (this is not implemented).
-- * We have to declare empty subcategorization frames in the heads
--   of the rules.  Otherwise, the verb would accept any given argument.
--------------------------------------------------------------------


-- | Phrasal verb "look at".
lookAtR :: Rule
lookAtR = mkR "lookAtR"
    ( verbPhrase >> num "?num" >> frame [] )
    [ verb >> lemma "look" >> num "?num"
    , prep >> lemma "at"
    , nounPhrase >> accusative ]
-- -- | Phrasal verb "look at"; version with subcat frame.
-- lookAtR' :: Rule
-- lookAtR' = mkR "lookAtR" hd [x1, x2] where
--     x1 = verb >> lemma "look" >> num "?num"
--     x2 = prep >> lemma "at"
--     hd = do
--         verbPhrase >> num "?num"
--         frame [nounPhrase >> accusative]


-- | For example, "look the word up [in the dictionary]".
-- We have to handle the NP argument of the verb directly within
-- the body of the rule.
-- TODO: perhaps we would like to handle a less correct(?) version,
-- "look up the word", as well?
lookUpR :: Rule
lookUpR = mkR "lookUpR"
    ( verbPhrase >> num "?num" >> frame [] )
    [ verb >> lemma "look" >> num "?num"
    , nounPhrase >> accusative
    , prep >> lemma "up" ]


mweSet :: [Rule]
mweSet = [lookAtR, lookUpR]
-- mweSet = []


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
parse :: [AVM] -> [Rule]
parse sent = parse' sent 0 (length sent)


-- | A simplified parsing function.
parse' :: [AVM] -> Int -> Int -> [Rule]
-- parse' sent = filter P.isFull $
parse' sent =
    P.parse rules $ map (compileEntry . avm) sent
  where
    rules = ruleSet ++ mweSet
    compileEntry fn = unjust ("compileEntry: " ++ show fn) $ do
        (i, g) <- R.compile fn
        return $ P.mkEntry i g
