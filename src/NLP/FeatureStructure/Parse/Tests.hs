{-# LANGUAGE OverloadedStrings #-}


module NLP.FeatureStructure.Parse.Tests
( 
-- * Lexicon

-- ** Verbs
  sleepL
, sleepsL
, loveL
, lovesL
, kickL
, kicksL
, kickedL
, eatL
, eatsL
, tellL
, tellsL
, lookL
, looksL

-- ** Misc
, lambL
, lambsL
, bucketL
, bucketsL
, sheL
, herL
, rachelL
, jacobL
, aL
, theL
, someL
, twoL
, atL
, upL

-- ** Adjectives
, whiteL
, slowL
, proverbialL


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
sent = catl "s"     -- sentence
verb = catl "v"     -- verb
noun = catl "n"     -- noun
prep = catl "prep"  -- preposition
adj = catl "adj"    -- adjective
adv = catl "adv"    -- adverb
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


kickL :: AVM
kickL = do
    orth "kick" >> lemma "kick"
    verb >> plural
    frame [nounPhrase >> accusative]


kickedL :: AVM
kickedL = do
    verb >> orth "kicked" >> lemma "kick"
    frame [nounPhrase >> accusative]


kicked'L :: AVM
kicked'L = do
    verb >> orth "kicked" >> lemma "kick"
    frame [nounPhrase >> accusative]


kicksL :: AVM
kicksL = do
    orth "kicks" >> lemma "kick"
    verb >> singular
    frame [nounPhrase >> accusative]


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
-- Lexicon: misc
--------------------------------------------------------------------


lambL :: AVM
lambL = noun >> singular >> orth "lamb"


lambsL :: AVM
lambsL = noun >> plural >> orth "lambs"


bucketL :: AVM
bucketL = noun >> singular >> orth "bucket" >> lemma "bucket"


bucketsL :: AVM
bucketsL = noun >> plural >> orth "buckets" >> lemma "bucket"


sheL :: AVM
sheL = pronoun >> singular >> nominative >> orth "she"


herL :: AVM
herL = pronoun >> singular >> accusative >> orth "her"


rachelL :: AVM
rachelL = properName >> singular >> orth "rachel"


jacobL :: AVM
jacobL = properName >> singular >> orth "jacob"


aL :: AVM
aL = determiner >> singular >> orth "a" >> lemma "a"


theL :: AVM
theL = determiner >> singular >> orth "the" >> lemma "the"


someL :: AVM
someL = determiner >> plural >> orth "some" >> lemma "some"


twoL :: AVM
twoL = determiner >> plural >> orth "two" >> lemma "two"


atL :: AVM
atL = prep >> orth "at" >> lemma "at"


upL :: AVM
upL = prep >> orth "up" >> lemma "up"


--------------------------------------------------------------------
-- Lexicon: adjectives
--------------------------------------------------------------------


whiteL :: AVM
whiteL = adj >> orth "white" >> lemma "white"


slowL :: AVM
slowL = adj >> orth "slow" >> lemma "slow"


proverbialL :: AVM
proverbialL = adj >> orth "proverbial" >> lemma "proverbial"


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
    , "?x" ]


-- | N -> Adj + N
--
-- Note, that the lemma feature of the complex noun is in fact
-- the base form of the its head.
nounAdjNounR :: Rule
nounAdjNounR = mkR "nounAdjNounR"
    (noun >> num "?num" >> lemma "?lemma")
    [adj, noun >> num "?num" >> lemma "?lemma"]


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
ruleSet =
    [ sentR, vpVerbR, npDetNounR, npPlNounR, npPronR, npPropNameR
    , subcatR, nounAdjNounR ]


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
-- * Valency frames of the sub-elements of MWE rules may not be consumed
--   in idiomatic interpretations.  For example, `kick` in `kickTheBucketR`.
-- * We have to declare empty subcategorization frames in the heads
--   of the rules.  Otherwise, the verb would accept any given argument.
-- * MWE rules can be fired up only when corresponding "anchors"
--   are present in the text and satisfy particular conditions
--   (this is not implemented yet, though).
-- * We could probably make use of the idea of tree families.  For
--   example, to describe the "kick the bucket" idiom, we could
--   construct an appropriate tree family and instantiate it with
--   appropriate anchors occuring in the idiom.
--
-- Questions:
--
-- * Is the idea of tree families really useful within the context of MWEs?
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


-- | Verbal(?) idiom "kick the bucket".
--
-- The rule does not cover:
-- * "NP will kick the bucket in a few days"
--   <- Unless "will kick" can be treated as a verb?
--
-- The rule allows:
-- * "I would like him to kick the bucket"
-- * Any adjectival modifier of the embedded noun ("bucket"),
--   in accordance with general rules.  In order to block
--   some modifications, we would need access to the modifier
--   of the "bucket" (but the same applies to TAGs?).
--   In order to block all modifications we would need to be
--   able to distinguish nouns from modified nouns.
-- * Any tense of the "kick" verb.
kickTheBucketR :: Rule
kickTheBucketR = mkR "kickTheBucketR"
    ( verbPhrase >> num "?num" >> frame [] )
    -- [ verb >> lemma "kick" >> num "?num" >> frame ["?x1"]
    [ verb >> lemma "kick" >> num "?num"
    , determiner >> lemma "the"
    -- , nounPhrase >> singular >> lemma "bucket" >> "?x1" ]
    , noun >> singular >> lemma "bucket" ]


mweSet :: [Rule]
mweSet = [lookAtR, lookUpR, kickTheBucketR]
-- mweSet = []


--------------------------------------------------------------------
-- MWE lexical experiments
--------------------------------------------------------------------


doRakWlasnych :: AVM
doRakWlasnych = do
    verb >> lemma "doręczyć"
    mkSpec subj
    mkComp [obj, objT, rak]
  where
    subj = nounPhrase
    obj  = nounPhrase >> cas dative
    objT = 
    rak  = do
        -- We need to specify "do" as well.
        ppPhrase >> plural >> lemma "ręka"  -- head' $ lemma "ręka"?
        adjunct $ lemma "własny"


videDetSac :: AVM
videDetSac = do
    verb >> lemma "vider"
    mkSpec subj
    mkComp [obj]
  where
    subj = nounPhrase >> pers "?pers" >> num "?num"
    obj  = do
        head' $ lemma "sac"
        spec  $ determiner >> pers "?pers" >> num "?num"


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
