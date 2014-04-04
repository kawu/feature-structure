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
-- import qualified Data.Text as T
import qualified Data.Vector as V


import           NLP.FeatureStructure.Tree
    (avm, leaf, atom, feat, name, undef, list)
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
type Avm = R.Avm Text Text Text


--------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------


-- | Grammatical class.
sent, verb, determiner, noun, pronoun, nounPhrase, properName :: Avm
sent = leaf "cat" "s"
verb = leaf "cat" "v"
noun = leaf "cat" "n"
pronoun = leaf "cat" "pron"
determiner = leaf "cat" "d"
nounPhrase = leaf "cat" "np"
properName = leaf "cat" "propn"


-- | Number.
singular, plural :: Avm
singular = leaf "num" "sg"
plural = leaf "num" "pl"


-- | Case.
nominative, accusative :: Avm
nominative = leaf "case" "nom"
accusative = leaf "case" "acc"


-- | Orth.
orth :: Text -> Avm
orth x = leaf "orth" x


-- | Subcategorization frame.
subcat :: FF -> Avm
subcat = feat "subcat" . list "nil" "first" "rest"


-- | A singleton forest.
single :: Avm -> FF
single x = [avm x]


-- -- | A named feature.
-- named :: Text -> Text -> FN -> Avm
-- named x y = feat x . name y
-- 
-- 
-- -- | A dead end.
-- dead :: Text -> Text ->  Avm
-- dead x y = named x y undef


--------------------------------------------------------------------
-- Lexicon: verbs
--------------------------------------------------------------------


sleepL :: FN
sleepL = avm $ do
    verb >> plural
    orth "sleep"
    subcat []


sleepsL :: FN
sleepsL = avm $ do
    verb >> singular
    orth "sleeps"
    subcat []


loveL :: FN
loveL = avm $ do
    verb >> plural
    orth "love"
    subcat $ single $ nounPhrase >> accusative


lovesL :: FN
lovesL = avm $ do
    verb >> singular
    orth "loves"
    subcat $ single $ nounPhrase >> accusative


eatL :: FN
eatL = avm $ do
    verb >> plural
    orth "eat"
    subcat $ single $ nounPhrase >> accusative


eatsL :: FN
eatsL = avm $ do
    verb >> singular
    orth "eats"
    subcat $ single $ nounPhrase >> accusative


tellL :: FN
tellL = avm $ do
    verb >> plural
    orth "tell"
    subcat
        [ avm $ nounPhrase >> accusative
        , avm sent ]


tellsL :: FN
tellsL = avm $ do
    verb >> singular
    orth "tells"
    subcat
        [ avm $ nounPhrase >> accusative
        , avm sent ]


--------------------------------------------------------------------
-- Lexicon: other
--------------------------------------------------------------------


lambL :: FN
lambL = avm $ noun >> singular >> orth "lamb"


lambsL :: FN
lambsL = avm $ noun >> plural >> orth "lambs"


sheL :: FN
sheL = avm $ pronoun >> singular >> nominative >> orth "she"


herL :: FN
herL = avm $ pronoun >> singular >> accusative >> orth "her"


rachelL :: FN
rachelL = avm $ properName >> singular >> orth "rachel"


jacobL :: FN
jacobL = avm $ properName >> singular >> orth "jacob"


aL :: FN
aL = avm $ determiner >> singular >> orth "a"


twoL :: FN
twoL = avm $ determiner >> plural >> orth "two"


--------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------


-- | A sentence rule.
sentR :: P.Rule Text Text
sentR = unjust "sentR" $ do
    (is, g) <- R.compiles
        [ avm $ do
            leaf "cat" "s"
        , avm $ do
            leaf "cat" "np"
            feat "num" $ name "?num" undef
            nominative
        , avm $ do
            verb
            feat "num" $ name "?num" undef
            subcat [] ]
    (rh, rb) <- unCons is
    return $ P.mkRule rh rb g


-- | Subcategorization resolution.
subcatR :: P.Rule Text Text
subcatR = unjust "subcatR" $ do
    (is, g) <- R.compiles
        [ avm $ do
            verb
            feat "num" $ name "?num" undef
            feat "subcat" $ name "?args" undef
        , avm $ do
            verb
            feat "num" $ name "?num" undef
            feat "subcat" $ avm $ do
                feat "first" $ name "?arg1" undef
                feat "rest" $ name "?args" undef
        , name "?arg1" undef ]
    (rh, rb) <- unCons is
    return $ P.mkRule rh rb g


-- | NP -> D + N
npDetNounR :: P.Rule Text Text
npDetNounR = unjust "npDetNounR" $ do
    (is, g) <- R.compiles
        [ avm $ do
            nounPhrase
            feat "num"  $ name "?num" undef
            feat "case" $ name "?case" undef
        , avm $ do
            determiner
            feat "num" $ name "?num" undef
        , avm $ do
            noun
            feat "num" $ name "?num" undef
            feat "case" $ name "?case" undef ]
    (rh, rb) <- unCons is
    return $ P.mkRule rh rb g


-- | NP -> N (plural)
npPlNounR :: P.Rule Text Text
npPlNounR = unjust "npPlNounR" $ do
    (is, g) <- R.compiles
        [ avm $ do
            nounPhrase >> plural
            feat "case" $ name "?case" undef
        , avm $ do
            noun >> plural
            feat "case" $ name "?case" undef ]
    (rh, rb) <- unCons is
    return $ P.mkRule rh rb g


-- | NP -> Pronoun
npPronR :: P.Rule Text Text
npPronR = unjust "npPronR" $ do
    (is, g) <- R.compiles
        [ avm $ do
            nounPhrase
            feat "num"  $ name "?num" undef
            feat "case" $ name "?case" undef
        , avm $ do
            pronoun
            feat "num" $ name "?num" undef
            feat "case" $ name "?case" undef ]
    (rh, rb) <- unCons is
    return $ P.mkRule rh rb g


-- | NP -> Proper name
npPropNameR :: P.Rule Text Text
npPropNameR = unjust "npPropNameR" $ do
    (is, g) <- R.compiles
        [ avm $ do
            nounPhrase
            feat "num"  $ name "?num" undef
            feat "case" $ name "?case" undef
        , avm $ do
            properName
            feat "num" $ name "?num" undef
            feat "case" $ name "?case" undef ]
    (rh, rb) <- unCons is
    return $ P.mkRule rh rb g


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


-- -- | Reidentify sentence and rules. 
-- reidData
--     :: [P.Rule Text Text] -> [FN]
--     -> ([[P.Rule Text Text]], [[P.Rule Text Text]])
-- reidData rs ws = Reid.runReid $ do
--     -- Rules
--     rs' <- replicateM (length ws) $ do
--         forM rs $ \x -> do
--             Reid.split
--             Reid.reidRule x
--     -- Words
--     ws' <- forM ws $ \x -> do
--         Reid.split
--         y <- Reid.reidRule $ compileEntry x
--         return [y]
--     return (rs', ws')
--   where
--     -- Compile entry
--     compileEntry fn = unjust ("compileEntry: " ++ show fn) $ do
--         (i, g) <- R.compile fn
--         return $ P.mkEntry i g


-- | A simplified parsing function.
parse :: [FN] -> [P.Rule Text Text]
parse sent = parse' sent 0 (length sent)


-- | A simplified parsing function.
parse' :: [FN] -> Int -> Int -> [P.Rule Text Text]
-- parse sent = filter P.isFull $ P.parse
parse' sent =
    P.parse ruleSet (map compileEntry sent)
    -- 0 (length sent)
  where
--     -- Reidentified rules and words
--     (rulesReided, sentReided) = reidData ruleSet sent
    compileEntry fn = unjust ("compileEntry: " ++ show fn) $ do
        (i, g) <- R.compile fn
        return $ P.mkEntry i g

--     -- Reidentified rules
--     rulesReided = Reid.runReid . replicateM (length sent) $ do
--         Reid.split >> mapM Reid.reidRule ruleSet
--     -- Reidentified sentence
--     sentReided = map (:[]) . Reid.runReid $ do
--         mapM (Reid.reidRule . compileEntry) sent
