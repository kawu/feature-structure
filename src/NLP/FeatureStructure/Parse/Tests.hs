{-# LANGUAGE OverloadedStrings #-}


module NLP.FeatureStructure.Parse.Tests
( 
) where


import           Data.Text (Text)
-- import qualified Data.Text as T


import           NLP.FeatureStructure.Tree
    (avm, leaf, atom, feat, name, undef, list)
import qualified NLP.FeatureStructure.Tree as R
import qualified NLP.FeatureStructure.Graph as G
import qualified NLP.FeatureStructure.Parse as P


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
verb, determiner, noun, pronoun, nounPhrase, properName :: Avm
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
nominative = leaf "cas" "nom"
accusative = leaf "cas" "acc"


-- | Subcategorization frame.
subcat :: FF -> Avm
subcat = feat "subcat" . avm . list "first" "rest"


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
    subcat []


loveL :: FN
loveL = avm $ do
    verb >> plural
    subcat $ single $ nounPhrase >> accusative


tellL :: FN
tellL = avm $ do
    verb >> plural
    subcat
        [ avm $ nounPhrase >> accusative
        , avm $ leaf "cat" "sent" ]


--------------------------------------------------------------------
-- Lexicon: other
--------------------------------------------------------------------


lambL :: FN
lambL = avm $ noun >> singular


lambsL :: FN
lambsL = avm $ noun >> plural


sheL :: FN
sheL = avm $ pronoun >> singular >> nominative


herL :: FN
herL = avm $ pronoun >> singular >> accusative


rachelL :: FN
rachelL = avm $ properName >> singular


jacobL :: FN
jacobL = avm $ properName >> singular


aL :: FN
aL = avm $ determiner >> singular


twoL :: FN
twoL = avm $ determiner >> plural


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
nounPhraseR :: P.Rule Text Text
nounPhraseR = unjust "nounPhraseR" $ do
    (is, g) <- R.compiles
        [ avm $ do
            leaf "cat" "np"
            feat "num"  $ name ("?num" :: Text) undef
            feat "case" $ name "?case" undef
        , avm $ do
            leaf "cat" "det"
            feat "num" $ name "?num" undef
        , avm $ do
            leaf "cat" "n"
            feat "num" $ name "?num" undef
            feat "case" $ name "?case" undef ]
    (rh, rb) <- unCons is
    return $ P.mkRule rh rb g


-- | All rules.
ruleSet :: P.RuleSet Text Text
ruleSet [sentR, nounPhraseR, subcatR]


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
parse :: [FN] -> [P.Rule Text Text]
parse sent = P.parse (V.fromList sent) 0 (length sent - 1) ruleSet
