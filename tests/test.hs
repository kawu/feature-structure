import           Test.Tasty (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck (QuickCheckTests (..))

import qualified NLP.FeatureStructure.Graph.Tests
import qualified NLP.FeatureStructure.Unify.Tests


main :: IO ()
main = defaultMain $ opts $ testGroup "Tests"
    [ NLP.FeatureStructure.Graph.Tests.tests
    , NLP.FeatureStructure.Unify.Tests.tests
    ]
  where
    opts = localOption $ QuickCheckTests 500
