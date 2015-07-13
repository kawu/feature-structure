import           Test.Tasty (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck (QuickCheckTests (..))

import qualified NLP.FeatureStructure.Graph.Tests


main :: IO ()
main = defaultMain $ opts $ testGroup "Tests"
    [ NLP.FeatureStructure.Graph.Tests.tests
    ]
  where
    opts = localOption $ QuickCheckTests 500
