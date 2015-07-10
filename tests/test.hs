import           Test.Tasty (defaultMain, testGroup)

import qualified NLP.FeatureStructure.Graph.Tests


main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ NLP.FeatureStructure.Graph.Tests.tests
    ]
