module Main (main) where

import Test.Tasty

import qualified Test.FusionResolverCardano

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Fusion Resolver Cardano"
    [ Test.FusionResolverCardano.tests
    ]
