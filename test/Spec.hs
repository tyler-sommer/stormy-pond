{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson

import Pond.Data

import Test.HUnit

testJSONEncodeNoDescNoFacets :: Test
testJSONEncodeNoDescNoFacets =
  TestCase $ assertEqual "No description or facets"
                         (encode Ripple { summary = "Summary", description = Nothing, facets = []})
                         "{\"summary\":\"Summary\",\"description\":null,\"facets\":[]}"

testJSONEncodeWithDescAndFacets :: Test
testJSONEncodeWithDescAndFacets =
  TestCase $ assertEqual "With description and facets"
                         (encode Ripple
                                 { summary = "A Summary"
                                 , description = Just "Description"
                                 , facets = ["simple", "interesting"]
                                 })
                         "{\"summary\":\"A Summary\",\"description\":\"Description\",\"facets\":[\"simple\",\"interesting\"]}"

testJSONDecode :: Test
testJSONDecode = do
  let ripple = Just Ripple
                    { summary = "A Summary"
                    , description = Just "Description"
                    , facets = ["simple", "interesting"]
                    }
  TestCase $ assertEqual "JSON decode"
                         (decode "{\"summary\":\"A Summary\",\"description\":\"Description\",\"facets\":[\"simple\",\"interesting\"]}")
                         ripple

main :: IO Counts
main = runTestTT $ TestList [testJSONEncodeNoDescNoFacets, testJSONEncodeWithDescAndFacets, testJSONDecode]
