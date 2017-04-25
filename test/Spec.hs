{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson

import Pond.Data

import Test.HUnit

testJSONEncodeNoDescNoReflections :: Test
testJSONEncodeNoDescNoReflections =
  TestCase $ assertEqual "No description or reflections"
                         (encode Ripple { summary = "Summary", description = Nothing, reflections = []})
                         "{\"summary\":\"Summary\",\"description\":null,\"reflections\":[]}"

testJSONEncodeWithDescAndReflections :: Test
testJSONEncodeWithDescAndReflections =
  TestCase $ assertEqual "With description and reflections"
                         (encode Ripple
                                 { summary = "A Summary"
                                 , description = Just "Description"
                                 , reflections = ["simple", "interesting"]
                                 })
                         "{\"summary\":\"A Summary\",\"description\":\"Description\",\"reflections\":[{\"name\":\"simple\",\"group\":null},{\"name\":\"interesting\",\"group\":null}]}"

testJSONDecode :: Test
testJSONDecode = do
  let ripple = Just Ripple
                    { summary = "A Summary"
                    , description = Just "Description"
                    , reflections = ["simple", "interesting"]
                    }
  TestCase $ assertEqual "JSON decode"
                         (decode "{\"summary\":\"A Summary\",\"description\":\"Description\",\"reflections\":[{\"name\":\"simple\",\"group\":null},{\"name\":\"interesting\",\"group\":null}]}")
                         ripple

main :: IO Counts
main = runTestTT $ TestList [testJSONEncodeNoDescNoReflections, testJSONEncodeWithDescAndReflections, testJSONDecode]
