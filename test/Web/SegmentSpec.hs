{-# LANGUAGE OverloadedStrings #-}

module Web.SegmentSpec where

import Test.Hspec

import Data.Time (getCurrentTime)
import System.Environment
import Web.Segment

import qualified Data.UUID.V4 as UUID

spec :: Spec
spec = describe "segment tests" $ do
  it "simple identify + track signup" $ do
    key <- getEnv "SEGMENT_KEY"
    runner <- mkRunner key
    u <- UUID.nextRandom
    t <- getCurrentTime
    print (key, u, t)
    let ident = FullMsg emptyFreeform emptyIdentify (emptyCommonMsg t $ Anonymous "rhabdo the clown") emptyFreeform
        track = FullMsg emptyFreeform (Track "signup" emptyTrackProperties) (emptyCommonMsg t $ Anonymous "rhabdo the clown") emptyFreeform
    response <- runner (BatchedMsg u [ident, track] t)
    print response
    response `shouldBe` Right SegmentResponse

  it "should also work for all the other things that i haven't done yet" $ do
    pendingWith "TODO"
    1 `shouldBe` 2
