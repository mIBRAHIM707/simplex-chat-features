#!/usr/bin/env stack
-- stack --resolver lts-18.21 script

{-# LANGUAGE OverloadedStrings #-}

-- Simple test script to verify timer negotiation logic
module TestTimerLogic where

-- Mock the essential types we need
data User = User { userDefaultTimerTTL :: Int } deriving (Show, Eq)
data Contact = Contact { chatItemTTL :: Maybe Int } deriving (Show, Eq)  
data Profile = Profile { profileDefaultTimerTTL :: Maybe Int } deriving (Show, Eq)

-- Test the negotiation logic from Direct.hs
testNegotiateTimer :: User -> Profile -> Maybe Int
testNegotiateTimer user profile = 
  let userDefaultTTL = userDefaultTimerTTL user
      contactDefaultTTL = profileDefaultTimerTTL profile
  in case (userDefaultTTL, contactDefaultTTL) of
       (uTTL, Just cTTL) -> Just $ min uTTL cTTL
       (uTTL, Nothing) -> Just uTTL

-- Test cases
testCases :: [(String, User, Profile, Maybe Int)]
testCases = 
  [ ("Both have timers - should pick minimum", 
     User 3600, Profile (Just 7200), Just 3600)
  , ("User has timer, contact doesn't - should use user's", 
     User 1800, Profile Nothing, Just 1800) 
  , ("User has shorter timer", 
     User 900, Profile (Just 3600), Just 900)
  , ("Contact has shorter timer", 
     User 7200, Profile (Just 1800), Just 1800)
  ]

runTests :: IO ()
runTests = do
  putStrLn "Testing timer negotiation logic:"
  mapM_ runTest testCases
  where
    runTest (desc, user, profile, expected) = do
      let result = testNegotiateTimer user profile
      let status = if result == expected then "✓ PASS" else "✗ FAIL"
      putStrLn $ status ++ " - " ++ desc
      putStrLn $ "  Expected: " ++ show expected ++ ", Got: " ++ show result

main :: IO ()
main = runTests