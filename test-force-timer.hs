#!/usr/bin/env stack
{- stack script --resolver lts-21.25 -}

-- Test forcing the timer to be enabled

data FeatureAllowed = FAYes | FANo deriving (Show, Eq)
data TimedMessagesPreference = TimedMessagesPreference 
  { allow :: FeatureAllowed
  , ttl :: Maybe Int 
  } deriving (Show, Eq)

-- Simulate the forced preference logic
forceTimerEnabled :: Maybe Int -> Maybe TimedMessagesPreference -> Maybe TimedMessagesPreference
forceTimerEnabled negotiatedTTL profilePref = case negotiatedTTL of
  Just _ -> Just $ TimedMessagesPreference {allow = FAYes, ttl = Nothing}
  Nothing -> profilePref

main :: IO ()
main = do
  putStrLn "Testing forced timer enablement:\n"
  
  -- Test 1: Contact has no preferences, we have negotiated timer
  let result1 = forceTimerEnabled (Just 3600) Nothing
  putStrLn "Test 1: Negotiated=3600s, Contact=Nothing"
  putStrLn $ "Result: " ++ show result1
  putStrLn $ "Expected: Just TimedMessagesPreference {allow = FAYes, ttl = Nothing}"
  putStrLn $ if result1 == Just (TimedMessagesPreference FAYes Nothing) then "✓ PASS" else "✗ FAIL"
  putStrLn ""
  
  -- Test 2: Contact has preferences disabled, we have negotiated timer
  let result2 = forceTimerEnabled (Just 1800) (Just $ TimedMessagesPreference FANo (Just 600))
  putStrLn "Test 2: Negotiated=1800s, Contact=FANo"
  putStrLn $ "Result: " ++ show result2
  putStrLn $ "Expected: Just TimedMessagesPreference {allow = FAYes, ttl = Nothing}"
  putStrLn $ if result2 == Just (TimedMessagesPreference FAYes Nothing) then "✓ PASS" else "✗ FAIL"
  putStrLn ""
  
  -- Test 3: No negotiated timer
  let result3 = forceTimerEnabled Nothing (Just $ TimedMessagesPreference FANo (Just 600))
  putStrLn "Test 3: Negotiated=Nothing, Contact=FANo"
  putStrLn $ "Result: " ++ show result3
  putStrLn $ "Expected: Just TimedMessagesPreference {allow = FANo, ttl = Just 600}"
  putStrLn $ if result3 == Just (TimedMessagesPreference FANo (Just 600)) then "✓ PASS" else "✗ FAIL"