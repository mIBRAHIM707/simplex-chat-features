#!/usr/bin/env stack
{- stack script --resolver lts-21.25 -}

-- Test negotiated TTL logic with various scenarios

-- Simulate the negotiation logic
negotiatedTTL :: Int -> Maybe Int -> Maybe Int
negotiatedTTL userDefaultTTL contactDefaultTTL = case (userDefaultTTL, contactDefaultTTL) of
  (uTTL, Just cTTL) | uTTL > 0 && cTTL > 0 -> Just $ min uTTL cTTL
  (uTTL, Just cTTL) | uTTL > 0 -> Just uTTL
  (uTTL, Just cTTL) | cTTL > 0 -> Just cTTL
  (uTTL, Nothing) | uTTL > 0 -> Just uTTL
  _ -> Nothing

main :: IO ()
main = do
  putStrLn "Testing negotiated timer logic:\n"
  
  -- Test 1: Both users have timers - should pick minimum
  let result1 = negotiatedTTL 7200 (Just 3600)  -- User: 2hr, Contact: 1hr
  putStrLn "Test 1: User=7200s, Contact=3600s"
  putStrLn $ "Result: " ++ show result1
  putStrLn $ "Expected: Just 3600 (minimum)"
  putStrLn $ if result1 == Just 3600 then "✓ PASS" else "✗ FAIL"
  putStrLn ""
  
  -- Test 2: User has timer, contact doesn't 
  let result2 = negotiatedTTL 1800 Nothing  -- User: 30min, Contact: None
  putStrLn "Test 2: User=1800s, Contact=Nothing"
  putStrLn $ "Result: " ++ show result2
  putStrLn $ "Expected: Just 1800 (user's timer)"
  putStrLn $ if result2 == Just 1800 then "✓ PASS" else "✗ FAIL"
  putStrLn ""
  
  -- Test 3: User has no timer (0), contact has timer
  let result3 = negotiatedTTL 0 (Just 3600)  -- User: None, Contact: 1hr
  putStrLn "Test 3: User=0 (no timer), Contact=3600s"
  putStrLn $ "Result: " ++ show result3
  putStrLn $ "Expected: Just 3600 (contact's timer)"
  putStrLn $ if result3 == Just 3600 then "✓ PASS" else "✗ FAIL"
  putStrLn ""
  
  -- Test 4: Neither has timer
  let result4 = negotiatedTTL 0 Nothing  -- User: None, Contact: None
  putStrLn "Test 4: User=0 (no timer), Contact=Nothing"
  putStrLn $ "Result: " ++ show result4
  putStrLn $ "Expected: Nothing (no timer)"
  putStrLn $ if result4 == Nothing then "✓ PASS" else "✗ FAIL"
  putStrLn ""
  
  -- Test 5: Both have no timer (user=0, contact=0)
  let result5 = negotiatedTTL 0 (Just 0)  -- User: None, Contact: None
  putStrLn "Test 5: User=0 (no timer), Contact=0 (no timer)"
  putStrLn $ "Result: " ++ show result5
  putStrLn $ "Expected: Nothing (no timer)"
  putStrLn $ if result5 == Nothing then "✓ PASS" else "✗ FAIL"
  putStrLn ""
  
  -- Test 6: User has shorter timer
  let result6 = negotiatedTTL 900 (Just 1800)  -- User: 15min, Contact: 30min
  putStrLn "Test 6: User=900s, Contact=1800s"
  putStrLn $ "Result: " ++ show result6
  putStrLn $ "Expected: Just 900 (minimum)"
  putStrLn $ if result6 == Just 900 then "✓ PASS" else "✗ FAIL"