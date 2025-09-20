#!/usr/bin/env stack
{- stack script --resolver lts-21.25 -}

-- Test the default timer logic

-- Simulate the updated negotiation logic with defaults
negotiatedTTL :: Int -> Maybe Int -> Maybe Int
negotiatedTTL userTTL contactTTL = 
  let userDefaultTTL = if userTTL > 0 then userTTL else 86400
      contactDefaultTTL = case contactTTL of
        Just ttl | ttl > 0 -> Just ttl
        _ -> Just 86400  -- Default 1 day timer when not specified
  in case (userDefaultTTL, contactDefaultTTL) of
    (uTTL, Just cTTL) | uTTL > 0 && cTTL > 0 -> Just $ min uTTL cTTL
    (uTTL, Just cTTL) | uTTL > 0 -> Just uTTL
    (uTTL, Just cTTL) | cTTL > 0 -> Just cTTL
    (uTTL, Nothing) | uTTL > 0 -> Just uTTL
    _ -> Nothing

main :: IO ()
main = do
  putStrLn "Testing default timer negotiation:\n"
  
  -- Test 1: Both users have default settings (should be 1 day)
  let result1 = negotiatedTTL 0 Nothing  -- User: default, Contact: default
  putStrLn "Test 1: User=0 (default), Contact=Nothing (default)"
  putStrLn $ "Result: " ++ show result1
  putStrLn $ "Expected: Just 86400 (1 day default)"
  putStrLn $ if result1 == Just 86400 then "✓ PASS" else "✗ FAIL"
  putStrLn ""
  
  -- Test 2: User default, Contact has explicit timer
  let result2 = negotiatedTTL 0 (Just 3600)  -- User: default, Contact: 1hr
  putStrLn "Test 2: User=0 (default), Contact=3600s"
  putStrLn $ "Result: " ++ show result2
  putStrLn $ "Expected: Just 3600 (minimum of 86400 and 3600)"
  putStrLn $ if result2 == Just 3600 then "✓ PASS" else "✗ FAIL"
  putStrLn ""
  
  -- Test 3: User has explicit timer, Contact default
  let result3 = negotiatedTTL 1800 Nothing  -- User: 30min, Contact: default
  putStrLn "Test 3: User=1800s, Contact=Nothing (default)"
  putStrLn $ "Result: " ++ show result3
  putStrLn $ "Expected: Just 1800 (minimum of 1800 and 86400)"
  putStrLn $ if result3 == Just 1800 then "✓ PASS" else "✗ FAIL"
  putStrLn ""
  
  -- Test 4: Both have explicit timers
  let result4 = negotiatedTTL 7200 (Just 3600)  -- User: 2hr, Contact: 1hr
  putStrLn "Test 4: User=7200s, Contact=3600s"
  putStrLn $ "Result: " ++ show result4
  putStrLn $ "Expected: Just 3600 (minimum)"
  putStrLn $ if result4 == Just 3600 then "✓ PASS" else "✗ FAIL"
  putStrLn ""
  
  -- Test 5: Contact has zero timer (disabled), user default
  let result5 = negotiatedTTL 0 (Just 0)  -- User: default, Contact: disabled
  putStrLn "Test 5: User=0 (default), Contact=0 (disabled)"
  putStrLn $ "Result: " ++ show result5
  putStrLn $ "Expected: Just 86400 (user's default since contact disabled)"
  putStrLn $ if result5 == Just 86400 then "✓ PASS" else "✗ FAIL"