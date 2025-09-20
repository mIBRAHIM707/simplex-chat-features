#!/usr/bin/env stack
{- stack script --resolver lts-21.25 -}

-- Test to verify chatItemTTL logic

data Contact = Contact 
  { chatItemTTL :: Maybe Int 
  , otherField :: String
  } deriving (Show, Eq)

-- Simulate the contactTimedTTL logic
contactTimedTTL :: Contact -> Maybe (Maybe Int)
contactTimedTTL Contact {chatItemTTL = cTTL}
  -- If a chat-level TTL is persisted for this contact, prefer it (feature enabled with that TTL)
  | Just ttl64 <- cTTL = Just (Just ttl64)
  | otherwise = Nothing  -- Simplified version

main :: IO ()
main = do
  putStrLn "Testing chatItemTTL priority logic:"
  
  -- Test 1: Contact with chatItemTTL set
  let contact1 = Contact { chatItemTTL = Just 3600, otherField = "test" }
      result1 = contactTimedTTL contact1
  
  putStrLn $ "Contact with chatItemTTL = Just 3600"
  putStrLn $ "contactTimedTTL result: " ++ show result1
  putStrLn $ "Expected: Just (Just 3600)"
  if result1 == Just (Just 3600)
    then putStrLn "✓ PASS - chatItemTTL takes priority"
    else putStrLn "✗ FAIL - chatItemTTL not working"
  
  putStrLn ""
  
  -- Test 2: Contact without chatItemTTL  
  let contact2 = Contact { chatItemTTL = Nothing, otherField = "test" }
      result2 = contactTimedTTL contact2
  
  putStrLn $ "Contact with chatItemTTL = Nothing"
  putStrLn $ "contactTimedTTL result: " ++ show result2
  putStrLn $ "Expected: Nothing"
  if result2 == Nothing
    then putStrLn "✓ PASS - No timer when chatItemTTL is Nothing"
    else putStrLn "✗ FAIL - Should be Nothing"