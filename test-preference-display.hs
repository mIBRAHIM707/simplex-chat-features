#!/usr/bin/env stack
{- stack script --resolver lts-21.25 -}

-- Test to verify current behavior with timer preferences

data FeatureAllowed = FAYes | FANo deriving (Show, Eq)
data TimedMessagesPreference = TimedMessagesPreference 
  { allow :: FeatureAllowed
  , ttl :: Maybe Int 
  } deriving (Show, Eq)

data ContactUserPreference = ContactUserPreference
  { enabled :: (Bool, Bool)  -- (forUser, forContact)
  , userPreference :: TimedMessagesPreference
  , contactPreference :: TimedMessagesPreference
  } deriving (Show, Eq)

-- Mock the FeatureEnabled calculation
calculateEnabled :: TimedMessagesPreference -> TimedMessagesPreference -> (Bool, Bool)
calculateEnabled userPref contactPref = 
  let userAllows = allow userPref == FAYes
      contactAllows = allow contactPref == FAYes
  in (userAllows, contactAllows)

-- Test current vs fixed behavior
main :: IO ()
main = do
  putStrLn "Testing timer preference display issues:\n"
  
  -- Current behavior: User negotiates timer but contact preferences aren't set
  putStrLn "=== Current Behavior ==="
  let userPref = TimedMessagesPreference {allow = FAYes, ttl = Just 3600}
      contactPref = TimedMessagesPreference {allow = FANo, ttl = Nothing}
      enabled = calculateEnabled userPref contactPref
      currentPrefs = ContactUserPreference enabled userPref contactPref
  
  putStrLn $ "User preference: " ++ show userPref
  putStrLn $ "Contact preference: " ++ show contactPref  
  putStrLn $ "Enabled (forUser, forContact): " ++ show enabled
  putStrLn $ "Result: Timer shows as 'off' even though negotiated"
  putStrLn ""
  
  -- Fixed behavior: Both users have timer enabled after negotiation
  putStrLn "=== Fixed Behavior ==="
  let fixedContactPref = TimedMessagesPreference {allow = FAYes, ttl = Just 3600}
      fixedEnabled = calculateEnabled userPref fixedContactPref
      fixedPrefs = ContactUserPreference fixedEnabled userPref fixedContactPref
  
  putStrLn $ "User preference: " ++ show userPref
  putStrLn $ "Contact preference (fixed): " ++ show fixedContactPref
  putStrLn $ "Enabled (forUser, forContact): " ++ show fixedEnabled
  putStrLn $ "Result: Timer shows as 'enabled' - both users can use the timer"
  putStrLn ""
  
  putStrLn "=== Summary ==="
  putStrLn "The issue is that when a timer is negotiated, both users need to have"
  putStrLn "their preferences set to FAYes for the UI to show the timer as enabled."
  putStrLn "Currently only the initiating user has proper preferences set."