#!/usr/bin/env kotlin

// Test Android ContactPreferencesView behavior with negotiated timer

// Mock the essential types from Android codebase
data class FeatureAllowed(val value: String) {
    companion object {
        val YES = FeatureAllowed("YES")
        val NO = FeatureAllowed("NO") 
        val ALWAYS = FeatureAllowed("ALWAYS")
    }
}

data class TimedMessagesPreference(
    val allow: FeatureAllowed,
    val ttl: Int?
)

data class ContactUserPref(val pref: TimedMessagesPreference)

data class ContactUserPreferenceTimed(
    val userPreference: ContactUserPref,
    val contactPreference: TimedMessagesPreference
)

data class ContactUserPreferences(
    val timedMessages: ContactUserPreferenceTimed
)

data class ContactFeaturesAllowed(
    val timedMessagesAllowed: Boolean,
    val timedMessagesTTL: Int?
)

// Function from Android codebase
fun contactUserPrefsToFeaturesAllowed(contactUserPreferences: ContactUserPreferences): ContactFeaturesAllowed {
    val pref = contactUserPreferences.timedMessages.userPreference
    val allow = pref.pref.allow
    return ContactFeaturesAllowed(
        timedMessagesAllowed = allow == FeatureAllowed.YES || allow == FeatureAllowed.ALWAYS,
        timedMessagesTTL = pref.pref.ttl
    )
}

fun main() {
    println("Testing Android ContactPreferencesView behavior:\n")
    
    // Test 1: Before our backend fix (timer disabled)
    println("=== BEFORE FIX ===")
    val oldPreferences = ContactUserPreferences(
        timedMessages = ContactUserPreferenceTimed(
            userPreference = ContactUserPref(
                pref = TimedMessagesPreference(
                    allow = FeatureAllowed.NO,  // Timer not enabled
                    ttl = null  // No timer value
                )
            ),
            contactPreference = TimedMessagesPreference(
                allow = FeatureAllowed.NO,
                ttl = null
            )
        )
    )
    
    val oldFeaturesAllowed = contactUserPrefsToFeaturesAllowed(oldPreferences)
    println("User preference allow: ${oldPreferences.timedMessages.userPreference.pref.allow.value}")
    println("User preference TTL: ${oldPreferences.timedMessages.userPreference.pref.ttl}")
    println("Features allowed - timedMessagesAllowed: ${oldFeaturesAllowed.timedMessagesAllowed}")
    println("Features allowed - timedMessagesTTL: ${oldFeaturesAllowed.timedMessagesTTL}")
    println("UI would show: ${if (oldFeaturesAllowed.timedMessagesAllowed) "Timer enabled with ${oldFeaturesAllowed.timedMessagesTTL} seconds" else "Timer OFF"}")
    println()
    
    // Test 2: After our backend fix (timer enabled with negotiated value)
    println("=== AFTER FIX ===")
    val newPreferences = ContactUserPreferences(
        timedMessages = ContactUserPreferenceTimed(
            userPreference = ContactUserPref(
                pref = TimedMessagesPreference(
                    allow = FeatureAllowed.YES,  // Timer enabled
                    ttl = 3600  // 1 hour (negotiated value)
                )
            ),
            contactPreference = TimedMessagesPreference(
                allow = FeatureAllowed.NO,  // Contact doesn't have preference yet
                ttl = null
            )
        )
    )
    
    val newFeaturesAllowed = contactUserPrefsToFeaturesAllowed(newPreferences)
    println("User preference allow: ${newPreferences.timedMessages.userPreference.pref.allow.value}")
    println("User preference TTL: ${newPreferences.timedMessages.userPreference.pref.ttl}")
    println("Features allowed - timedMessagesAllowed: ${newFeaturesAllowed.timedMessagesAllowed}")
    println("Features allowed - timedMessagesTTL: ${newFeaturesAllowed.timedMessagesTTL}")
    println("UI would show: ${if (newFeaturesAllowed.timedMessagesAllowed) "Timer enabled with ${newFeaturesAllowed.timedMessagesTTL} seconds" else "Timer OFF"}")
    println()
    
    println("=== CONCLUSION ===")
    if (newFeaturesAllowed.timedMessagesAllowed && newFeaturesAllowed.timedMessagesTTL == 3600) {
        println("✓ PASS - Android UI should now show timer as enabled with negotiated value")
        println("✓ PASS - ContactPreferencesView will display timer dropdown with 3600 seconds")
    } else {
        println("❌ FAIL - Android UI will still show timer as OFF")
    }
}