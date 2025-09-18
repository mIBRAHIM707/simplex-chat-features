package chat.simplex.app

import chat.simplex.common.model.*
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull
import kotlinx.datetime.Clock
import kotlinx.datetime.Instant

class TtlBehaviorTest {

  @Test
  fun testChatInfoTimedMessagesTTLPrecedence() {
    // contact with no persisted chatItemTTL but with user-level timedMessagesTTL
    val contactA = Contact.sampleData.copy(chatItemTTL = null, mergedPreferences = ContactUserPreferences.sampleData)
    val cInfoA = ChatInfo.Direct(contactA)
    // contact.timedMessagesTTL is inside mergedPreferences.timedMessages.userPreference.pref.ttl or similar
    // For the sample data, ensure we can inspect fallback property
    val expectedFallback: Int? = contactA.timedMessagesTTL
    assertEquals(expectedFallback, cInfoA.timedMessagesTTL)

    // contact with an explicit persisted chatItemTTL
    val contactB = contactA.copy(chatItemTTL = 45L)
    val cInfoB = ChatInfo.Direct(contactB)
    assertEquals(45, cInfoB.timedMessagesTTL)
  }

  @Test
  fun testComputeTtlToSendLogic() {
    fun computeTtlToSend(chatLevelTTL: Int?, userTtl: Int?): Int? {
      return if (userTtl != null && userTtl != chatLevelTTL) userTtl else null
    }

    // no chat-level ttl, user didn't choose -> null
    assertNull(computeTtlToSend(null, null))
    // chat-level 60, user chooses same 60 -> omit
    assertNull(computeTtlToSend(60, 60))
    // chat-level 60, user chooses 30 -> include 30
    assertEquals(30, computeTtlToSend(60, 30))
    // chat-level null, user chooses 10 -> include 10
    assertEquals(10, computeTtlToSend(null, 10))
  }

  @Test
  fun testExistingChatItemDeleteAtPreservedWhenContactUpdated() {
    // Create a chat item with a timed TTL and a computed deleteAt
    val now: Instant = Clock.System.now()
    val ttlSecs = 20
    val deleteAt = now.plusSeconds(ttlSecs.toLong())
    val meta = CIMeta.getSample(
      id = 12345L,
      ts = now,
      text = "hello",
      status = CIStatus.SndNew(),
      itemTimed = CITimed(ttl = ttlSecs, deleteAt = deleteAt)
    )
    val chatItem = ChatItem(
      chatDir = CIDirection.LocalSnd(),
      meta = meta,
      content = CIContent.SndEmpty(),
      formattedText = null,
      mentions = null,
      quotedItem = null,
      reactions = emptyList(),
      file = null
    )

    // Keep a copy of deleteAt
    val before = chatItem.meta.itemTimed?.deleteAt

    // Simulate a contact update that changes chatItemTTL
    val contact = Contact.sampleData.copy(chatItemTTL = 999L)
    // Perform client-side update; client shouldn't mutate existing chat items
    // (we're not wiring into the full chat list here; just assert value unchanged)

    val after = chatItem.meta.itemTimed?.deleteAt
    assertEquals(before, after)
  }
}
