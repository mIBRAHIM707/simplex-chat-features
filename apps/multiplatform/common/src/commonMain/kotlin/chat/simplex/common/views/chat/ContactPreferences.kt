package chat.simplex.common.views.chat

import InfoRow
import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.PreferenceToggle
import chat.simplex.common.model.*
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.chatModel
import chat.simplex.res.MR
import kotlinx.coroutines.*

@Composable
fun ContactPreferencesView(
  m: ChatModel,
  user: User,
  rhId: Long?,
  contactId: Long,
  close: () -> Unit,
) {
  val contact = remember { derivedStateOf { (m.getContactChat(contactId)?.chatInfo as? ChatInfo.Direct)?.contact } }
  val ct = contact.value ?: return
  var featuresAllowed by rememberSaveable(ct, user, stateSaver = serializableSaver()) { mutableStateOf(contactUserPrefsToFeaturesAllowed(ct.mergedPreferences)) }
  var currentFeaturesAllowed by rememberSaveable(ct, user, stateSaver = serializableSaver()) { mutableStateOf(featuresAllowed) }

  fun savePrefs(afterSave: () -> Unit = {}) {
    withBGApi {
      val prefs = contactFeaturesAllowedToPrefs(featuresAllowed)
      val toContact = m.controller.apiSetContactPrefs(rhId, ct.contactId, prefs)
      if (toContact != null) {
        withContext(Dispatchers.Main) {
          chatModel.chatsContext.updateContact(rhId, toContact)
          currentFeaturesAllowed = featuresAllowed
        }
      }
      afterSave()
    }
  }
  ModalView(
    close = {
      if (featuresAllowed == currentFeaturesAllowed) close()
      else showUnsavedChangesAlert({ savePrefs(close) }, close)
    },
  ) {
    ContactPreferencesLayout(
      featuresAllowed,
      currentFeaturesAllowed,
      user,
      ct,
      applyPrefs = { prefs ->
        featuresAllowed = prefs
      },
      reset = {
        featuresAllowed = currentFeaturesAllowed
      },
      savePrefs = ::savePrefs,
    )
  }
}

@Composable
private fun ContactPreferencesLayout(
  featuresAllowed: ContactFeaturesAllowed,
  currentFeaturesAllowed: ContactFeaturesAllowed,
  user: User,
  contact: Contact,
  applyPrefs: (ContactFeaturesAllowed) -> Unit,
  reset: () -> Unit,
  savePrefs: () -> Unit,
) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.contact_preferences))
    val timedMessages: MutableState<Boolean> = remember(featuresAllowed) { mutableStateOf(featuresAllowed.timedMessagesAllowed) }
    val onTTLUpdated = { ttl: Int? ->
      applyPrefs(featuresAllowed.copy(timedMessagesTTL = ttl))
    }
    TimedMessagesFeatureSection(
      featuresAllowed = featuresAllowed, 
      pref = contact.mergedPreferences.timedMessages, 
      allowFeature = timedMessages,
      contact = contact,
      rhId = rhId,
      onTTLUpdated = onTTLUpdated
    ) { allowed, ttl ->
      applyPrefs(featuresAllowed.copy(timedMessagesAllowed = allowed, timedMessagesTTL = ttl ?: currentFeaturesAllowed.timedMessagesTTL))
    }
    SectionDividerSpaced(true)
    val allowFullDeletion: MutableState<ContactFeatureAllowed> = remember(featuresAllowed) { mutableStateOf(featuresAllowed.fullDelete) }
    FeatureSection(ChatFeature.FullDelete, user.fullPreferences.fullDelete.allow, contact.mergedPreferences.fullDelete, allowFullDeletion) {
      applyPrefs(featuresAllowed.copy(fullDelete = it))
    }
    SectionDividerSpaced(true)
    val allowReactions: MutableState<ContactFeatureAllowed> = remember(featuresAllowed) { mutableStateOf(featuresAllowed.reactions) }
    FeatureSection(ChatFeature.Reactions, user.fullPreferences.reactions.allow, contact.mergedPreferences.reactions, allowReactions) {
      applyPrefs(featuresAllowed.copy(reactions = it))
    }
    SectionDividerSpaced(true)
    val allowVoice: MutableState<ContactFeatureAllowed> = remember(featuresAllowed) { mutableStateOf(featuresAllowed.voice) }
    FeatureSection(ChatFeature.Voice, user.fullPreferences.voice.allow, contact.mergedPreferences.voice, allowVoice) {
      applyPrefs(featuresAllowed.copy(voice = it))
    }
    SectionDividerSpaced(true)
    val allowCalls: MutableState<ContactFeatureAllowed> = remember(featuresAllowed) { mutableStateOf(featuresAllowed.calls) }
    FeatureSection(ChatFeature.Calls, user.fullPreferences.calls.allow, contact.mergedPreferences.calls, allowCalls) {
      applyPrefs(featuresAllowed.copy(calls = it))
    }
    SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = false)
    ResetSaveButtons(
      reset = reset,
      save = savePrefs,
      disabled = featuresAllowed == currentFeaturesAllowed
    )
    SectionBottomSpacer()
  }
}

@Composable
private fun FeatureSection(
  feature: ChatFeature,
  userDefault: FeatureAllowed,
  pref: ContactUserPreference,
  allowFeature: State<ContactFeatureAllowed>,
  onSelected: (ContactFeatureAllowed) -> Unit
) {
  val enabled = FeatureEnabled.enabled(
    feature.asymmetric,
    user = SimpleChatPreference(allow = allowFeature.value.allowed),
    contact = pref.contactPreference
  )

  SectionView(
    feature.text.uppercase(),
    icon = feature.iconFilled(),
    iconTint = if (enabled.forUser) SimplexGreen else if (enabled.forContact) WarningYellow else Color.Red,
    leadingIcon = true,
  ) {
    ExposedDropDownSettingRow(
      generalGetString(MR.strings.chat_preferences_you_allow),
      ContactFeatureAllowed.values(userDefault).map { it to it.text },
      allowFeature,
      icon = null,
      onSelected = onSelected
    )
    InfoRow(
      generalGetString(MR.strings.chat_preferences_contact_allows),
      pref.contactPreference.allow.text
    )
  }
  SectionTextFooter(feature.enabledDescription(enabled))
}

@Composable
private fun TimedMessagesFeatureSection(
  featuresAllowed: ContactFeaturesAllowed,
  pref: ContactUserPreferenceTimed,
  allowFeature: State<Boolean>,
  contact: Contact,
  rhId: Long?,
  onTTLUpdated: (Int?) -> Unit,
  onSelected: (Boolean, Int?) -> Unit
) {
  val enabled = FeatureEnabled.enabled(
    ChatFeature.TimedMessages.asymmetric,
    user = TimedMessagesPreference(allow = if (allowFeature.value) FeatureAllowed.YES else FeatureAllowed.NO),
    contact = pref.contactPreference
  )

  SectionView(
    ChatFeature.TimedMessages.text.uppercase(),
    icon = ChatFeature.TimedMessages.iconFilled(),
    iconTint = if (enabled.forUser) SimplexGreen else if (enabled.forContact) WarningYellow else Color.Red,
    leadingIcon = true,
  ) {
    PreferenceToggle(
      generalGetString(MR.strings.chat_preferences_you_allow),
      checked = allowFeature.value,
    ) { allow ->
      onSelected(allow, if (allow) featuresAllowed.timedMessagesTTL ?: 86400 else null)
    }
    InfoRow(
      generalGetString(MR.strings.chat_preferences_contact_allows),
      pref.contactPreference.allow.text
    )
    if (featuresAllowed.timedMessagesAllowed) {
      val ttl = rememberSaveable(featuresAllowed.timedMessagesTTL) { mutableStateOf(featuresAllowed.timedMessagesTTL) }
      DropdownCustomTimePickerSettingRow(
        selection = ttl,
        propagateExternalSelectionUpdate = true, // for Reset
        label = generalGetString(MR.strings.delete_after),
        dropdownValues = TimedMessagesPreference.ttlValues,
        customPickerTitle = generalGetString(MR.strings.delete_after),
        customPickerConfirmButtonText = generalGetString(MR.strings.custom_time_picker_select),
        onSelected = onTTLUpdated
      )
    } else if (pref.contactPreference.allow == FeatureAllowed.YES || pref.contactPreference.allow == FeatureAllowed.ALWAYS) {
      InfoRow(generalGetString(MR.strings.delete_after), timeText(pref.contactPreference.ttl))
    }
    
    // Show current chat timer (actual negotiated timer)
    SectionDividerSpaced(false)
    Text(
      text = "CURRENT CHAT TIMER",
      style = MaterialTheme.typography.body2,
      color = MaterialTheme.colors.secondary
    )
    val currentChatTimer = contact.chatItemTTL
    InfoRow(
      title = "Messages disappear after",
      value = currentChatTimer?.let { SimpleXAPI.formatTimerDuration(it) } ?: "Off"
    )
    
    // Allow manual override of chat timer
    var showTimerDialog by rememberSaveable { mutableStateOf(false) }
    SectionItemView(
      click = { showTimerDialog = true }
    ) {
      Row(
        modifier = Modifier.fillMaxWidth(),
        horizontalArrangement = Arrangement.SpaceBetween,
        verticalAlignment = Alignment.CenterVertically
      ) {
        Text("Override chat timer")
        Text(
          text = "Change",
          color = MaterialTheme.colors.primary
        )
      }
    }
    
    if (showTimerDialog) {
      TimerPickerDialog(
        currentTimer = currentChatTimer,
        onTimerSelected = { newTimer ->
          // Update chat timer
          withBGApi {
            val updatedContact = chatModel.controller.apiSetContactTimer(rhId, contact.contactId, newTimer)
            if (updatedContact != null) {
              withContext(Dispatchers.Main) {
                chatModel.chatsContext.updateContact(rhId, updatedContact)
              }
            }
          }
          showTimerDialog = false
        },
        onDismiss = { showTimerDialog = false }
      )
    }
  }
  SectionTextFooter(ChatFeature.TimedMessages.enabledDescription(enabled))
}

@Composable
private fun ResetSaveButtons(reset: () -> Unit, save: () -> Unit, disabled: Boolean) {
  SectionView {
    SectionItemView(reset, disabled = disabled) {
      Text(stringResource(MR.strings.reset_verb), color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
    }
    SectionItemView(save, disabled = disabled) {
      Text(stringResource(MR.strings.save_and_notify_contact), color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
    }
  }
}

@Composable
private fun TimerPickerDialog(
  currentTimer: Long?,
  onTimerSelected: (Long?) -> Unit,
  onDismiss: () -> Unit
) {
  val timerOptions = listOf(
    null to "Off",
    60L to "1 minute",
    300L to "5 minutes", 
    1800L to "30 minutes",
    3600L to "1 hour",
    28800L to "8 hours",
    86400L to "1 day",
    604800L to "1 week",
    2592000L to "1 month",
    31536000L to "1 year"
  )

  AlertDialog(
    onDismissRequest = onDismiss,
    title = { Text("Select Timer Duration") },
    text = {
      LazyColumn {
        items(timerOptions) { (value, label) ->
          SectionItemView(
            click = { onTimerSelected(value) }
          ) {
            Row(
              modifier = Modifier.fillMaxWidth(),
              horizontalArrangement = Arrangement.SpaceBetween,
              verticalAlignment = Alignment.CenterVertically
            ) {
              Text(label)
              if (value == currentTimer) {
                Text("✓", color = MaterialTheme.colors.primary)
              }
            }
          }
        }
      }
    },
    confirmButton = {
      TextButton(onClick = onDismiss) {
        Text("Cancel")
      }
    }
  )
}

private fun showUnsavedChangesAlert(save: () -> Unit, revert: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(MR.strings.save_preferences_question),
    confirmText = generalGetString(MR.strings.save_and_notify_contact),
    dismissText = generalGetString(MR.strings.exit_without_saving),
    onConfirm = save,
    onDismiss = revert,
  )
}
