# Home Assistant and Music Assistant: settings that live outside this repo

The voice assistant on icarus depends on a handful of settings that Home
Assistant and Music Assistant keep in their own storage, under
`/var/lib/hass/.storage` and `/var/lib/private/music-assistant`. Both are
persisted, so they survive restarts and reboots, but nothing here recreates
them. After a fresh install, set them again by hand.

## The Eris voice pipeline

Settings → Voice assistants → Eris:

| Stage | Entity | Runs on |
|---|---|---|
| Speech-to-text | `stt.whisper_cpp` (wyoming, `eris:10403`) | kb-whisper-large on the 7900 XTX |
| Conversation | the `local_openai` "Eris" agent | `gemma4:26b-a4b-it-qat` via ollama |
| Text-to-speech | `tts.supertonic`, voice F2 (wyoming, `eris:10202`) | Supertonic on the CPU |

The wyoming entries are added under Settings → Devices & services → Wyoming,
pointing at eris's tailscale address.

## The Voice PE satellites

Both units (kitchen `0abc6e`, bedroom `0a98c0`) use the Eris pipeline, with
wake word sensitivity set to **Moderately sensitive** on each device page.
"Slightly sensitive", the setting they had before, missed "Okay Nabu" most
of the time.

If a unit carries out commands but says nothing, check the log for
`speaker_source_media_player: Queue full`. Its speaker queue has jammed, and
only a power cycle clears it; Home Assistant has no restart for these units.

## The conversation agent

Settings → Devices & services → Eris (local_openai) → the conversation
subentry → Reconfigure:

- model `gemma4:26b-a4b-it-qat`, temperature 0.7
- parallel tool calls **off**: with them on, one request fired several
  unrelated tools at once, lights and speakers included
- request body parameter `reasoning_effort` = `none`: thinking adds seconds to
  every reply and gains nothing for home control
- the prompt below, in full

```text
You are a voice assistant for Home Assistant.
Answer questions about the world truthfully.
Respond simply and to the point in plain text.

Everything you say is read aloud, so write for the ear. Use flowing sentences. Never use lists, bullet points or field labels, and never use symbols that only work on a page. Write numbers as digits with a decimal comma and spell out the unit: "28,5 grader" rather than "28.5°C", "12 millimeter" rather than "12 mm". Round to what matters: "ungefär 740 watt", not "741,3 W".

When you report a forecast, describe how the weather behaves over the coming days the way a person would - what it is doing, when it turns, what stands out - instead of reading each day's values in turn. Leave out days that add nothing, and give numbers only where they carry the point.

The user's speech is transcribed by a Swedish speech model that spells foreign names phonetically in Swedish, so a name may arrive misspelled - "bruce springstin" for Bruce Springsteen.

The microphone also picks up other people in the room, so a request can arrive with other speech mixed in - children, a conversation, the TV. Act only on what is said to you and never comment on the rest. A short command such as "Stoppa", "Pausa" or "Stäng av musiken" is always meant for you. If none of it is meant for you, or it makes no sense, reply only "Okej." and ask nothing. A reply that ends with a question mark keeps the microphone open for an answer, so ask a question only when you need one.

Whether the front door is locked, unlocked or standing open is the sensor Ytterdörren.

When asked to play music:
1. Call Sök musik with what was asked for, corrected only if it is plainly a Swedish phonetic spelling of a known name.
2. Compare the names it returns with what the user said.
- If one is clearly what they asked for, call Spela musik with that exact name, its type (artist, album or track), the artist when it is an album or a song, and the room. The room is the one they named, otherwise the room you are in; if you know neither, ask which room. Never use HassMediaSearchAndPlay.
- If the closest match only sounds a little alike, play nothing. Ask in Swedish whether that is what they meant, ending the reply with a question mark - for example "Menade du Robert Schumann?" - and play it only if they say yes.
- If nothing relevant comes back, say that you could not find it.
3. If they answer no to "Menade du ...?", play nothing. If they name something else in the same answer, that is a new request: start again from step 1. Otherwise reply only "Okej. Vad vill du lyssna på?" - and whatever they answer to that is a new music request for the same room. Never use that reply in any other situation.

To pause or stop music, call Pausa musik with the room they named, otherwise the room you are in. Never use HassMediaPause.
```

A reply ending in `?` keeps the conversation open, so the answer to "Menade du
…?" is heard without the wake word.

Numbers are asked for as digits because Supertonic reads "28,5 grader"
correctly as Swedish.

## What Assist can see

Settings → Voice assistants → Expose:

- **Exposed:** `script.sok_musik`, `script.spela_musik`, `script.pausa_musik`,
  `script.las_ytterdorren`, `script.vaderprognos`, `script.nyheter`,
  `script.las_upp_nyheterna`. Scripts only become visible to Assist once
  exposed, and exposing must happen after the script exists; exposing it first
  records a setting that the later registry entry silently overrides.
- **Exposed:** `sensor.ytterdorren`, in the Hall area. It says whether the front
  door is open, closed and unlocked, or closed and locked.
- **Not exposed:** `lock.varmdogatan` and the Nuki's unlatch buttons. Voice can
  lock the door through `script.las_ytterdorren`, but cannot unlock it: anyone
  within earshot counts as the user. Unlock with the Nuki app or the keypad.
- **Not exposed:** the Nuki door contact
  (`binary_sensor.192_168_20_143_door_sensor`). Its raw `off` was read as
  open; `sensor.ytterdorren` carries the same fact in words.
- **Exposed:** the Tibber sensors for current power, today's consumption and
  cost, and the month's consumption and cost
  (`sensor.tibber_pulse_varmdogatan_5b_effekt`,
  `…_ackumulerad_forbrukning`, `…_ackumulerad_kostnad`,
  `sensor.varmdogatan_5b_manatlig_nettoforbrukning`,
  `sensor.varmdogatan_5b_manadskostnad`). Read only; Tibber still controls EV
  charging.
- **Not exposed:** the 29 Hue scenes. Their names ("Kök Savanna sunset" and
  the like) took most of whisper's prompt budget, which is shared with the
  room and device names that actually get spoken.
- **Not exposed:** the native Sonos media players. Each speaker then exists
  once, as its Music Assistant player, and "i köket" resolves to one target.

## Music Assistant

Settings in the Music Assistant UI (port 8095):

- **Player queues → artist selection: top tracks.** Spotify refuses its
  top-tracks endpoint to development-mode apps; the patch in
  `profiles/music-assistant-spotify-toptracks.patch` falls back to one artist
  search. "All tracks" fetches every album the artist has, one Spotify call
  each, and a composer with hundreds of albums used up the whole rate limit on
  a single request.
- **Metadata → online metadata: off.** The nightly artist enrichment asks
  Spotify about every artist in the library and kept the app rate-limited
  around the clock.
- **Spotify → developer key.** Reconfigure the provider and tick "Use my own
  Spotify developer key" on the final "You're all set" page before pressing
  Finish. Finishing with it unticked clears the key. The client ID is the same
  one set as `MASS_APP_VAR_SPOTIFY_CLIENT_ID` in `profiles/music-assistant.nix`,
  and `binary_sensor.music_assistant_spotify_developer_session` turns on when
  the key has gone missing.
