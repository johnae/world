#!/usr/bin/env python3
"""Wyoming text-to-speech server backed by Supertonic 3.

Supertonic ships a Python SDK and four ONNX graphs, not a network service, so
this is a thin wyoming front for it rather than a proxy. The model is loaded
once at startup: it is only a few hundred megabytes, and reloading per request
would dominate the synthesis time.
"""

from __future__ import annotations

import argparse
import asyncio
import logging

import numpy as np
from supertonic import TTS
from wyoming.audio import AudioChunk, AudioStart, AudioStop
from wyoming.event import Event
from wyoming.info import Attribution, Describe, Info, TtsProgram, TtsVoice
from wyoming.server import AsyncEventHandler, AsyncServer
from wyoming.tts import Synthesize

_LOGGER = logging.getLogger(__name__)

## Five female and five male preset styles ship with the open weights.
VOICES = ["F1", "F2", "F3", "F4", "F5", "M1", "M2", "M3", "M4", "M5"]

## Chunked so Home Assistant can start playing before synthesis of a long
## sentence has finished streaming out.
CHUNK_SAMPLES = 2048


class SupertonicHandler(AsyncEventHandler):
    def __init__(self, *args, cli_args: argparse.Namespace, tts: TTS, **kwargs) -> None:
        super().__init__(*args, **kwargs)
        self.cli_args = cli_args
        self.tts = tts

    async def handle_event(self, event: Event) -> bool:
        if Describe.is_type(event.type):
            await self.write_event(_info(self.cli_args, self.tts).event())
            return True

        if not Synthesize.is_type(event.type):
            return True

        synthesize = Synthesize.from_event(event)
        voice = self.cli_args.voice
        if synthesize.voice is not None and synthesize.voice.name in VOICES:
            voice = synthesize.voice.name

        pcm = await asyncio.get_running_loop().run_in_executor(
            None, self._synthesize, synthesize.text, voice
        )

        rate = self.tts.sample_rate
        await self.write_event(AudioStart(rate=rate, width=2, channels=1).event())
        for start in range(0, len(pcm), CHUNK_SAMPLES * 2):
            await self.write_event(
                AudioChunk(
                    rate=rate,
                    width=2,
                    channels=1,
                    audio=pcm[start : start + CHUNK_SAMPLES * 2],
                ).event()
            )
        await self.write_event(AudioStop().event())
        return False

    def _synthesize(self, text: str, voice: str) -> bytes:
        style = self.tts.get_voice_style(voice_name=voice)
        wav, _ = self.tts.synthesize(
            text,
            voice_style=style,
            lang=self.cli_args.language,
            speed=self.cli_args.speed,
        )
        ## The SDK hands back float32 in [-1, 1] with a leading batch axis;
        ## wyoming wants interleaved little-endian 16-bit PCM.
        samples = np.asarray(wav, dtype=np.float32).reshape(-1)
        samples = np.clip(samples, -1.0, 1.0)
        return (samples * 32767.0).astype("<i2").tobytes()


def _info(cli_args: argparse.Namespace, tts: TTS) -> Info:
    attribution = Attribution(
        name="Supertone", url="https://huggingface.co/Supertone/supertonic-3"
    )
    return Info(
        tts=[
            TtsProgram(
                name="supertonic",
                description="Supertonic 3 on-device text-to-speech",
                attribution=attribution,
                installed=True,
                version="3",
                voices=[
                    TtsVoice(
                        name=name,
                        description=f"Supertonic 3 {name}",
                        attribution=attribution,
                        installed=True,
                        languages=[cli_args.language],
                        version="3",
                    )
                    for name in VOICES
                ],
            )
        ]
    )


async def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--uri", required=True, help="unix:// or tcp://")
    parser.add_argument("--voice", default="F5", help="Preset style used when the request names none")
    parser.add_argument("--language", default="sv")
    parser.add_argument("--speed", type=float, default=1.05)
    parser.add_argument("--debug", action="store_true")
    cli_args = parser.parse_args()

    logging.basicConfig(level=logging.DEBUG if cli_args.debug else logging.INFO)

    ## Loading here rather than per connection: it takes about a second, and
    ## the first spoken reply should not pay for it.
    tts = TTS(auto_download=True)
    _LOGGER.info("ready, voice=%s language=%s rate=%d", cli_args.voice, cli_args.language, tts.sample_rate)

    server = AsyncServer.from_uri(cli_args.uri)
    await server.run(lambda *a, **kw: SupertonicHandler(*a, cli_args=cli_args, tts=tts, **kw))


if __name__ == "__main__":
    asyncio.run(main())
