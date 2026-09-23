#!/usr/bin/env python3
"""Wyoming speech-to-text server backed by a llama.cpp server running Qwen3-ASR.

Home Assistant speaks wyoming; llama-server speaks OpenAI chat-completions with
an ``input_audio`` part. This bridges the two, and adds the thing that made
Qwen3-ASR worth the trouble: a vocabulary prompt in the system turn, which has
no token budget, unlike whisper's 223.
"""

from __future__ import annotations

import argparse
import asyncio
import base64
import io
import logging
import re
import wave

import aiohttp
from wyoming.asr import Transcribe, Transcript
from wyoming.audio import AudioChunk, AudioStart, AudioStop
from wyoming.event import Event
from wyoming.info import AsrModel, AsrProgram, Attribution, Describe, Info
from wyoming.server import AsyncEventHandler, AsyncServer

_LOGGER = logging.getLogger(__name__)

# Qwen3-ASR prefixes its output with a language preamble, e.g.
# "language Swedish<asr_text>Tänd lampan". Only the text after the marker is
# the transcript.
_ASR_TEXT = re.compile(r".*?<asr_text>", re.DOTALL)


def _strip_preamble(text: str) -> str:
    return _ASR_TEXT.sub("", text, count=1).strip()


class QwenAsrHandler(AsyncEventHandler):
    def __init__(self, *args, cli_args: argparse.Namespace, **kwargs) -> None:
        super().__init__(*args, **kwargs)
        self.cli_args = cli_args
        self._audio = bytearray()
        self._rate = 16000
        self._width = 2
        self._channels = 1

    async def handle_event(self, event: Event) -> bool:
        if Describe.is_type(event.type):
            await self.write_event(_info(self.cli_args).event())
            return True

        if Transcribe.is_type(event.type):
            return True

        if AudioStart.is_type(event.type):
            start = AudioStart.from_event(event)
            self._rate, self._width, self._channels = start.rate, start.width, start.channels
            self._audio = bytearray()
            return True

        if AudioChunk.is_type(event.type):
            self._audio.extend(AudioChunk.from_event(event).audio)
            return True

        if AudioStop.is_type(event.type):
            text = await self._transcribe(bytes(self._audio))
            await self.write_event(Transcript(text=text).event())
            return False

        return True

    def _to_wav(self, pcm: bytes) -> bytes:
        buf = io.BytesIO()
        with wave.open(buf, "wb") as wav:
            wav.setnchannels(self._channels)
            wav.setsampwidth(self._width)
            wav.setframerate(self._rate)
            wav.writeframes(pcm)
        return buf.getvalue()

    async def _transcribe(self, pcm: bytes) -> str:
        if not pcm:
            return ""

        audio_b64 = base64.b64encode(self._to_wav(pcm)).decode("ascii")
        messages = []
        vocabulary = _read_vocabulary(self.cli_args)
        if vocabulary:
            messages.append({"role": "system", "content": vocabulary})
        messages.append(
            {
                "role": "user",
                "content": [
                    {
                        "type": "input_audio",
                        "input_audio": {"data": audio_b64, "format": "wav"},
                    }
                ],
            }
        )

        payload = {"messages": messages, "temperature": 0}
        timeout = aiohttp.ClientTimeout(total=self.cli_args.timeout)
        try:
            async with aiohttp.ClientSession(timeout=timeout) as session:
                async with session.post(
                    f"{self.cli_args.api.rstrip('/')}/chat/completions", json=payload
                ) as resp:
                    resp.raise_for_status()
                    body = await resp.json()
        except Exception:
            # A failed transcription must not wedge the pipeline; an empty
            # transcript lets Home Assistant say it did not catch that.
            _LOGGER.exception("transcription failed")
            return ""

        raw = body["choices"][0]["message"]["content"]
        text = _strip_preamble(raw)
        _LOGGER.debug("transcript: %s", text)
        return text


def _read_vocabulary(cli_args: argparse.Namespace) -> str | None:
    """Vocabulary comes from a file so it can be regenerated without a restart."""
    if not cli_args.vocabulary_file:
        return cli_args.vocabulary
    try:
        with open(cli_args.vocabulary_file, encoding="utf-8") as handle:
            content = handle.read().strip()
    except OSError:
        _LOGGER.warning("cannot read %s", cli_args.vocabulary_file)
        return cli_args.vocabulary
    return content or cli_args.vocabulary


def _info(cli_args: argparse.Namespace) -> Info:
    return Info(
        asr=[
            AsrProgram(
                name="qwen3-asr",
                description="Qwen3-ASR via llama.cpp",
                attribution=Attribution(name="Qwen", url="https://github.com/QwenLM/Qwen3-ASR"),
                installed=True,
                version="1",
                models=[
                    AsrModel(
                        name=cli_args.model_name,
                        description=cli_args.model_name,
                        attribution=Attribution(
                            name="Qwen", url="https://github.com/QwenLM/Qwen3-ASR"
                        ),
                        installed=True,
                        languages=cli_args.languages,
                        version="1",
                    )
                ],
            )
        ]
    )


async def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--uri", required=True, help="unix:// or tcp://")
    parser.add_argument("--api", default="http://127.0.0.1:8081/v1")
    parser.add_argument("--model-name", default="Qwen3-ASR-1.7B")
    parser.add_argument("--languages", nargs="*", default=["sv"])
    parser.add_argument("--vocabulary", default=None, help="Biasing text for the system turn")
    parser.add_argument("--vocabulary-file", default=None, help="Read biasing text from here")
    parser.add_argument("--timeout", type=float, default=30.0)
    parser.add_argument("--debug", action="store_true")
    cli_args = parser.parse_args()

    logging.basicConfig(level=logging.DEBUG if cli_args.debug else logging.INFO)
    _LOGGER.info("ready, proxying to %s", cli_args.api)

    server = AsyncServer.from_uri(cli_args.uri)
    await server.run(lambda *a, **kw: QwenAsrHandler(*a, cli_args=cli_args, **kw))


if __name__ == "__main__":
    asyncio.run(main())
