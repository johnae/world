#!/usr/bin/env python3
"""Wyoming speech-to-text server backed by whisper.cpp's whisper-server.

whisper.cpp runs whisper on the GPU through Vulkan, which faster-whisper cannot
do on AMD hardware. What it lacks is Home Assistant awareness, so the biasing
prompt is built here with wyoming-faster-whisper's own name cache: the same
tiers, budget and refresh behaviour the CPU server uses, counted with the
model's real tokenizer.
"""

from __future__ import annotations

import argparse
import asyncio
import io
import logging
import os
import wave

import aiohttp
from huggingface_hub import hf_hub_download
from tokenizers import Tokenizer
from wyoming.asr import Transcribe, Transcript
from wyoming.audio import AudioChunk, AudioStart, AudioStop
from wyoming.event import Event
from wyoming.info import AsrModel, AsrProgram, Attribution, Describe, Info
from wyoming.server import AsyncEventHandler, AsyncServer
from wyoming_faster_whisper.hass_api import HomeAssistant
from wyoming_faster_whisper.name_cache import HassNameCache

_LOGGER = logging.getLogger(__name__)


class TokenCounter:
    """Stands in for a transcriber so the name cache can budget exactly."""

    def __init__(self, tokenizer: Tokenizer) -> None:
        self._tokenizer = tokenizer

    def count_prompt_tokens(self, text: str) -> int:
        return len(self._tokenizer.encode(text, add_special_tokens=False).ids)


class WhisperCppHandler(AsyncEventHandler):
    def __init__(
        self,
        *args,
        cli_args: argparse.Namespace,
        names: HassNameCache | None,
        counter: TokenCounter,
        **kwargs,
    ) -> None:
        super().__init__(*args, **kwargs)
        self.cli_args = cli_args
        self.names = names
        self.counter = counter
        self._audio = bytearray()
        self._rate, self._width, self._channels = 16000, 2, 1

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
            # Start fetching names while the user is still speaking, so the
            # prompt is ready by the time the audio ends.
            if self.names is not None:
                self.names.schedule_refresh()
            return True

        if AudioChunk.is_type(event.type):
            self._audio.extend(AudioChunk.from_event(event).audio)
            return True

        if AudioStop.is_type(event.type):
            text = await self._transcribe(bytes(self._audio))
            await self.write_event(Transcript(text=text).event())
            return False

        return True

    async def _prompt(self) -> str | None:
        if self.names is None:
            return self.cli_args.initial_prompt
        return await self.names.initial_prompt(self.counter)

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

        form = aiohttp.FormData()
        form.add_field("file", self._to_wav(pcm), filename="audio.wav", content_type="audio/wav")
        form.add_field("language", self.cli_args.language)
        form.add_field("response_format", "json")
        form.add_field("temperature", "0")
        prompt = await self._prompt()
        if prompt:
            form.add_field("prompt", prompt)
            _LOGGER.debug("prompt: %s", prompt)

        timeout = aiohttp.ClientTimeout(total=self.cli_args.timeout)
        try:
            async with aiohttp.ClientSession(timeout=timeout) as session:
                async with session.post(
                    f"{self.cli_args.api.rstrip('/')}/inference", data=form
                ) as resp:
                    resp.raise_for_status()
                    body = await resp.json()
        except Exception:
            # A failed transcription must not wedge the pipeline; an empty
            # transcript lets Home Assistant say it did not catch that.
            _LOGGER.exception("transcription failed")
            return ""

        text = body.get("text", "").strip()
        _LOGGER.debug("transcript: %s", text)
        return text


def _info(cli_args: argparse.Namespace) -> Info:
    return Info(
        asr=[
            AsrProgram(
                name="whisper-cpp",
                description="whisper.cpp on the GPU",
                attribution=Attribution(
                    name="ggml-org", url="https://github.com/ggml-org/whisper.cpp"
                ),
                installed=True,
                version="1",
                models=[
                    AsrModel(
                        name=cli_args.tokenizer,
                        description=cli_args.tokenizer,
                        attribution=Attribution(name="KBLab", url="https://huggingface.co/KBLab"),
                        installed=True,
                        languages=[cli_args.language],
                        version="1",
                    )
                ],
            )
        ]
    )


def _read_token(cli_args: argparse.Namespace) -> str | None:
    path = cli_args.hass_token_file or os.environ.get("WYO_WHISPER_HASS_TOKEN_FILE")
    if not path:
        return None
    with open(path, encoding="utf-8") as handle:
        return handle.read().strip() or None


async def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--uri", required=True, help="unix:// or tcp://")
    parser.add_argument("--api", default="http://127.0.0.1:8910")
    parser.add_argument("--language", default="sv")
    parser.add_argument(
        "--tokenizer",
        default="KBLab/kb-whisper-large",
        help="Hugging Face repo whose tokenizer.json matches the served model",
    )
    parser.add_argument("--initial-prompt", default=None, help="Kept ahead of any names")
    parser.add_argument("--initial-prompt-file", default=None, help="Read the prompt from here")
    parser.add_argument("--hass-api", default=None, help="e.g. http://icarus:8123/api")
    parser.add_argument("--hass-token-file", default=None)
    parser.add_argument("--hass-refresh-seconds", type=float, default=300.0)
    parser.add_argument("--prompt-max-tokens", type=int, default=200)
    parser.add_argument("--timeout", type=float, default=30.0)
    parser.add_argument("--debug", action="store_true")
    cli_args = parser.parse_args()

    logging.basicConfig(level=logging.DEBUG if cli_args.debug else logging.INFO)

    if cli_args.initial_prompt_file:
        with open(cli_args.initial_prompt_file, encoding="utf-8") as handle:
            cli_args.initial_prompt = handle.read().strip() or None

    counter = TokenCounter(
        Tokenizer.from_file(hf_hub_download(cli_args.tokenizer, "tokenizer.json"))
    )

    names: HassNameCache | None = None
    token = _read_token(cli_args)
    if cli_args.hass_api and token:
        names = HassNameCache(
            HomeAssistant(token, cli_args.hass_api),
            prefix=cli_args.initial_prompt,
            max_tokens=cli_args.prompt_max_tokens,
            refresh_seconds=cli_args.hass_refresh_seconds,
        )
        names.schedule_refresh()
        _LOGGER.info("biasing toward names from %s", cli_args.hass_api)

    _LOGGER.info("ready, proxying to %s", cli_args.api)
    server = AsyncServer.from_uri(cli_args.uri)
    await server.run(
        lambda *a, **kw: WhisperCppHandler(
            *a, cli_args=cli_args, names=names, counter=counter, **kw
        )
    )


if __name__ == "__main__":
    asyncio.run(main())
