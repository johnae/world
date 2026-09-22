## Home Assistant speaks wyoming; llama.cpp speaks OpenAI chat-completions with
## an `input_audio` part. Nothing bridges the two, hence this.
##
## The reason to bother: Qwen3-ASR takes its biasing vocabulary as an ordinary
## system turn, with no token budget. Whisper's `initial_prompt` is capped at
## 223 tokens and Home Assistant's entity names already fill most of it, which
## leaves no room for the names that actually get mistranscribed - foreign
## artist names inside a Swedish sentence.
{
  lib,
  python3Packages,
  ...
}:
python3Packages.buildPythonApplication {
  pname = "wyoming-qwen-asr";
  version = "0.1.0";
  format = "other";

  src = ./.;

  propagatedBuildInputs = with python3Packages; [
    aiohttp
    wyoming
  ];

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -Dm755 wyoming_qwen_asr.py $out/bin/wyoming-qwen-asr
    runHook postInstall
  '';

  meta = {
    description = "Wyoming speech-to-text bridge to a llama.cpp server running Qwen3-ASR";
    mainProgram = "wyoming-qwen-asr";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
