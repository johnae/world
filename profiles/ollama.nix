{...}: {
  services.ollama.enable = true;
  services.open-webui.enable = true;
  services.open-webui.environment = {
    PYDANTIC_SKIP_VALIDATING_CORE_SCHEMAS = "True";
    WEBUI_AUTH = "False";
  };
}
