{pkgs, ...}: {
  services.opentelemetry-collector = {
    enable = true;
    package = pkgs.opentelemetry-collector-contrib;
    settings = {
      receivers.journald = {
        directory = "/var/log/journal";
        priority = "info";
      };
      exporters.otlphttp.logs_endpoint = "https://victorialogs.9000.dev/insert/opentelemetry/v1/logs";
      processors = {
        "resourcedetection/system" = {
          detectors = ["system"];
          system.hostname_sources = ["os"];
        };
        "transform/journald" = {
          error_mode = "ignore";
          log_statements = [
            {
              context = "log";
              statements = [
                "set(severity_number, SEVERITY_NUMBER_DEBUG) where Int(body[\"PRIORITY\"]) == 7"
                "set(severity_number, SEVERITY_NUMBER_INFO) where Int(body[\"PRIORITY\"]) == 6"
                "set(severity_number, SEVERITY_NUMBER_INFO2) where Int(body[\"PRIORITY\"]) == 5"
                "set(severity_number, SEVERITY_NUMBER_WARN) where Int(body[\"PRIORITY\"]) == 4"
                "set(severity_number, SEVERITY_NUMBER_ERROR) where Int(body[\"PRIORITY\"]) == 3"
                "set(severity_number, SEVERITY_NUMBER_FATAL) where Int(body[\"PRIORITY\"]) <= 2"
                "set(attributes[\"priority\"], body[\"PRIORITY\"])"
                "set(attributes[\"process.comm\"], body[\"_COMM\"])"
                "set(attributes[\"process.exec\"], body[\"_EXE\"])"
                "set(attributes[\"process.uid\"], body[\"_UID\"])"
                "set(attributes[\"process.gid\"], body[\"_GID\"])"
                "set(attributes[\"owner_uid\"], body[\"_SYSTEMD_OWNER_UID\"])"
                "set(attributes[\"unit\"], body[\"_SYSTEMD_UNIT\"])"
                "set(attributes[\"syslog_identifier\"], body[\"SYSLOG_IDENTIFIER\"])"
                "set(attributes[\"syslog_identifier_prefix\"], ConvertCase(body[\"SYSLOG_IDENTIFIER\"], \"lower\")) where body[\"SYSLOG_IDENTIFIER\"] != nil"
                "replace_pattern(attributes[\"syslog_identifier_prefix\"], \"^[^a-zA-Z]*([a-zA-Z]{3,25}).*\", \"$$1\") where body[\"SYSLOG_IDENTIFIER\"] != nil"
                "set(attributes[\"unit_prefix\"], ConvertCase(body[\"_SYSTEMD_UNIT\"], \"lower\")) where body[\"_SYSTEMD_UNIT\"] != nil"
                "replace_pattern(attributes[\"unit_prefix\"], \"^[^a-zA-Z]*([a-zA-Z]{3,25}).*\", \"$$1\") where body[\"_SYSTEMD_UNIT\"] != nil"
                "set(attributes[\"job\"], attributes[\"syslog_identifier_prefix\"])"
                "set(attributes[\"job\"], attributes[\"unit_prefix\"]) where attributes[\"job\"] == nil and attributes[\"unit_prefix\"] != nil"
                "set(body, body[\"MESSAGE\"])"
              ];
            }
          ];
        };
        groupbyattrs = {
          keys = [
            "service.name"
            "host.name"
            "receiver"
            "job"
          ];
        };
      };
      service.pipelines.logs = {
        receivers = ["journald"];
        processors = ["transform/journald" "batch" "resourcedetection/system" "groupbyattrs"];
        exporters = ["otlphttp"];
      };
    };
  };
}
