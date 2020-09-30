{ name, rev, ... }:

{
  resources.pipelineRuns.${name}.spec = {
    pipelineRef.name = name;
    serviceAccountName = "robot";
    params = {
      giturl.value = "git@github.com:johnae/world";
      gitrev.value = rev;
    };
  };

}
