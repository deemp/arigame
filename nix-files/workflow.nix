{ workflows, system, name, scripts }:
let
  inherit (workflows.lib.${system})
    writeWorkflow expr mkAccessors genAttrsId
    steps run os nixCI cacheNix names;
  workflow =
    nixCI {
      jobArgs = {
        strategy = { };
        runsOn = os.ubuntu-22;
        cacheNixArgs = {
          linuxGCEnabled = true;
          linuxMaxStoreSize = 5000000000;
        };
        doSaveFlakes = false;
        steps = _: [
          {
            name = "Clean npm cache";
            run = run.nixScript { name = scripts.npmCleanCache.pname; };
          }
          {
            name = "Cache app dependencies";
            uses = "actions/cache@v3";
            "with" = {
              key = "${expr names.runner.os}-node-${expr "hashFiles('**/package-lock.json', '**/*.dhall')" }";
              path = ''
                ~/.npm
                .spago
                output
              '';
              restore-keys = ''
                "${expr names.runner.os }-node-
              '';
            };
          }
          {
            name = "Build";
            run = run.nixScript { name = scripts.buildGHPages.pname; };
          }
          {
            name = "GitHub Pages action";
            uses = "peaceiris/actions-gh-pages@v3.9.3";
            "with" = {
              github_token = expr names.secrets.GITHUB_TOKEN;
              publish_dir = "./docs";
              force_orphan = true;
            };
          }
        ];
      };
    };
in
writeWorkflow name workflow
