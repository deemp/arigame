{ workflows, system, name, scripts }:
let
  inherit (workflows.lib.${system})
    writeWorkflow expr mkAccessors genAttrsId
    steps run os nixCI cacheNix;
  job1 = "_1_update_flake_locks";
  job2 = "_2_front";
  names = mkAccessors {
    secrets = genAttrsId [ "GITHUB_TOKEN" ];
  };
  workflow =
    (nixCI { }) // {
      jobs = {
        "${job1}" = (nixCI { }).jobs.nixCI // { strategy.matrix.os = [ os.ubuntu-22 ]; };
        "${job2}" =
          {
            name = "Publish static files";
            permissions = {
              contents = "write";
            };
            runs-on = os.ubuntu-20;
            steps = [
              steps.checkout
              (steps.installNix { })
              (steps.cacheNix { })
              {
                name = "Clean npm cache";
                run = run.nixScript { name = scripts.npmCleanCache.pname; };
              }
              {
                name = "Cache dependencies";
                uses = "actions/cache@v3";
                env = {
                  cache-name = "cache-deps";
                };
                "with" = {
                  key = "\${{ runner.os }}-build-\${{ env.cache-name }}-\${{ hashFiles('**/package-lock.json') }}-\${{ hashFiles('**/*.dhall') }}";
                  path = ''
                    ~/.npm
                    .spago
                    output
                  '';
                  restore-keys = ''
                    ''${{ runner.os }}-build-''${{ env.cache-name }}-
                    ''${{ runner.os }}-build-
                    ''${{ runner.os }}-
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
    };
in
writeWorkflow name workflow
