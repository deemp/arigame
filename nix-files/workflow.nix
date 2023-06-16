{ workflows, system, name }:
let
  inherit (workflows.functions.${system}) writeWorkflow expr mkAccessors genAttrsId;
  inherit (workflows.attrsets.${system}) steps run os nixCI;
  inherit (workflows.functions.${system}) installNix cacheNixDirs;
  job1 = "_1_update_flake_locks";
  job2 = "_2_front";
  names = mkAccessors {
    secrets = genAttrsId [ "GITHUB_TOKEN" ];
  };
  workflow =
    nixCI // {
      jobs = {
        "${job1}" = {
          name = "Update flake locks";
          runs-on = os.ubuntu-20;
          steps =
            [
              steps.checkout
              (installNix { })
              steps.configGitAsGHActions
              steps.updateLocksAndCommit
            ];
        };
        "${job2}" =
          {
            name = "Publish static files";
            # needs = job1;
            permissions = {
              contents = "write";
            };
            runs-on = os.ubuntu-20;
            steps = [
              steps.checkout
              (installNix { })
              (cacheNixDirs { restoreOnly = false; })
              {
                name = "Clean npm cache";
                run = run.nixRun "npmCleanCache";
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
                run = run.nixRun "buildGHPages";
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
              steps.nixStoreCollectGarbage
            ];
          };
      };
    };
in
writeWorkflow name workflow
