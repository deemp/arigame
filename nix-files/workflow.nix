{ workflows, system, name }:
let
  inherit (workflows.functions.${system}) writeWorkflow expr mkAccessors genAttrsId;
  inherit (workflows.configs.${system}) steps os nixCI;
  inherit (workflows.functions.${system}) installNix;
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
            runs-on = os.ubuntu-20;
            steps = [
              steps.checkout
              (installNix { })
              {
                name = "Build";
                run = ''nix run .#buildGHPages'';
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
