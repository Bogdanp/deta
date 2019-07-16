workflow "main" {
  on = "push"
  resolves = ["docs", "test"]
}

action "build" {
  uses = "docker://bogdanp/racket:7.3"
  runs = "/github/workspace/ci/build.sh"
}

action "test" {
  needs = ["build"]
  uses = "docker://bogdanp/racket:7.3"
  runs = "/github/workspace/ci/test.sh"
}

action "docs" {
  needs = ["build"]
  uses = "docker://bogdanp/racket:7.3"
  runs = "/github/workspace/ci/docs.sh"
}
