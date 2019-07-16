workflow "main" {
  on = "push"
  resolves = ["test"]
}

action "test" {
  uses = "docker://Bogdanp/racket:7.3"
  runs = "/github/workspace/ci/test.sh"
}
