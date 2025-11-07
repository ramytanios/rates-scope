# set dotenv-required := true
# set dotenv-load := true

@default:
    just --list

fmt:
    just --fmt --unstable

[unix]
scalafmt:
    sbt scalafmtAll

[unix]
scalafix:
    sbt 'scalafixEnable; scalafixAll'

[unix]
fix:
    just scalafmt scalafix

[confirm]
[unix]
clean:
    git clean -Xdf

[unix]
deps:
    sbt dependencyUpdates
