@default:
    just --list

fmt:
    just --fmt --unstable

ws:
  websocat ws://localhost:8090/api/ws

scalafmt:
    sbt scalafmtAll

scalafix:
    sbt 'scalafixEnable; scalafixAll'

fix:
    just scalafmt scalafix

[confirm]
clean:
    git clean -Xdf

deps:
    sbt dependencyUpdates

