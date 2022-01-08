# remote-io

[![Test Workflow](https://github.com/LolHens/remote-io/workflows/test/badge.svg)](https://github.com/LolHens/remote-io/actions?query=workflow%3Atest)
[![Release Notes](https://img.shields.io/github/release/LolHens/remote-io.svg?maxAge=3600)](https://github.com/LolHens/remote-io/releases/latest)
[![Maven Central](https://img.shields.io/maven-central/v/de.lolhens/remote-io_2.13)](https://search.maven.org/artifact/de.lolhens/remote-io_2.13)
[![Apache License 2.0](https://img.shields.io/github/license/LolHens/remote-io.svg?maxAge=3600)](https://www.apache.org/licenses/LICENSE-2.0)

Scala RPCs based on effect types.

### build.sbt
```sbt
// use this snippet for the JVM
libraryDependencies += "de.lolhens" %% "remote-io-core" % "0.0.1"
libraryDependencies += "de.lolhens" %% "remote-io-http4s" % "0.0.1"

// use this snippet for JS, or cross-building
libraryDependencies += "de.lolhens" %%% "remote-io-core" % "0.0.1"
libraryDependencies += "de.lolhens" %%% "remote-io-http4s" % "0.0.1"
```

## License
This project uses the Apache 2.0 License. See the file called LICENSE.
