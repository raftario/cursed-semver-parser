# cursed-semver-parser

Cursed alternative parser for the semver crate that works like legacy QPM

## Usage

```toml
cursed-semver-parser = { git = "https://github.com/raftario/cursed-semver-parser" }
```

```rs
use semver::VersionReq;

let version_req: VersionReq = cursed_semver_parser::parse(">=0.6.5<0.8").unwrap();
```

## Usage with serde

```toml
cursed-semver-parser = { git = "https://github.com/raftario/cursed-semver-parser", features = ["serde"] }
```

```rs
use semver::VersionReq;
use serde::Deserialize;

#[derive(Deserialize)]
struct Dependency {
    name: String,
    #[serde(deserialize_with = "cursed_semver_parser::deserialize")]
    version: VersionReq,
}
```
