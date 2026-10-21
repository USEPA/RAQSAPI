# RAQSAPI devcontainer - A basic devcontainer for development of RAQSAPI.
![OSS Lifecycle](https://img.shields.io/osslifecycle?file_url=https%3A%2F%2Fgithub.com%2FUSEPA%2FRAQSAPI%2Fblob%2Fmain%2F.devcontainer%2FOSSMETADATA&style=plastic)

## Instructions
Reopen the project in the devcontainer. RStudio Server is forwarded on port
8787 and opens in a browser. Sign in with username `RAQSAPI` and password
`RAQSAPI`[^1].

[^1]: The fixed password is intended only for a trusted, local development
environment. Do not expose this server to an untrusted network.

The devcontainer image is based upon the rocker project [verse R image](https://rocker-project.org/images/).
Additional development tools are installed using the `r-packages` feature,
which is configured with the r-universe repositories for rOpenSci and its
review tools. When the devcontainer is created, the `r-dependent-packages`
feature uses `pak::local_install_deps()` to install the dependencies listed by
the mounted project, with those same repositories configured for dependency
resolution.

See the [contributing.md](https://github.com/USEPA/RAQSAPI/blob/main/contributing.md) document for those that want to contribute.