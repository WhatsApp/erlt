
This directory contains:
- rebar3: use this rebar for the project, consider adding it to your path.
    - implementation detail: this is a shell script that downloads the right version of rebar if you don't have it already and then forwards arguments to it.

- (for developers of erlt itself): sbtn. This is used from our Makefile. It downloads sbtn if you don't have it already and forwards arguments to it.
