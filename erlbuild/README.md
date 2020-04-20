'erlbuild' is a low-level build system for Erlang.

It is meant to be used by higher-level build systems (e.g. rebar3, make), rather
being called directly.

For an example on how to use 'erlbuild' from make, see './erlbuild.mk'.

'erlbuild' has the following properties:

- it builds several .erl, .xrl, .yrl files in one Erlang src directory
- it does this correctly, incrementally, and in parallel (if requested)
- it supports the same command-line options as 'erlc'
- the logic and the implementation are very simple and robust. The core logic
  is implemented in only 7 lines of Makefile code
- all build phases and individual compilation steps are explicit, observable,
  and can be reproduced manually from the commmand-line

Looking at 'erlbuild' from interface standpoint, is a drop-in replacement
for 'erlc' compiling multiple .erl files.

For instance, calling

    erlbuild <flags> *.erl

Is equivalent to

    erlc <flags> *.erl

The difference are:

- 'erlbuild' automatically determines the correct order in which source files
  should be compiled. Specifically, behaviors and parse transforms should be
  compiled before they are used.

- 'erlbuild' compiles incrementally by building and maintaining an exhaustive
  graph of compile-time dependencies.

- 'erlbuild' compiles in parallel (controlled by -j option)

- 'erlbuild' compiles .xrl and .yrl files to .beam (if .xrl/yrl files are given as input)


Details of how it works:

1.  Under the hood, 'erlbuild' generates a self-contained makefile named
    'erlbuild.mk' and calls 'make -f erlbuild.mk'. GNU make then drives the
    phases as described below.

    The simplicify and robustness of 'erlbuild' comes from the fact its core
    logic is implemented in 6 lines of 'make'.


2.  The build process is divided into 3 non-overlapping phases:

    - "generate"     -- generation of .erl files from .xrl and .yrl (if .xrl/yrl files are given as input)
    - "scan"         -- scanning of .erl files to build or update the dependency graph, and extract metadata
    - "compile"      -- compiling or recompiling .erl files into .beam files

    "scan" and "compile" are the main build phases.

    "generate" was introduced mainly to simplify integration of 'erlbuild'
    with the existing build systems (i.e. rebar3 and app.mk).

    Importantly, each phase is incremental and fully parallelizable.


3.  "scan" builds or updates an exhaustive graph of compile-time dependencies

    Compiled .beam files depend on source .erl files, .hrl files, and also
    behavior and parse transform .beams.

    The dependency graph is then used for three things:

    - determining the order in which .erl files should be compiled
    - triggering rebuild of .beam files if their dependencies change
    - triggering incremental rebuild of the dependency graph itself on change of
      its dependencies

    The dependency graph is cached in the filesystem as *.d files, one per
    source .erl file. This allows building/updating the dependency graph in
    parallel.

    Note that the dependency graph (i.e. *.d) files depend on source .erl files,
    and .hrl files, but *not* on behavior and parse transform .beams.

    Dependency scan of individual .erl files is performed by calling
    'erlbuild-erlc --build-phase scan ...'. See "erlbuild dependency scanner"
    section below for details.

    These are the exact 'make' directives implementing this logic. See
    './erlbuild.template.mk' for details.

    ```
        DEPFILES := $(ERLS:%.erl=$(BUILD_DIR)/%.d)

        $(BUILD_DIR)/%.d: %.erl
                erlbuild-erlc --build-phase scan -M -MP -MF $@ $(ERLC_FLAGS) $<

        include $(wildcard $(DEPFILES))
    ```


4.  "compile" phase compiles .erl files into .beam files

    Compilation is performed by calling 'erlc' for individual .erl files.

    Compilation of an .erl file is triggered if target .beam file is missing, or
    if some of its dependencies have been updated.

    These are the exact 'make' directives implementing this logic. See
    './erlbuild.template.mk' for details.

    ```
        $(EBIN)/%.beam: %.erl
                erlbuild-erlc --build-phase compile $(ERLC_FLAGS) $<

        include $(DEPFILES)
    ```


5.  "generate" phase generates of .erl files from .xrl and .yrl

    Generation is performed by calling 'erlc' for individual .xrl and .yrl files.

    Normally, code generation step should be run as a separate build module. In
    the 'erlbuild' world view, it would correspond to a separate
    'erlbuild generate' step.

    But because of a very simple model for building .yrl and .xrl files (each
    corresponding to a single .erl and target .beam) it is safe to include them.

    These are the exact 'make' directives implementing this logic. See
    './erlbuild.template.mk' for details.

    ```
        %.erl: %.xrl
                erlc $<

        %.erl: %.yrl
                erlc $<
    ```


6.  The generated 'erlbuild.mk' files, intermediate compilation state, and
    the dependency graph are kept in a separate directory. See 'erlbuild-erlc
    --build-dir' option for details.


7.  'erlbuild' passes most of command-line options unchanged to 'erlc'. There
    are several options specific to 'erlbuild' -- see 'erlbuild -h' for a
    complete list.


## erlbuild dependency scanner

Overall, it follows Erlang standard dependency scanner ('erlc -M') logic and
interface. There are several critical extensions:

- in addition to included files treated as dependencies by 'erlc -M',
  'erlbuild-erlc -M2' generates a list of .beam dependencies from behaviors
  and parse transforms

- unlike 'erlc -M', it does not tolerate epp errors. For example, the original
  depscanner silently ignores includes that are not found at the dependency scan
  stage. This could lead to all sorts of problems under a stricter compilation
  model, including inconsistent builds or builds failing for obcsure reasons if
  the dependency graph ends up being inconsistent.

- when '-MF' flag is set, it generates dependencies for the dependency graph
  itself (which is includes, parse transforms, but not behaviors). This, in
  turn, allows to maintain and consistently update the dependency graph during
  incremental builds.

- by default, 'erlbuild-erlc -M2' does not run parse transforms.

  This behavior can be overridden by `erlbuild-erlc -M2 +makedep2_run_parse_transforms`
  option.

  Not executing parse transforms during dependency scan allows us to build all
  .erl files in a src directory using one 'erlbuild' run with 0 configuration.
  Otherwise, we would have to use 2 runs, and listing parse transform sources
  explcitly.

  Although running parse transforms during dependency scan is the safest thing
  to do, it is not very useful in practice, as it is highly unlikely for
  transformations to introduce new compile-time dependenices (i.e.  other
  include files and behaviors).

  This assumption definitely holds true for `wa_erl_pp.erl`, which is the only
  parse transform we use in server/erl codebase.

  Note that rebar3 and mix do not run parse transformations during their
  dependencies scan either. Although, likely unknowingly.


## Known Limitations

1. This approach doesn't allow building Erlang source files from subdirectories
   of a given src directory. This is because under the hood we use make and
   automatic make rules like `%.beam: %.erl`. While it is possible support
   this, it would introduce some additional complexity.

2. This approach depends on GNU make, which limits portability.


## Future Steps

1. Currently, erlbuild is slower and less efficient than it could be, because
   each individual compile and scan commands have a ~100ms overhead for
   starting Erlang VM. This inefficiency can be addressed by implementing a
   build server as a part of erlbuild, and implementing scan and compile
   steps as requests to the build server.

2. Implementing a build server would open up more optimization opportunities.
   For example, caching AST obtained during the scan phas, and avoiding parsing
   it the second time during compile.

3. Another potential optimization is not rebuilding .beam when a source file
   change doesn't change the AST.

4. Later, we may add support for using Ninja in addition to GNU make. Compared
   to make, Ninja is faster and provides more consistency guarantees. See
   details at https://ninja-build.org
