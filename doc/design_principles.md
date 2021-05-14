# ErlT design principles

This document serves to outline the general goals and principles for ErlT. While it is by no means exhaustive, it aims to summarize the rules for making the design decisions which shape the language. Some of these rules are subjective, and at times are at odds with each other; reaching the right balance and making the right exceptions is the essence of how successful programming languages are designed.

## ErlT design principles

Same as for Erlang, core value of ErlT is high productivity of software engineering when developing complex, highly concurrent, highly reliable systems.

In ErlT design, we follow the original core principles of Erlang design:

**high-level declarative language**

High-level language allows to closely model the application domain, while avoiding added complexity, and eliminating major classes of errors.

**simple and conservative**

“Less is more”. Superficial ergonomic and aesthetic wins are not good excuses for adding more language features and making it more complex.

**explicit**

Following the logic of a program should be straightforward. There is little to no indirection or hidden abstractions.


On top of it, ErlT adds two more principles:

**productivity at any scale**

The language must scale for projects and teams of any sizes. From small prototypes to large codebases, large teams, high velocity of changes.

**delightful user experience**

We approach designing ErlT as a polished and coherent consumer product meant to create a delightful experience for its users.

Overall, the key consideration when designing ErlT is to address specific set of problems, and do in a way that is in line with the language design principles:

* this doesn’t make the language significantly more complex
* this doesn’t lead to explosion of complexity and implicitness in Erlang code that uses the new capabilities

## ErlT design tradeoffs

Generally, ErlT design follows the same set of tradeoffs as Erlang.

Most importantly, engineering productivity and reliability are prioritized above everything else, including runtime performance and succinctness.

### Runtime performance and optimizations

Certain semantics is not possible to express directly in the language. For example:

* manipulation on mutable memory
* control over low-level memory layout
* manual memory management
* shared memory for concurrent applications


This prohibits certain classes of optimizations in sequential Erlang code.

### Succinctness

Some logic can be expressed more succinctly in high-level imperative languages such as Python. For example, Erlang doesn’t have constructs for imperative control-flow: generic loops, local returns and breaks, mutable updates.

To keep the language simple and explicit, the language doesn’t offer some capabilities commonly found in other high-level declarative languages that make programs more succinct. For example: operator overloading, pipe operator.

### Use of Static typing vs Dynamic typing

Although ErlT is designed with static typing as a first-class language capability, the language doesn’t mandate the extent static typing should be used in each particular case. For example, ErlT code automatically migrated from Erlang is going to be dynamically typed by default.

However, for new ErlT codebases, there is an expectation most code would benefit from being written in statically typed paradigm. For this reason, in ErlT design, we prioritize ergonomics of reading and writing of statically typed code over ergonomics of dealing with dynamically typed code.

But even then choices of the typing discipline may vary for different engineering cultures, and parts of the codebase. The effect of having dynamically typed code will be different in each particular case. But generally, the more dynamically-typed code there will be left around, the fewer benefits users will get in terms of IDE functionality, reliable refactoring, well-documented API, and so on.
