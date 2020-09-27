# StErlang code coverage

Ideally, we would like to get 100% code coverage of stErlang with tests.
But since stErlang is specialised to some restrictions it makes it a bit 
non-trivial.

Fortunately, there is only a handful of such specialisations, and we can 
document them and disable coverage checking for corresponding small portions of 
stErlang code.

There are also states which stErlang should not get into (by design). 
The corresponding portions of code explicitly throw `IllegalStateException` and
are also excluded from code coverage reports.

The mechanism for excluding code from coverage is 
`$COVERAGE-OFF$`/`$COVERAGE-ON$` comments.

## Extensible shapes

[comment]: <> (extensible-shapes)

There are 2 places which can be covered only with extensible shapes
(supporting adding and deleting fields) in the first place with integration 
tests. Those places are commented with `$COVERAGE-OFF$ extensible-shapes`.

Options to fill the gap:

- We may consider to actually support adding and deleting fields in stErlang
internal language in the future (but not in the erlT surface language).
- Just to write simpler unit tests checking `Unify` directly.

## Interactive IO

[comment]: <> (interactivity)

Printing feedback (errors and warnings) to console is not tested in integration
tests. Those places are commented with `$COVERAGE-OFF$ interactivity`.

It is possible, however, to test it - since `System.out` can be mocked.
