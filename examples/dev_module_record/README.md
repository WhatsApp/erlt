# Tests

## Positive tests

- ma_mod01, ma_mod02 - testing that #MA:MR works in type definitions.
- ma_mod03 - testing that #MA:MR{...} works in patterns.
- ma_mod04 - testing that E#MA:MR works in expressions.
- ma_mod05 - testing that E#MA:MR works in guards.
- ma_mod06 - testing that #MA:MR (record field index) works in expressions.
- ma_mod07 - testing that #MA:MR (record field index) works in guards.
- ma_mod08 - testing that #MA:MR (record field index) works in patterns.
- ma_mod09 - testing that #MA:MR{...} works in expressions.
- ma_mod10 - testing that #MA:MR{...} works in guards.
- ma_mod11 - testing that #MA:MR{...} works in record updates.
- ma_mod12 - testing that -record(?MODULE:r, ...) will produce global record 'm:r'.
- ma_mod13 - testing that all above cases work for the case `record(?MODULE:r, ...)` and usages of `#r` 
(without `?MODULE:`) in the current module.

The first level of testing is just to check that these modules are compiled.

All tests are written in the following manner:

```
test1_global() ->
  #long_record_name1{}.
test1_local() ->
  #m01:r1{}.
```

XXX_local illustrates one particular usage of #MA:MR construct.
XXX_global shows the desired result of the parse transformation.

So, the second level of testing is to run

```
erl2c {other necessary flags} -P ma_modN.erl
```

and observe that the internals of XXX_local and XXX_global are the same in ma_modN.P.
