# JoLang JoScript

> A ML / LISP passing as Python

JoLang (jo for short) is a general purpose language, with dope ass syntax.
The intention is to write a language that both compiles to run on the web,
and run natively.

## About

- [Usage][compiler-api]
- [Design Goals][design-goals]

[compiler-api]: https://github.com/AKST/jo/wiki/Compiler-API
[design-goals]: https://github.com/AKST/jo/wiki/Design-Goals

### Current syntax

I'm honestly suprised by the number of libraries or languages that don't show
you examples of syntax in the read me, so here is some syntax. Also since
this language is currently in development this example maybe out of date but I
try to keep this up to date (A commented version is in the `examples` folder).

```jojo
.define 'akst.simple where: |out-factory|
  .import 'jo.lang
    with: '[match type lazy math eq ->]
		as: 'std

  type.assert out-factory
    implements: std.writer-factory

  Result := type.define-enum as: |self a|
    .case 'None     type: a -> [self a]
    .case 'Fizz     type: [self a]
    .case 'Buzz     type: [self a]
    .case 'FizzBuzz type: [self a]

  result-to-str := type.define-impl
    with: [[ a | Result [.that a implements: std.to-str] ]]
    assoc: std.to-str
    where: |result|
      .return
        match result of: |
          .case Result.FizzBuzz | "fizzbuzz"
          .case Result.Fizz | "fizz"
          .case Result.Buzz | "buzz"
          .case Result.None |n|
            type.assert n implements: std.to-str
            .return std.to-str n

  int-to-result := |number|
    type.assert-impl math eq in: number
    div-3 := | eq 0 [math.mod number 3]
    div-5 := | eq 0 [math.mod number 5]
    both  := | std.and [div-3] [div-5]
    .return
      std.guard |
        .case both | Result.FizzBuzz
        .case div-3 | Result.Fizz
        .case div-5 | Result.Buzz
        .case 'else | Result.None number

  main := |
    out := [out-factory.get-instance]
    writeln := |value|
      message := std.to-str value
      std.write out message
    std.for [std.range 1 100] |num|
      writeln [int-to-result num]


  .export main          behave-as: '[script main default]
```

> Since this langauge is currently being developed there's a possiblity
> this example could be out of date, there are more examples in the
> examples directoryo of this project.

## Dev Notes

### To build

```
stack build
```

### Debugging & Development

```
stack ghci
... makes changes
ghci: :reload
ghci: :main build -d text:lexer --pretty ../examples/simple.jo
... *pretty printed debug output from the lexer*
```

I just use GHCI for debugging and development with running `:reload` on
recompliation, and testing the compile output with the debug flag as
shown above.

