# JoLang JoScript

> A ML / LISP passing as Python

JoLang JoScript (jojo/jo for short) is a general purpose language,
with dope ass syntax. The intention is to write a language that
both compiles to run on the web, and run natively.

[namesake]: https://www.youtube.com/watch?v=Xg5wISNSDvQ

## About

### Motivation

I wanted to make a language, so made a language.

### Design Goals

- Types that aren't a pain in the ass...
- Pleasurable to write, pleasurable to debug, pleasurable to read
  - I love guns as much as the next guy, but if I'm blowing off my
    foot I feel I ow everyone the courtesy things are about to get
    a little messy, you feel me?
- High Expressive and Modular without insisting on the alignment of the stars
  - I would prefer not to be overly redundant
    and have to repeat myself over and over, when context and
    intent is clear and unambigious.
- Defaulting to explictity, but allowing implictity where reasonable
  - Read this as I don't plan to go to town with syntaxtic sugar
    but there are expections to everything and they should be well
    justified.

### 🌚  Features 🌝

> Like most of these aren't implemented yet, so think of this as a todo list

- Dope ass syntax
- Pattern matching
- Quansi quotes
- Macros
- Mostly Functional
- Immutable by default
- ML style modules

#### Type Features

The type system is completely optional, as in you can turn it off.

- Optionals
- Varient types
- Record types
- Immutable by default (like rust)
- Dependant types
- Global type inference

### Current syntax

I swear to god, I'm honestly suprised by the number of languages,
libraries, tools, whatever don't show you examples of syntax. Lol,
so since this language is currently in development this example may
be out of date but I try to keep this up to date.

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

## To build

```sh
make all
```

## An Example

```sh
./build/src/main.native examples/simple.script
```

## Dependencies

I don't particually understand the ocaml packaging ecosystem
right now, but once I do I'll update this.

