# JoLang JoScript

> A ML / LISP passing as Python

JoLang (jo for short) is a general purpose language, with dope ass syntax.
The intention is to write a language that both compiles to run on the web,
and run natively.

[namesake]: https://www.youtube.com/watch?v=Xg5wISNSDvQ

## About

### Motivation

I wanted to make a language, so made a language.

### Design Goals

- Types that aren't a pain in the ass...
- Pleasurable to write, pleasurable to debug, pleasurable to read
  - A pleasant development feedback loop
  - Tool friendly, with a compiler api
  - This is partial subjective, like my preference for lisps hypen snake case
    over camel case, or white space as a block delimiter.
  - I love foot guns as much as the next guy, but if I'm about to blow off
    my foot I'd like it to be super obvious.
  - Shouldn't need to import `Debug.trace` to start debugging, or know the
    exact line number in my repl.
- High Expressive and Modular without insisting on the alignment of the stars
- Default to explictity, but allowing implictity when reasonable
  - Super vague bullet point, but basically strict a good balanced between
    implict and explict stuff, for example:
      - type casting, always explict
      - imported namespace, always explict
      - imported type classes\*, maybe implict?

> \* Note I'm not sure what mechanism I'll use for polymophism and
> shared interfaces but I'll just say type classes though out.

### 🌚  Features 🌝

> Like most of these aren't implemented yet, so think of this as a todo list

- Dope ass syntax
- Pattern matching
- Quansi quotes
- Lisp Style Macros
  - Preferably without paren soup syntax
  - At the same time I'd like some level of typing
  - Probably closer to Rusts macro system
- Mostly Functional
  - Immutable by default
  - Probably not purely functional
- ML style modules

#### Type Features

The type system is completely optional, as in you can turn it off.

> I think being able to turn off a type system might be good for
> fast iteration, and an improved feedback loop for the developer.
> But I don't want this to be at the expensive of the actual type
> system, like you see in Groovy when you try to do actual types
> there are some seriously awkward.

- Higher kinded types
- Optionals
- Varient types
- Record types
- Immutable by default (like rust)
- Global type inference

> I'm not sure how ML style modules will mix with a system like type classes

### Current syntax

I'm honestly suprised by the number of library/language projects that don't
show you examples of syntax in the read me, so here is some syntax. Also since
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

## Compiler API

Right now the compiler barely even generates anything, it just reads the
source, but you can inspect what it's doing by reading the debug output.

```
$ jo build MAIN_FILES

# debugging the output for the various compilation passes, passes include
#
#   - text:lexer
#   - text:block
#   - text:parse
#
$ jo build -d COMPILER_PASS MAIN_FILES

# pretty printed compiler output for the various compilation passes
$ jo build -d COMPILER_PASS --pretty MAIN_FILES
```

> The thought process behind this is to have the subcommands like build,
> repl, debug, etc, what ever makes sense. The debug output should be also
> usable by tooling like an IDE or linter or whatever.

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

