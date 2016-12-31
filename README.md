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
  - A pleasant development feedback loop, while remaining Tool friendly
    - compiler has an API
  - This is partial subjective
    - style with lisp's hypen snake case, instead of camel case
      - but lets do something retarded like outlaw the use of `_` or camelcase
      - this is something that can be established through linters and standard library functions.
    - use white space as a block delimiter
  - I love foot guns as much as the next guy, but if I'm about to blow off
    my foot I'd like it to be super obvious.
  - Shouldn't need to import `Debug.trace` to start debugging, or know the
    exact line number in my repl.
- High Expressive and Modular without insisting on the alignment of the stars
  - declaritive syntax can be expressed in terms of imparitive
    - how classes in javascript can be implemented by extending a prototype
    - async/await in javascript can be expressed as a series of nested promises
  - statements are expressions like coffeescript/rust/lisp 😄
    - except assignment because that's unforgivable 😡
  - overloadable syntax sugar
    - user defined syntax constructs can have their own keywords like `return`,
      `yield` that appear similar to language defined keywords, without having
      a large dictionary of forbidden keywords
        - See contextuals
    - syntax should not discourage use of the correct approach
      - like in vanilla haskell, it's much easier to use a linked list of
        64 bit characters as a string verse `Data.String`, or a list of pairs
        instead of `Data.Map` or `Data.HashMap`.
      - Async IO doesn't need to be scary like it is in or eye sore
        like callback soup javascript. Having something like async/await
        from C# / Python / Javascript is very desirable
    - list/set/map syntax sugar should be overloadable
    - async await should be expressable in syntax used else where in the
      language, without being to vague about it's indent... Kind of like
      a monad but not scary?
  - Default to explictity, but allowing implictity when reasonable
    - Super vague bullet point, but basically strict a good balanced between
      implict and explict stuff, for example:
        - type casting, always explict
        - imported namespace, always explict
        - utilization of a type class when type being dispatched has single
          implementation imported into the current namespace
        - return statements? I'm not realy sure if I want to make this implict,
          maybe in single line lambdas

> \* Note I'm not sure what mechanism I'll use for polymophism and
> shared interfaces but I'll just say type classes though out.

### Feature wish list 🌝

> Like most of these aren't implemented yet, so think of this as a todo list

- Dope ass syntax
- Pattern matching
- Quansi quotes
- multiline strings and string interpolation
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

### Actual features 🌚

Think of this is a list of things that are partially implemented

#### Contextuals

Keywords like `return` are prefixed with a `.` like `.return`, which allows
user define syntax constructs to have keywords that more naturally fit with
their constructs. The type function or value of constucts **Contextuals**
are determined on a case by case basis. Defining them is yet to be decided.

```jo
# a function has the contextuals .return
double := | x |
  .return [* x 2]

gcd := | x y |
  if [< x y] then:
    .return gcd(x, y)

  modded := % x y
  if [== modded 0]
    .return y
  else
    .return gcd(y, modded)
```

#### Lambdas

> TODO

#### Data Literals

> TODO, but basically strings

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

