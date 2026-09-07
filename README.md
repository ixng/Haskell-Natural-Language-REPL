# λGPT — Natural Language REPL

A read-eval-print loop in Haskell that answers questions written in plain
English. Built with parser combinators rather than a language model, so every
answer is derived from the grammar rather than predicted.

## What it does

Hello.
Hi there!

What day is it tomorrow?
Tomorrow is Sunday.

How long ago was 2026-02-01?
2026-02-01 was 13 days ago.

What is two plus three times four?
The answer is twenty.

Remember that the sky is blue.
Okay.

Tell me about the sky.
Sure - the sky is blue.

What is that plus five?
The answer is twenty-five.


## How it works

Input is parsed into a `Request` algebraic data type, and `respondTo` pattern
matches on that type to produce a reply. Parsing and responding are separated,
so adding a feature means adding a constructor, a parser, and a case.

**Parser combinators.** Small parsers compose into larger ones. The arithmetic
grammar is built from a number parser and three operator parsers rather than
written as one function.

**Arithmetic follows natural language, not maths.** `two plus three times four`
evaluates left to right as `(2 + 3) × 4 = 20`, because that is how the sentence
reads aloud. Numbers are parsed from and printed back to English words.

**State threads through the loop**, so the chatbot remembers stored facts and
the result of the previous expression, which `that` refers back to.

## Structure

src/LGPT/TUI.hs the REPL, Request type, parsing and responses
src/LGPT/Numbers.hs longhand number parser and printer
src/LGPT/Helpers.hs convenience functions
test/ test suite


## Building

```bash
stack build
stack run
stack test
```

## Notes

Written for CS141 Functional Programming at the University of Warwick,
March 2026. The skeleton (`TUI.hs` scaffolding, `Numbers.hs`, `Helpers.hs`)
was provided; the parsers, request algebra, evaluation and state handling
are mine.
