# HTriple

[![CircleCI](https://circleci.com/gh/Cypher1/HTriple/tree/master.svg?style=svg&circle-token=044e2d53c34ded9fef2189ab4f828a2eae3a8dd5)](https://circleci.com/gh/Cypher1/HTriple/tree/master)

## Plan

- Construct a set of logics and forms that can represent statements and requirements
- Write some simple (proof of concept) solvers for them
- Use these solvers and representations to construct and find missing requirements in systems of Hoare triples
- Write a domain specific language that constructs these Hoare triples and building small programs out of them
- Use the solvers to prove properties of the small programs (e.g. memory safety, termination/non-termination, use of IO, etc.) where it is possible to do so.

## Layout

- app/ holds all the code for the executable that can be used for experimenting with the DSL
- test/ holds all the test specific code
- lib/ holds all the core code for defining the DSL, solvers and data structures
- examples/ holds some example code in the (experimental) HTriple interface definition language, these are meant to be used as tests for the parser (which in turn is meant to facilitate writing proofs about code using HTriple)

----
Joshua Pratt
