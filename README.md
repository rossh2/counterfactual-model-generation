# Minimal Models for Counterfactuals and Conditionals

This repository contains the code for my 2020 Master's Thesis in CL at Brandeis University. As of March 16, 2020, this is a work in progress which still contains a number of TODOs.

This Haskell codebase shows how to generate minimal models for conditionals of three types: indicative, subjunctive and counterfactual (subjunctive past).
Specifically, it focuses on the distinction of whether the conditional deals with a pair of events at different times which contrast, i.e. the 'same event' has different outcomes.

* Indicative, non-contrasting: _If Charlie brings his calculator tomorrow, Charlie will pass his test._ or _If Charlie takes his test tomorrow, Charlie will pass his test._ (assuming Charlie hasn't taken his test yet)
* Subjunctive, non-contrasting: _If Charlie brought his calculator tomorrow, Charlie would pass his test._
* Counterfactual, non-contrasting: _If Charlie had brought his calculator yesterday, Charlie would have passed his test._
* Indicative, contrasting: _If Charlie re-takes his test tomorrow, Charlie will pass his test._ (assuming Charlie already took his test, but wants to re-take it)
* Subjunctive, contrasting: _If Charlie re-took his test tomorrow, Charlie would pass his test._
* Counterfactual, contrasting: _If Charlie had taken his test yesterday/tomorrow, Charlie would have passed his test._

It also discusses the differences that arise when we include negation, such as _If Charlie had taken his test yesterday/tomorrow, Charlie wouldn't have failed his test._

## Code Structure

`DataStructures.hs` contains (unsurprisingly) the data structures representing this problem, such as for propositions, times, and the various types of conditional. The model generation happens in `ModelGeneration.hs`. To see this in action and check whether the generated models are correct, `ParsedExamples.hs` declares a number of example conditionals which can be checked using `Main.hs`: calling its function `main` prints all the example conditionals along with their generated models; smaller sets of conditionals can also be printed using the other functions declared in `Main.hs`.