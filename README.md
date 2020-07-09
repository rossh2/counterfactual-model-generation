# Minimal Models for Conditionals

This repository contains the code for my 2020 Master's Thesis in Computational Linguistics at Brandeis University. The full thesis is available at the [Brandeis Institutional Repository](http://bir.brandeis.edu/handle/10192/3752); a shorter paper (and poster) presented at WeSSLLI 2020 is available on [OSF](https://osf.io/jpqad/).

This Haskell codebase shows how to generate minimal models for conditionals of three types: indicative, subjunctive and counterfactual (subjunctive past).
Specifically, it focuses on the distinction of whether the conditional deals with a pair of events at different times which contrast, i.e. the 'same event' has different outcomes.

* Indicative, non-contrasting: _If Charlie brings his calculator tomorrow, Charlie will pass his test._ or _If Charlie takes his test tomorrow, Charlie will pass his test._ (assuming Charlie hasn't taken his test yet)
* Subjunctive, non-contrasting: _If Charlie brought his calculator tomorrow, Charlie would pass his test._
* Counterfactual, non-contrasting: _If Charlie had brought his calculator yesterday, Charlie would have passed his test._
* Indicative, contrasting: _Charlie failed his test. If Charlie re-takes his test tomorrow, Charlie will pass his test._ (assuming Charlie already took his test, but wants to re-take it)
* Subjunctive, contrasting: _Charlie failed his test. If Charlie re-took his test tomorrow, Charlie would pass his test._
* Counterfactual, contrasting: _Charlie failed his test. If Charlie had taken his test yesterday/tomorrow, Charlie would have passed his test._

It also discusses the differences that arise when we include negation, such as _Charlie failed his test. If Charlie had taken his test yesterday/tomorrow, Charlie wouldn't have failed his test._

# Code Structure

The codebase can be broken down into two broad parts: (1) taking syntactic trees from a small grammar and processing/parsing them ready for model generation, and (2) generating minimal models from these parsed trees. 

The grammar is declared in `Grammar`, specifically in `Lexicon.hs`, `Grammar.hs`, and `Features.hs`. This grammar is completely agnostic of the fact that it will be used for minimal model generation - in particular, there are no dependencies from these modules to other parts of the code - except that only the parts of the grammar that are needed to form simple sentences and conditionals are declareed. However, it should be easy to extend the grammar to handle more complex sentences. Unlike the grammar and lexicon in e.g. van Eijck and Unger (2010), the lexicon in particular bears more resemblance to the lexicon used in traditional linguistics: we declare syntactic categories and then list items which instantiate them, with features and verb forms. However, as is common in computational linguistics, the grammar still declares all the allowed rules individually in contrast to the now-standard X-Bar theory.
A large quantity of examples are declared in `Test/TreeExamples.hs`.

Sentences from this grammar are parsed in `Parsing`, specifically in `Parsing.hs` into the data structures (events, propositions, parsed sentences) declared in `Parsing/DataStructures.hs`. There are two parts to this: (1) extracting the features required for model generation, such as separating events and times, extracting presuppositions, and splitting conditionals into the antecedent and the consequent, and (2) mapping the lexical items to a compositional semantics which gives the meaning of the propositions/events. The choice of compositional semantics is thus arguably somewhat independent of the rest of the model generation, although it influences the ease with which presuppositions can be formulated and extracted. 
Examples for these data structures, including fully parsed sentences enumerating every type of conditional we want to consider, can be found in `Test/PropExamples.hs` and `Test/ParsedSentenceExamples.hs`. `Test/ParsingTests` uses these examples to assert that all the sentences are parsed correctly.

Finally, model generation is located in `Model`: it declares the structure of the model in `ModelStructures.hs`, with time handling split out into a separate module `Times.hs` for ease of importing (and because there is a lot of verbose mapping code related to time and tenses). Model generation occurs in `ModelGeneration.hs`, which has two primary methods: one to generate minimal models for sentences, and another to combine two models. This is tested in `Test/ModelGenerationTests.hs`, which exhaustively tests that a minimal model is either generated or not for all the possible felicitous and infelicitous conditional discourse examples involving _pass_ and _fail_. 

In the future, `Main.hs` will provide a convenient entry point to the whole codebase to pass in a sentence and view the generated model, but right now the two test classes are the best way to interact with the code.
