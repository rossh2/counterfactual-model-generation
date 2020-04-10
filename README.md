# Minimal Models for Counterfactuals and Conditionals

This repository contains the code for my 2020 Master's Thesis in CL at Brandeis University. As of March 25, 2020, this is a work in progress which still contains a number of TODOs.

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

# Remaining Work

## Fundamental Questions

### Model Generation

* What are the felicity judgements for indicative conditionals?
* Do counterfactuals with no time contrast ("calculator examples") have the same felicity judgements as counterfactuals which change the date of the event? (I'm fairly sure they do, but check on data.)
* Should _take_ and _retake_ have the same propositions - i.e. if a model contains the presupposition _Charlie takes the test_ and we add the information _Charlie retakes the test_, does that overwrite the presupposition or does the model contain both? This relates to the issue below about handling (non-)repeatable predicates more nicely.

### Data Structures and Grammar

* What is the correct name for words such as _tomorrow_ and _yesterday_? They're analysed as (pronominal) nouns apparently, but they can occur in places that other nouns cannot and so need to be handled differently in the toy grammar. Do other nouns exist which behave in this way but are not temporal? German has _zuhause_ for _at home_. Should my grammar account for this even if English doesn't have any?

## Missing Implementation

### Must Have

### Should Have

* Parse presuppositions - can be somewhat hard-coded if necessary, but handle them somehow
* Compositional semantics for event meanings, rather than strings

### Nice to Have (Stretch Goals)

* Handle special accommodation options for _didn't fail_
* Model repeatability of e.g. test-taking in lexicon (is there a better way than just generating the presupposition of test not taken?) 
Mesh this nicely with the  `Repetition` flag on _retake_, right now _retake_ is an entirely separate predicate.
* Teach model generation that any time in past (e.g. _yesterday_) conflicts with the general time `Past` when generating models
* Handle/check how well mixed time conditionals e.g. _If Charlie went to the review session today, he would pass his test tomorrow_. or _If he had gone shopping yesterday, then he would still have food (now/in the future)._ are handled. (Some of them should be handled now that each event has its own time, but certainly the latter doesn't match the heuristic used for consequent times.)
* Handle expanding event horizon / reverse Sobel sequences

## Testing & Safety

* In `ModelGeneration > addProp`, consider checking whether set of props is actually consistent rather than just relying on the first match being the only match

* Unit tests for specific model generation
* Add ice-cream examples to unit tests
* Add _not taken yet_ examples to unit tests
* Unit tests for `combineModels` specifically? (Or handle only by checking felicity for discourses, which calls this)
* Update `Main.hs` to be a useful interface (separate to `ParsingTests.hs` and `ModelGenerationTests.hs`)
