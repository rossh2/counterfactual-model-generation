# Future Work

## Fundamental Questions

* Should _take_ and _retake_ have the same propositions - i.e. if a model contains the presupposition _Charlie takes the test_ and we add the information _Charlie retakes the test_, does that overwrite the presupposition or does the model contain both? This relates to the issue below about handling (non-)repeatable predicates more nicely.
* What is the best way to handle words such as _tomorrow_ and _yesterday_? They're analysed as (pronominal) nouns apparently, but they can occur in places that other nouns cannot and so need to be handled differently in the toy grammar. Do other nouns exist which behave in this way but are not temporal? German has _zuhause_ for _at home_. Should my grammar account for this even if English doesn't have any?

## Implementation

### Must Have

*Completed*

### Should Have

* Parse presuppositions - can be somewhat hard-coded if necessary, but handle them somehow. (When handling presuppositions, switch from _his calculator_ to _Mia's calculator_ and extend grammar accordingly)
* Compositional semantics for event meanings with lambda calculus in `Semantics.hs` and `DataStructures.hs`, rather than strings

### Nice to Have (Stretch Goals)

* Handle special accommodation options for _didn't fail_
* Model repeatability of e.g. test-taking in lexicon (is there a better way than just generating the presupposition of test not taken?) 
Mesh this nicely with the  `Repetition` flag on _retake_, right now _retake_ is an entirely separate predicate.
* Teach model generation that any time in past (e.g. _yesterday_) conflicts with the general time `Past` when generating models
* Handle/check how well mixed time conditionals e.g. _If Charlie went to the review session today, he would pass his test tomorrow_. or _If he had gone shopping yesterday, then he would still have food (now/in the future)._ are handled. (Some of them should be handled now that each event has its own time, but certainly the latter doesn't match the heuristic used for consequent times.) See the note on `inferConsequentTime` in `Parsing.hs`.
* Handle expanding event horizon / reverse Sobel sequences

## Code Style

* No need to pass `tree` to `parseSimpleSentence` - just reconstruct it on the spot from the TP

## Testing & Error Checking

* In `ModelGeneration > addProp`, consider checking whether set of props is actually consistent rather than just relying on the first match being the only match
* Unit tests for specific model generation
* Add ice-cream examples to unit tests
* Add _not taken yet_ examples to unit tests
* Unit tests for `combineModels` specifically? (Or handle only by checking felicity for discourses, which calls this)
* Consider using "proper" Haskell unit tests (with unit testing framework)
* Update `Main.hs` to be a useful interface (separate to `ParsingTests.hs` and `ModelGenerationTests.hs`)
