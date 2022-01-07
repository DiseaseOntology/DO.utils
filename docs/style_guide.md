# Documentation Style Guide

## General
Most text should be written in sentence case, use proper punctuation, and end in a period.


## Functions

### Names
1. Should use words, preferably unabbreviated, separated by "_".
2. Should be as succinct as possible, while conveying clearly what the function does.
3. Should start with a verb, such that they imply taking action.

#### Examples
Good: `cast_to_string()`


### Title
1. Should _VERY_ succinctly describe what a function does.
2. Should start with an infinitive verb (without "to").
3. Should be capitalized (as is standard for titles).
4. Should end without punctuation.

#### Examples
Good: "Create a String from Inputs"


### Description
Should be formatted in an active voice and immediately describe the primary goal of the function, preferably without naming it (e.g. the description for `sum` would be "Adds numbers together.")


### Arguments

#### Names
1. Should use words, preferably unabbreviated, separated by "_".
2. Should be as succinct as possible, while conveying clearly what it represents.
3. Should be nouns.
4. Should be consistent and ordered consistently across functions, where possible.

#### Descriptions
1. Should be formatted as sentences, with the first word capitalized first word and with a period at the end, except when ending in bulleted lists.
2. Should, **in the first sentence**, describe clearly what the argument represents at the beginning (in the abstract, for human understanding) and list the acceptable input types at the end.
3. Should _not_ include lengthy descriptions or descriptions relating to multiple arguments. These should be written in a title `@section` and, where necessary, have pointers from applicable arguments (e.g. "(see {section_name})").
