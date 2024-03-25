# rohlang2

A point free, **purely** functional language.

That doesnt mean rohlang's functions are pure.. it means there are only functions and combinators.  


## plans
- types (product, sum)
  - higher kinded types
- named functions
- modularized
- proper errors instead of panic!()
- rich stdlib (more combinators, etc)
- set with order/algebra
- ability to extend/hack on the language from the language itself
  - serve as a base for other languages (DSL)
  - aliases, etc
- SMT solver based verification

## philosophy
- rohlang is meant to be embedded in other languages
- rohlang is meant for me to use it and currently will not lend itself to widespread adoption
- rohlang is meant to be abstract and simple
- rohlang is meant to have as little external dependencies as possible
- rohlang is meant to have as small a spec as possible and defer implementation to stdlib
- rohlang is meant to be secure
- rohlang is meant to be terse 
- rohlang is meant to gracefully degrade
- rohlang is meant to have abstract language features that are informed by compiler impl

