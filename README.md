# Registry workshop [![Build Status](https://github.com/etorreborre/registry-workshop/workflows/ci/badge.svg)](https://github.com/etorreborre/registry-workshop/actions)

##### *It's functions all the way down* <img src="doc/images/unboxed-bottomup.jpg" border="0"/>

### Presentation

This project shows how the registry library is implemented:

 1. with the basic data structure supporting the creation of values from functions
 2. with additional features to make the creation of values statically safe
 3. with additional features to "tweak" the creation of values
 4. with additional features to define a more concise API

### Creating values from functions

In this section we

 1. use `Data.Dynamic` to put functions and values in a list of untyped values
 2. implement an algorithm to retrieve a value from the list, based on its type, by doing recursive function applications

### Making it safe

How do we know if it is possible to build a value, given the functions present in the registry?

We know this by tracking input and output types for the functions, then by making some inclusion checks,
using type families and type classes to check what can be built.

### Making it ergonomic

#### The `<:` operator

This operator allows to append both values and registries together.

#### Lifting functions

When working with application wiring for example it useful to be able to add a mixture of pure and effectful
functions to the registry. But then the effectful output of a function cannot be the input of another function.
We need then to "lift" pure functions to effectful functions in order for the algorithm to proceed.
This uses typeclasses.

### Additional features

Some useful features are not demonstrated in this project because they just build on the existing structure:

 - `val` vs `fun` to be able to display values with `Show` instances
 - specialization of a value creation "under" a specific type (or list of types)
 - full reporting for the attempted creation of a value
 - creation of a dependency graph as a dot graph
