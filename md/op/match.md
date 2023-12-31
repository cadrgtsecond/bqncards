# Depth
To find the depth of an array, use Depth (`≡`). For example, the depth of a list of numbers or characters is 1:

        ≡ 2‿3‿4
        ≡ "a string is a list of characters"

Depth is somewhat analogous to an array's [rank](shape.md) `=𝕩`, and in fact rank can be "converted" to depth by splitting rows with `<⎉1` ([Enclose](enclose.md) [Rank](rank.md) 1), reducing the rank by 1 and increasing the depth. Unlike rank, Depth doesn't care at all about its argument's shape:

        ≡ 3‿4⥊"characters"
        ≡ (1+↕10)⥊"characters"

Also unlike rank, Depth *does* care about the elements of its argument: in fact, to find the depth of an array, every element must be inspected recursively.

        ≡ ⟨2,3,4,5⟩
        ≡ ⟨2,<3,4,5⟩
        ≡ ⟨2,<3,4,<<<5⟩

The depth of an array is the maximum of its elements' depths, plus one. The base case, an atom (including a function or modifier), has depth 0.

        ≡ 'c'
        F←+ ⋄ ≡f
        ≡ ⟨'c',f,2⟩

Using `0=•Type` to test whether `𝕩` is an array, as well as the [Choose](choose.md) modifier, we can write a recursive definition of Depth.

    Depth ← (0=•Type)◶0‿{1+0⌈´Depth¨⥊𝕩}

The minimum element depth of 0 implies that an empty array's depth is 1.

        ≡⟨⟩
        ≡2‿0‿3⥊0

# Match
The primitive Match (`≡`) tests whether its two argument arrays are considered equivalent in BQN, returning `1` if so and `0` otherwise. Not Match (`≢`) is the opposite, returning `1` if the two arrays aren't equivalent and `0` if they are.

        "abc" ≡ 'a'‿'b'‿'c'
        4 ≢ <4

Match always gives the same result as [Equals](arithmetic.md#comparisons) (`=`) when both arguments are atoms, but the two functions are extended to arrays differently: while the pervasive Equals maps over array arguments to return an array of results, Match compares them in totality and always returns one boolean (it never gives an error). Match is the basis for BQN's [search](search.md) and [self-search](selfcmp.md) functions.

        "abc" = "acc"
        "abc" ≡ "acc"

        "abc" = "ab"  # Mismatched shapes
        "abc" ≡ "ab"

Match compares arrays based on their fundamental properties—[shape](shape.md) and elements—and not the [fill element](fill.md), which is an inferred property. Since it can be computed differently in different implementations, using the fill element in Match could lead to some confusing results. Even if the implementation doesn't define a fill for `'a'‿'b'‿'c'`, it should still be considered to match `"abc"`.

To give a precise definition, two arrays are considered to match if they have the same shape and all corresponding elements from the two arrays match. Every array has a finite [depth](depth.md), so this recursive definition always ends up comparing non-arrays, or atoms. And because an array never matches an atom, the result if only one argument is an atom is `0`. The interesting case is when both arguments are atoms, discussed below.

## Atomic equality

Atoms in BQN have six possible [types](types.md): number, character, function, 1-modifier, 2-modifier, and namespace. Equality testing isn't allowed to fail for any two arguments, so it needs to be defined on all of these types.

Starting with the easiest rules, values with different types are never equal to each other.

        ⟨'a', +, 3⟩ = ⟨-⟜», '+', 3˙⟩

Two characters are equal when they have the same code point. Numeric equality depends on the number system in use, but probably works about how you expect. If you're coming from APL, note that BQN doesn't use comparison tolerance. To see if two floats are roughly equal you'll need to write a tolerant comparison yourself, but how often do you really need to do this?

        'x' = "wxyz"

        1.25 = 1 + 0.25

Operations and namespaces are more difficult. Here there are three cases:
- Primitives are equal if they have the same glyph.
- Compound functions or modifiers are split into components.
- Block instances or namespaces are equal if they are the same instance.

The first two are fairly similar to how numbers and arrays work. Primitives and compounds like trains, or modifiers with bound operands, are immutable, so they're defined purely by what components they contain.

        ⟨+,-,×⟩ = ⟨+,-,÷⟩

        ⟨+ - ×⟩ = ⟨+ - ÷⟩  # Compare two three-trains component-wise

        ⟨+ - ÷⟩ = ⟨+ - ÷⟩

This approach can't tell you whether two functions are mathematically different—that is, whether they ever return different results given the same arguments (this is an undecidable problem, and also gets confusing since "different" is included in its own definition). However, if two functions compare equal, then they will always return the same results.

### Block equality

The final point above about block instances is subtler. An instance of a block function or modifier is [mutable](lexical.md#mutation), meaning that its behavior can change over the course of a program. Consider the following two functions:

        F‿G ← { a←10 ⋄ {a+𝕩}‿{a↩𝕩} }

        F 5   # One result
        G 8
        F 5   # Another result—the definition of insanity!

(A side note is that BQN restricts what can cause these side effects: they can only happen by calling a block function or modifier, and never a primitive or purely [tacit](tacit.md) operation). Now suppose we share the value of `F` with another variable like `F1` below. When we apply `G`, the result of `F` might change, but so does `F1`! This effect is called [aliasing](https://en.wikipedia.org/wiki/Aliasing_(computing)).

        F1 ← F
        {𝕏 6}¨ F‿F1

        G 3
        {𝕏 6}¨ F‿F1

In some cases you might not be able to demonstrate aliasing so cleanly. A function such as a random number generator changes its own state, so calling one function will change the other. But comparison tells you directly whether two blocks are the same.

        f = f1

As with other kinds of functions, just because two blocks always behave the same doesn't mean they are equal. Any function that's written as `{𝕩}` will always work the same as other functions spelled that way, but the two functions below are different instances because they come from two different places in the source code.

        =´ {𝕩}‿{𝕩}

Two blocks that come from the same source code location could also be different. Consider the following code, featuring a function that creates block functions:

        Gen ← { a←𝕩 ⋄ {a×𝕩} }
        t2 ← Gen 2
        t3 ← Gen 3
        {𝕏 4}¨ T2‿T3

These functions both have the definition `{a×𝕩}`, but give different results! They are different *instances* of the same block, and have different environments: for `T2`, `a` is `2`, and for `T3`, it's `3`.

        t2 = t3

Some definitions should help to make things clearer. A "block" is not actually a BQN value, but a region of source code enclosed in `{}` brackets. When the program encounters a block function or modifier, it creates an instance of this block, and then uses this instance in the rest of the expression (actually, an immediate block also creates an instance, but this instance is immediately run, and discarded when it finishes, so it can't be accessed as a value). Every time the function `Gen` is run, it evaluates the statements it contains, and the second statement `{a×𝕩}` creates a block instance. So `Gen` creates a new block instance each time. This is necessary for `Gen` to work correctly: each time it runs, it creates a new scope, so it needs to create a new function that will be tied to that scope.
