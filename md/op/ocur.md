# Occurrence Count
Occurrence Count (`⊒`) is a somewhat more sophisticated take on the idea behind Mark Firsts: instead of just testing whether a cell is a duplicate, it returns a number indicating how many previous cells match it. This means that Mark Firsts can be implemented with `0=⊒`.

        ⊒   2‿7‿1‿8‿1‿7‿1‿8‿2‿8‿4

        ≍⟜⊒ 2‿7‿1‿8‿1‿7‿1‿8‿2‿8‿4

        0=⊒ 2‿7‿1‿8‿1‿7‿1‿8‿2‿8‿4  # First appearances marked

While Occurrence Count maintains more information than Mark Firsts, it's still not as much as [Classify](#classify). We can swap many digits around while keeping the occurrence counts the same; Classify would detect these changes.

        ⊒   7‿1‿2‿8‿7‿1‿1‿2‿8‿8‿4

One easy example with Occurrence count is to take a list that has duplicates and return exactly one copy of each duplicate element. Taking each value where the count is 1 ensures that the result has no duplicates, and that every cell that appears twice or more in the argument is represented in the result, since the second occurrence has count 1. Results are ordered by the position of these second occurrences, so a different method might be needed if the ordering is important.

        (1=⊒)⊸/ "aaaabcddcc"

An interesting combination is Occurrence Count applied to the result of [Indices](replicate.md#indices) (`/`). The result counts up to each number from the argument in turn; in other symbols, it's `∾↕¨`. This version is interesting because it doesn't create any nested arrays, just lists of natural numbers.

        ⊒ / 2‿3‿4

A more efficient way when `⊒` doesn't have a fast implementation is `` /(¯1⊸⊑↕⊸-⊏⟜»)+` ``, but that's clearly quite a bit more complicated.

# Progressive Index of
Progressive Index of (`⊒`), as the name and glyph suggest, is a more sophisticated variant of Index of. Like Index of, it returns either `≠𝕨` or an index of a cell from `𝕨` that matches the given cell of `𝕩`. Unlike Index of, no index can ever be repeated (but `≠𝕨` can). Progressive Index of returns the index of the first *unused* match, provided there's still one left.

        "aaa" ⊒ "aaaaa"

        "aaabb" ⊒ "ababababab"

Above we said that `𝕩∊𝕨` is `(𝕨⊐𝕩)<≠𝕨`, so that `⊐˜<≠∘⊢` is an implementation of Member of. The corresponding `⊒˜<≠∘⊢` implements *progressive* member of, that is, membership on [multisets](https://en.wikipedia.org/wiki/Multiset). So if `𝕩` contains two copies of `'a'`, only the first two instances of `'a'` in `𝕨` are considered to belong to it. And like membership is useful for set intersection and difference, progressive membership gives multiset versions of these.

        "aabbcc" (⊐˜<≠∘⊢) "baa"

        "aabbcc" (⊒˜<≠∘⊢) "baa"

        "aabbcc" ((⊒˜=≠∘⊢)/⊣) "baa"  # Multiset difference

This primitive gives an interesting way to implement the [ordinals](order.md#ordinals) pattern that might be easier to understand than the classic `⍋⍋` (it's probably a little slower though). The idea is to use the sorted array as the left argument to `⊒`. Now the index returned for each cell is just where it ended up in that sorted order. If we used ordinary Index of then equal cells would share the smallest index; Progressive Index of means ties are broken in favor of earlier cells.

        ⍋∘⍋ "adebcedba"

        ∧⊸⊒ "adebcedba"

        ∧⊸⊐ "adebcedba"  # Ties included

Here's a goofy code golf tip: if the two arguments to Progressive Index of are the same, then every cell will be matched to itself, because all the previous indices are taken but the current one does match. So `⊒˜` is the same as `↕∘≠`.

        ⊒˜ "anything at all"
