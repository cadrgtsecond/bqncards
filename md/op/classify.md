# Classify

Classify is the universal self-search function, in that it preserves all the self-search information in its argument. It gives each different cell value a natural number, ordered by first appearance.

        ⊐ 5‿6‿2‿2‿5‿1

[Coupling](couple.md) the argument to the result shows how values are numbered. Each `5` is `0` in the result, each `6` is `1`, `2` is `2` in this particular case, and `1` is `3`.

        ≍⟜⊐ 5‿6‿2‿2‿5‿1

Applying Classify before another self-search function will never change the result, except in the case of Deduplicate (`⍷`), which constructs its result from cells in the argument. In particular, Classify is [idempotent](https://en.wikipedia.org/wiki/Idempotent), meaning that applying it twice is the same as applying it once.

        ∊   "dbaedcbcecbcd"
        ∊ ⊐ "dbaedcbcecbcd"

        {(𝕏≡𝕏∘⊐)"dbaedcbcecbcd"}¨ ⊐‿⊒‿∊‿⍷

### Classify and Deduplicate
Classify is also related to [Deduplicate](#deduplicate). In a way they are complements: applying both in sequence always gives a completely uninteresting result!

        ⊢ c ← >"yellow"‿"orange"‿"yellow"‿"purple"‿"orange"‿"yellow"
        ⍷ ⊐ c
        ⊐ ⍷ c

Applying both separately is a different story, and gives completely interesting results. These results contain all information from the original argument, as `⍷` indicates which cells it contained and `⊐` indicates where they were located. The function [Select](select.md) (`⊏`) reconstructs the argument from the two values.

        ⍷ c
        ⊐ c
        (⊐c) ⊏ (⍷c)

One way to view this relationship is from the perspective of linear algebra, where an idempotent transformation is called a "projection". That means that the argument might be any value but the result is part of a smaller class of values, and any argument from that smaller class is left the same. What arrays do the two functions project to? The result of Deduplicate is an array with no repeated major cells. The result of Classify is a list of natural numbers, but it also has an additional property: each number in the list is at most one higher than the previous numbers, and the first number is zero. This comes from the way Classify numbers the cells of its argument. When it finds a cell that hasn't appeared before (at a lower index), it always chooses the next higher number for it.

Applying both Classify and Deduplicate gives an array that has both properties (this isn't the case for all pairs of projections—we need to know that Classify maintains the uniqueness property for Deduplicate and vice-versa). It has no duplicate major cells, *and* it's a list of natural numbers that starts with 0 and never goes up by more than one. Taken together, these are a tight constraint! The first element of the argument has to be 0. The next can't be 0 because it's already appeared, but it can't be more than one higher—it has to be 1. The next can't be 0 or 1, and has to be 2. And so on. So the result is always `↕n` for some `n`. It's possible to determine the length as well, by noting that each function preserves the number of unique major cells in its argument. Classify does this because distinct numbers in the output correspond exactly to distinct major cells in the input; Deduplicate does this because it only removes duplicate cells, not distinct ones. So the final result is `↕n`, where `n` is the number of unique major cells in the argument.


## Index of
Index of (`⊐`) returns the index of the first occurrence of each entry in `𝕨`, or `≠𝕨` if an entry doesn't appear in `𝕨` at all.

        "zero"‿"one"‿"two"‿"three" ⊐ "one"‿"eight"‿"two"

`𝕩∊𝕨` is the same as `(𝕨⊐𝕩)<≠𝕨`. Note the reversal of arguments! In both `∊` and `⊐`, the open side points to the searched-in argument and the closed side points to the searched-for argument. Relatedly, in Select (`⊏`), the open side points to the selected argument, which is more like the searched-in argument in that its cells are generally accessed out of order (the searched-for argument is most like the selection result `𝕨⊏𝕩`).

Index of always returns exactly one number, even if there are multiple matches, or no matches at all. To find the indices of all matches, start with [Match](match.md) [Each](map.md), then apply [Indices](replicate.md#indices) (I didn't mean for it to sound so repetitive! It just happened!).

        / "letters" ≡¨< 'e'        # Many to one

        "letters" (<∘/˘≡⌜˜) "let"  # Many to many
