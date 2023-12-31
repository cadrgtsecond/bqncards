## The Depth modifier

The Depth 2-modifier (`⚇`) is a generalization of [Each](map.md) that allows diving deeper into an array. To illustrate it we'll use a shape `4‿2` array of lists of lists.

        ⊢ n ← <⎉1⍟2 4‿2‿2‿3⥊↕48
        ≡ n

Reversing `n` swaps all the rows:

        ⌽ n

Depth `¯1` is equivalent to Each, and reverses the larger lists, while depth `¯2` applies Each twice to reverse the smaller lists:

        ⌽⚇¯1 n
        ⌽⚇¯2 n

While a negative depth tells how many levels to go down, a non-negative depth gives the maximum depth of the argument before applying the left operand. On a depth-3 array like above, depth `2` is equivalent to `¯1` and depth `1` is equivalent to `¯2`. A depth of `0` means to descend all the way to the level of atoms, that is, apply [pervasively](arithmetic.md#pervasion), like an arithmetic function.

        ⟨'a',"bc"⟩ ≍⚇0 ⟨2‿3,4⟩

With a positive operand, Depth doesn't have to use the same depth everywhere. Here, [Length](shape.md) is applied as soon as the depth for a particular element is 1 or less, including if the argument has depth 0. For example, it maps over `⟨2,⟨3,4⟩⟩`, but not over `⟨11,12⟩`, even though these are elements of the same array.

        ≠⚇1 ⟨1,⟨2,⟨3,4⟩⟩,⟨5,⟨6,7⟩,⟨8,9,10⟩⟩,⟨11,12⟩⟩

[Like the Rank modifier](rank.md#multiple-and-computed-ranks), the right operand `𝔾` is really a function that returns the depth to be used, and that depth can be a single number, or a list of one to three numbers. One number gives the same depth to all arguments, two correspond to `𝕨` and `𝕩`, and three to monadic `𝕩`, and dyadic `𝕨` and `𝕩`.
