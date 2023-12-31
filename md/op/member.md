# Member of

<table>
  <thead>
    <tr>
      <th></th>
      <th>Name</th>
      <th>for</th>
      <th>in</th>
      <th>Return</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>∊</code></td>
      <td>Member of</td>
      <td><code>𝕨</code></td>
      <td><code>𝕩</code></td>
      <td><code>1</code> if found, <code>0</code> if not</td>
    </tr>
  </tbody>
</table>

The simplest of the search functions, Member of (`∊`) returns `1` if an entry in `𝕨` matches some entry in `𝕩`, and `0` if it doesn't.

        "green"‿"bricks"‿"cow"‿"blue" ∊ "red"‿"green"‿"blue"

The result is independent of the ordering of `𝕩`: all that matters is which cells it contains.

Member of can be used in a [train](train.md) to compute the set intersection and difference of two arrays. For example, `∊/⊣` uses `𝕨∊𝕩` to [filter](replicate.md) `𝕨` (from `𝕨⊣𝕩`), giving an intersection.

        "initial set" (∊/⊣) "intersect"     # Keep 𝕩

        "initial set" (¬∘∊/⊣) "difference"  # Remove 𝕩

These functions appear in APL as Intersect (`∩`) and Without (`~`). Really, only `𝕩` is treated like a set, while the ordering and multiplicity of elements of `𝕨` are maintained. I think the explicit implementations show this well, since `𝕩` is only used as the right argument to `∊`, and prefer this clarity to the brevity of a single symbol.

# Mark Firsts

Mark Firsts (`∊`) is the simplest numeric self-search function: it returns `0` for any major cell of the argument that is a duplicate of an earlier cell and `1` for a major cell that's the first with its value. To implement [Deduplicate](#deduplicate) in terms of Mark Firsts, just [filter](replicate.md) out the duplicates with `∊⊸/`.

        ∊   3‿1‿4‿1‿5‿9‿2‿6‿5

        ∊⊸/ 3‿1‿4‿1‿5‿9‿2‿6‿5

Mark Firsts has other uses, of course. Instead of keeping the unique values, you might remove the first of each value with `¬∘∊⊸/`. You can use `∧´∊` to check that an array has no duplicate major cells, or `+´∊` to count the number of unique ones.

What about marking the elements that appear exactly once? There's a trick for this: find the cells that are firsts running both forwards (`∊`) and [backwards](reverse.md) (`∊⌾⌽`). Such a cell has no equal before it, nor after it, so it's unique in the entire array.

        (∊∧∊⌾⌽) "duck"‿"duck"‿"teal"‿"duck"‿"goose"

Remember that you don't have to apply the result of Mark Firsts to the same array you got it from! For example, it might be useful in a database application to find unique values in a particular column but use these to filter the entire table, or one other column.

