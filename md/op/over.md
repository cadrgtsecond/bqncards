# Over

Atop and Over are 2-modifiers that extend the idea of "apply this, then that" in two different ways. They're modelled after the mathematical notation f∘g to compose two functions, and both do the same thing when there's one argument: either `F∘G x` or `F○G x` is `F G x`.

<table>
  <thead>
    <tr>
      <th><code>Cmp</code></th>
      <th><code>Cmp 𝕩</code></th>
      <th><code>𝕨 Cmp 𝕩</code></th>
      <th>Unified</th>
      <th>On list</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>F○G</code></td>
      <td><code>F G 𝕩</code></td>
      <td><code>(G 𝕨) F G 𝕩</code></td>
      <td><code>{(𝔾𝕨)𝔽𝔾𝕩}</code></td>
      <td><code>F´G¨𝕩</code></td>
    </tr>
  </tbody>
</table>

When there are two arguments, we might say Atop treats the right operand `𝔾` as primary and Over treats `𝔽` as primary—the primary operand becomes dyadic while the other is always monadic. Atop applies `𝔾` directly, making it more like mathematical composition if we suppose that `𝔾` is a function that can take a pair of arguments. Over instead makes two calls to apply `𝔾` separately to both arguments, then passes the results to `𝔽`.

Once you get used to Over, it's painful to go without it. I'd use it all the time in C if I could.

Usually Over is used just for the dyadic meaning. If you have a composition that only works with one argument it's typical to write it with Atop (`∘`). And cases that work with one or two arguments do come up from time to time, but they're fairly rare, so the examples below are just for two arguments.

A classic is the function `≡○∧`, which tests whether `𝕨` is a reordering of `𝕩`. The idea is to sort both arrays with `∧` to remove the ordering information, then see if they match.

        "BQN" ≡○∧ "QNB"
        "BQN" ≡○∧ "BBQ"

Another example is `/○⥊`, used to filter elements in a high-rank array. Alone, `/` won't do this because there's no automatic choice of ordering for the results. Applying [Deshape](reshape.md) (`⥊`) to both chooses index order.

        ⊢ a ← "qBrs"≍"QtuN"

        a < 'a'  # Capital letters

        (a<'a') / a  # Not allowed

        (a<'a') /○⥊ a

Over is closely connected with the [Under](under.md) modifier, which performs all the same steps but then undoes `𝔾` afterwards.
