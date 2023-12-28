# Atop
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
      <td><code>F∘G</code></td>
      <td><code>F G 𝕩</code></td>
      <td><code>F 𝕨 G 𝕩</code></td>
      <td><code>{𝔽𝕨𝔾𝕩}</code></td>
      <td><code>F G´𝕩</code></td>
    </tr>
  </tbody>
</table>
<br/>

Of the two modifiers on this page, Atop is more common but less impactful. The composition `F∘G` is equivalent to the 2-[train](train.md) `F G` (the trains page has hints on when you'd choose one or the other). Its definition `{F𝕨G𝕩}` means that `G` is applied to one or two arguments and `F` is applied monadically to the result. It's sort of a "default way" to compose two functions. Keeps [tacit](tacit.md) programming syntax running smoothly, without making noise about it. Not like that busybody `⊸`. Some examples:

`↕∘≠` is useful with one argument: `↕≠l` is a list of indices for `l`.

`⌊∘÷` is useful with two arguments: `⌊a÷b` is the integer part when dividing `a` by `b`, often paired with the [remainder](arithmetic.md#additional-arithmetic) `b|a`.

`⊔∘⊐` is useful with one or two arguments. From right to left, we have [Classify](selfcmp.md#classify)/[Index-of](search.md#index-of) (`⊐`) to convert values to indices, and [Group Indices](group.md) to group the indices. Er, that sounds good but what it *actually* does is to group indices of Group's argument, which correspond to indices of the original `𝕩`, according to their values as returned by `⊐`. Without a left argument, this means indices of `𝕩` are grouped corresponding to `⍷𝕩`, and if `𝕨` is provided the groups correspond to `𝕨` instead.

        ⊔∘⊐ "bbeabee"

        "abcde" ⊔∘⊐ "bbeabee"

## Relation to Over
Atop and Over are 2-modifiers that extend the idea of "apply this, then that" in two different ways. They're modelled after the mathematical notation f∘g to compose two functions, and both do the same thing when there's one argument: either `F∘G x` or `F○G x` is `F G x`.

When there are two arguments, we might say Atop treats the right operand `𝔾` as primary and Over treats `𝔽` as primary—the primary operand becomes dyadic while the other is always monadic. Atop applies `𝔾` directly, making it more like mathematical composition if we suppose that `𝔾` is a function that can take a pair of arguments. Over instead makes two calls to apply `𝔾` separately to both arguments, then passes the results to `𝔽`.
