# Exploration

TL;DR of interesting features:
- Distinction between `ctl`, `fun` & `val` operations (Koka) 
[jonathan says: we should encode these as associated functions/consts; unrelated: does this mean we need to solve the trait-with-fields problem?]
- Named and scoped handlers (Koka)
- Effect row aliases (Koka)
- `with` keyword (Koka)
- `return` branch is optional (Koka)
- inline handlers (Koka)
- Divergence as an effect (Koka)
- Ambient effect rows (Frank)
- Contextual effect rows (Effekt), like ambient, but does not rely on syntactic sugar
- Forcing computations with `!` (Frank, Unison)
- Requiring the use of `!` (Tao)
- Effect labelling (Idris)
  
Open questions:
- Best effect row semantics?
- Automatically lift effects (like Koka) or explicit notation like Tau? (Terts says: LSP could help here.)
- Should handlers be separate from functions?
- Can we always "emulate" higher-order effects?
- What is a good way to label effect (with dependent types)?

## Koka

Ref: @@koka, [website](https://koka-lang.github.io/koka/doc/book.html)

```koka
effect yield
  ctl yield( i : int ) : bool

handler
  ctl yield(i)
    ...
```

### Effect Rows

Basic syntax:
```
<A|<B|...>>
```

Sugar:
```
<A,B,...>
```

Extension:
```
<A|e>
```

Brackets are not necessary if you refer to a single effect, alias or polymorphic row:
```
console
total
e
```

Aliases:
```
alias pure = <div,exn>
```

### With

```koka
fun foo()
  with some_handler
  ...
```

This is just syntactic sugar for:

```koka
fun foo()
  some_handler {
    ...
  }
```

## Inline handlers

Very concise syntax for creating handlers with a single effect operation:

```
fun foo()
  with ctl ask() 42
  ask()
```

Although that should of course be written as

```
fun foo()
  with val ask = 42
  ask
```

### Initially & Finally

Ref: https://koka-lang.github.io/koka/doc/book.html#sec-resource

Very useful for dealing with resources. It's ensuring that resources are dropped.

### Named and Scoped Handlers

Ref: @@koka-named-handlers

This is introduced specifically for an allocation effect (which they call `Heap`).

Scoped handlers associate a type parameter with each handler _instantiation_. So that, for example, references cannot be used on a different handler. This is similar to, but also different from, lifetimes. 
See lifetimes as tokens: 
Ref: @@beingessner2016

### Linear effects

From the docs:

> Use linear effect to declare effects whose operations are always tail-resumptive and use only linear effects themselves (and thus resume exactly once). This removes monadic translation for such effects and can make code that uses only linear effects more compact and efficient.

### Functions pretend to be language constructs

Koka's flexible syntax allows functions to be used in ways that are very similar to built-in language constructs in some languages.

```koka
while { ... }
  ...
```

```koka
list(1,10).foreach
  ...
```

### Mutability

Mutable variables desugar to the state effect: https://koka-lang.github.io/koka/doc/book.html#sec-var


## Frank

Ref: @@frank, [GitHub](https://github.com/frank-lang/frank)

In Frank, the empty effect row is not the empty effect row. Instead, it is the _ambient_ effect row. This is convenient:

```frank
map : {{X -> Y} -> List X -> List Y}
-- is actually:
map : {{X -> [E|]Y} -> List X -> [E|]List Y}
```

which is the same as Koka's:

```koka
fun map(f: X -> e Y, l: List[X]) e Y
```
, the empty effec
But in Frank, we have to think less about the effect rows.

Here is the abort effect ([src](https://github.com/frank-lang/frank/blob/master/examples/exception.fk)):

```frank
interface Abort = aborting : Zero

abort : {[Abort]X}
abort! = case aborting! { }
  -- Zero is uninhabited, so no clauses are needed

catch : {<Abort>X -> {X} -> X}
catch x               h = x
catch <aborting -> k> h = h!
```

`{X}` means "a computation returning `X`"

They seem to support scoped effects: <https://github.com/frank-lang/frank/blob/master/examples/scoped/catch.fk>

## Effekt

Ref: @@effekt, [website](https://effekt-lang.org/)

Effekt treats effect row polymorphism not as syntactic sugar. But applies a different type system, which is very interesting. From the paper:

> Effect types express which capabilities a computation requires from its context.

It sounds obvious, but it's different from Koka (even though it often looks the same in Koka because of syntactic sugar).

```effekt
record Solution(first: Int, second: Int, third: Int)

effect Flip(): Boolean
effect Fail[A](): A

def choice(n : Int): Int / { Flip, Fail } =
  if (n < 1) { do Fail() }
  else if (do Flip()) { n }
  else { choice(n - 1) }

def handledTriple(n: Int, s: Int): List[Solution] / {} =
  try {
    try {
      [ triple(n, s) ]
    } with Fail[A] { () => [] }
  } with Flip { () => resume(true).append(resume(false)) }
```

### Bidirectional effects

Effects can use other effects, or at least, _inject_ effect operations: https://effekt-lang.org/docs/concepts/bidirectional

I'm not sure whether this should be a property of effects or handlers. Maybe both? Handlers would make more sense to me, since `Get` does not fail in some handlers, but might need `Exc` for other handlers.

## OCaml

Ref: @@ocaml, [website](https://v2.ocaml.org/manual/effects.html)

`Effect.t` is a type, which is interesting. We can extend it with effects. That seems strange, because then the effect we add is never removed again. I'm not entirely sure how that works. Maybe it's a result of trying to retrofit effects into OCaml.

```ocaml
open Effect
open Effect.Deep

type _ Effect.t += Xchg: int -> int t
let comp1 () = perform (Xchg 0) + perform (Xchg 1)

try_with comp1 ()
{ effc = fun (type a) (eff: a t) ->
    match eff with
    | Xchg n -> Some (fun (k: (a, _) continuation) ->
        continue k (n+1))
    | _ -> None }
```

## Unison

Ref: [website](https://www.unison-lang.org/learn/fundamentals/abilities/)

Unison has delayed computations, like Frank. They denote them with a `'` prefix, both for types and expressions.

It's syntax, is, ehh, a lot:
```unison
fundamentals.abilities.writingAbilities.inMemory :
  '{g, KVStore a b} r ->{g} r
fundamentals.abilities.writingAbilities.inMemory keyValueInteractions =
  impl : data.Map a b -> abilities.Request {KVStore a b} r -> r
  impl map = cases
    { pure } -> pure
    { KVStore.get key -> resume } ->
      handle resume (Map.get key map) with impl map
    { KVStore.put key value -> resume } ->
      updatedMap = Map.insert key value map
      handle !resume with impl updatedMap
  handle !keyValueInteractions with impl data.Map.empty
```

## Tao

Ref: [GitHub](https://github.com/zesterer/tao)

Effectful functions require an "effect propagation" operator `!` to be used. This is similar to the `!` from Unison or Frank, which force a delayed computation. Koka ignores this.

## Idris

Ref: @@idris

I love the idea of labelled effects:

```idris
(:::) : lbl -> EFFECT -> EFFECT
(:-) : (l : lbl) -> EffM m [x] [y] t -> EffM m [l ::: x] [l ::: y] t

tagCount : Tree a -> Eff m [Tag ::: STATE Int,
                            Count ::: STATE Int]
                              (Tree (Int, a))
                              
tagCount Leaf
  = do Count :- update (+1)
       return Leaf
tagCount (Node l x r)
  = do l’ <- tagCount l
       lbl <- Tag :- get
       Tag :- put (lbl + 1)
       r’ <- tagCount r
       return (Node l’ (lbl, x) r’)
```

## Elaine

Ref: [GitHub](https://github.com/tertsdiepraam/elaine)

In Elaine, handlers are separate types from functions. This also introduces the `handle` construct. A nice consequence is that many handlers can be put on a single line:

```elaine
let main = handle[hState] handle[hWrite] handle[hAbort] { ... };
```

## Nim

So Nim does not have algebraic effects. However, it does have something cool: pure functions.

```nim
# foo must be pure!
func foo():
  ...

# bar can have any effect!
proc bar():
  ...
```

Different keywords for different default sets of effects is certainly interesting.