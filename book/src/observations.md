# Observations & Ideas

## Effects in Rust

- `const` is the absence of effects.
- Panicking
- Allocation
- Async
- Iteration
- Unsafe
- Traits
- Mutability
- Try (Option/Result/ControlFlow)
- IO: File system, printing, etc.

## Traits are Effects

Say we have a function signature like this:

```rust
fn foo<T: Display>(x: T) -> String
```

That is _very_ similar to this:

```rust
fn foo(x: T) -> [Display<T>] String
```

Where the `[]` is the effect row. That's interesting, because it suggests that effect rows are a generalization of traits. So that means: **traits are function effects with global handlers**.

Of course, we have to ask what that gains us. Well, several things actually:

1. If we only have effects, the language is simpler.
2. We can ditch the orphan rule, or at least relax it.

That second point needs some explanation. In Rust, we cannot implement traits on items from other crates arbitrarily, because this would lead to problems where adding a crate B changes the implementation of crate A.

However, with effect handlers, we can explicitly choose which implementation we are using! So, the default implementation would still need the orphan rule, but we can opt in to using an implementation from another crate if we want to.

Overlapping implementations can be fine too.

Open questions:

- What to do about `Self`? It is not _necessary_ (Haskell doesn't have it either).
- What about trait bounds? (Maybe bi-directional effects, see Effekt)
- How do we say "this handler implements this effect for this set of types", not just for one specific type. E.g. impl `Eq` for everything that implements `PartialEq`.
- "global" and "default" handlers?
- What to do about types that rely on traits not being implemented or implemented a certain way. E.g. `Clone` for `Rc` and `File`. Although I guess there's still no way to implement those badly (without `unsafe`), because they also expose no way to implement the necessary functions, but that doesn't hold for marker traits. Let's call this the `Send`-problem.

## Allocation is an Effect

Rust has been retrofitting allocator arguments in some things. Zig has them everywhere. It's nice to be explicit about them but also to not have to think about it. What's the sweet spot? Say it with me now: effects!

As an effect, the allocation is sort of an implicit argument, but still very much part of the type system, so we can always know when a function needs to allocate.

A consequence of this is that the `core`/`alloc`/`std` divide disappears. Even if the OS does not provide an allocator, we can still handle allocation operations with a bump allocator, for example, and use a `Vec` in a `no_std` context.

Open questions:

- How to deal with multiple allocation arenas?
- Should `Allocation` take a type parameter, possibly with some `Any` type? Or should that be `Arena<T>`.
- How do we deal with freeing values? Do we need full ownership (probably yeah)?

## Effect Row Aliases

If everything becomes an effect, the function signatures become very heavy. We can partially fix this with aliases:

```rust
effect total = [];
effect pure = [div]
effect alloc = [pure,Allocation]
effect std = [alloc,FileSystem,Console]
// etc.
```

## Default Effect Rows for Modules

Something we haven't seen much is default effect rows per module:

```
effect default = [Allocation];
```

This row would be the default effect row, but can be extended or otherwise manipulated, for each function in the module.

## Effects are sets/bags

There's nothing stopping us from supporting set-like operations on effect rows:

```
[A, B] + C      == [A, B, C]
[A, B] - B      == [A]
[A, B] + [C, D] == [A, B, C, E]
[A, B] ^ [B, C] == [B]
A in [A, B, C]  == true
```

Are all of these useful? Maybe not, we'll have to see.

## Effects are the Environment/Platform

Effects could be good for multi-platform development, with separate handlers for different operating systems.

The dream: type check for all platforms at once. No conditional compilation without losing flexibility would be awesome. Not sure how that would work, but at the very least you could define cross-platform interfaces, with platform-specific handlers. However, that's not yet more flexible or powerful than traits.

## Arithmetic Behaviour could be an Effect

When we overflow, do we want to wrap, panic or saturate? We could specify this in a computation with handlers for arithmetic. Basically, local operator overloading, which sounds extremely cursed now that I've written that down.