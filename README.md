# effect-language


* explicit effect turns off orphan rules, global effects not.
* effect aliases make syntax nicer
* effects replace traits
* effects:
   * convert
   * allocator
   * async
   * const not, it's the lack of capabilities of a function
   * io (bundle of other effects)

* some effects can be "main effects" with which main is executed like the allocator main effect.
* Some effects can be implicit but which handler runs must never be a surprise
* Range of power: trait to async
* Tail resumptive --> Trait, effects can be non-tail resumptive
* Move types


```rust
effect Convertable<A, B> {
    fn convert(A) -> B;
}

struct X;
struct Y;

impl Convertable<X, Y> {
    fn convert(self: X) -> Y {
        Y
    }
}


fn convert() {

}

fn main() {
    let a = X;
    let b = a.convert();
}

```

weaker effect handlers with still async support: https://www.microsoft.com/en-us/research/publication/effect-handlers-evidently/

