# Tags

This is a small toy language that I think is interesting.


Let's just start off with example some code! Please exclude the formatting..


```lua

-- Line comment

| Bars are used for string literals, which will print
  the string literally when evaluated.

  Strings are not actually part of the language, nor is
  printing, but the language is inteded to be extended.
|

-- Oh yeah, Hello world I guess!

    | Hello, World! |


-- This is a variable declaration

newline: |
|

-- Which you can use like this:

| line 1 | newline
| line 2 | newline
| line 3 | newline

-- Variable declarations have a colon, and variable
-- reference does not. Easy peasy, nobody will find the confusing.

-- You can stick you code into blocks like this, if you'd like!
```
```lua
{
  -- woah, look at me
  -- I'm in a block now

  | printing in a block, my oh my |
  | printing twice in fact, we're both in the block |

  -- oh no, looks like the block is about to end
}
-- oh no, it's over now, time to go home
````


A master class in language design if I may say so myself, truely innovative!

Wait, what are those?
Those things ... down there .. in the block!

```
foo:
{
  we:      | 1  |
  are:     | 2  |
  the:     | 3  |
  binders: | 4  |
  that:    | 5  |
  live:    | 6  |
  in:      | 7  |
  a:       | 8  |
  block:   | 9  |

  fear:    | 10 |
  us:      | 11 |
}
```

yikes! binders in blocks!
Is this just javascript objects all over again?

```lua
bar:
{
  -- It is in fact NOT javascript objects.
  -- Variables are not so much variables in the traditional sense,
  -- but are more like modules!

  foo  -- import the contents of foo into the current scope

  we are the binders that like in a block
  fear us

  -- oh no, they are back.
}
```


Blocks can be nested of cource, and things
probably works pretty much how you imagine it.

The langauge does not have control flow,
or loops (not intensional ones, anyways >.<)
or anything like that- it is not trying to be
a programming language.

This is an experient with first class modules,
and it's designed so you can drop your own language
into it to add all the scope/module stuff. Neat!

Well then, why is the project names "tags"?

```lua


[we]
[are]
[the]
[tags]
[that]
[live]
[on]
[a]
[scope]
    scope -- kill me
    {}
    {}


-- Forget that you saw the `scope` keyword for now, I just had to do it for the joke.
-- You can attach tags to literally anything, like the boring blocks we saw before:


[lorem]
  {
  | Lorem, i say!
  |
  }

[This, works, too, you, don_t, need, individual, blocks] {}


-- Tags, for now, works pretty much exactly like #define and #ifdef from C.

set [lorem]

-- You can turn a tag ON or OFF, and it will enable or disable
-- things that does/does not have that the tag.

-- That block up there with the [lorem] tag was skipped, and even though
-- we've turned it on now it is too late.

unset [lorem]


-- like tags you can group your `set`'s if want like

set [foo, bar, baz]


-- and `unset` is in fact just sugar for negative
-- tags, which works like #ifndef

set [!foo, bar, !baz, qux]

```

One major difference from `#define` directives, is that we have scope.
The tags you enable are local to the current scope, and also
they are in fact attached to to the code where you put them.
You can move a module around, extend it and change it, but the tags
will stay firmly in place where you put them./


I want to do much more with tags, I just haven't done so yet.
Here are some ideas! :

You could select things to import from a module based on the
on the tags on the stuff inside, something like `import Foo [DEBUG]`


TODO: I've been writing this README for a while now!

I still haven't explained `scope` and it's not at all intuitive, but:

-  `{ }` blocks does not introduce a new scope
-  You need to use `scope` explicitly to get scoping at all. If you don't you are effectively always in the global scope
- `scope` is strictly more powerfull than a standard block scope

`scope` takes two "blocks" as arguments. if you only ever use the first one, it is equivallent to a standard block scope.
`Lang.hs` has pretty good comments on this!
I am not at all commited to `scope` or anything- this is just a toy project- I am trying wierd things!


In fact, I am just going to inline the relevant part from `Lang.hs`, because I want to talk about it anyaways.
There are literaally 5 constructors. `Hole` doesn't do anything, `Block` can be fully replaced by `Scope`.
What are we left with? `Var`, `Abs`, `App` (named scope)- it's literally just lambda calculus with a different flavour.
but it sure doesn't feel that way!

You can't escape! Is it even possible to write programming language with binders that doesn't just boil down to
some flavour of LC? This language is not supposed to be turing-complete. so hopefully `Scope` does not turn out
to be App


```haskell
-- A binder
--
data B x
   = V x      -- Variable
   | T x Bool -- Tag
   deriving
     ( Show, Functor
     )

data Decl x a

   -- An arbitrary hole that we don't care about.
   -- It is left as-is in the syntax tree, so you can
   -- inject another language into this one.
   --
   = Hole a

   -- V binders are effectively module imports.
   -- T binders set and/or clear tags
   | Var (B x)
   --
   -- Tags look like this:
   --   [foo] [bar] [baz] { code code code }
   --
   -- They are alluxary extra binders you can attach to code- any code!
   -- The names have no inherent meaning. Tags are local to the scope,
   -- and each tag is attached to a specific piece of code.
   -- 
   -- At the moment tags can only be used to enable/disable code
   -- siminar to #define, but I want to find more ways to use tags.
   --
   -- Like using them to select what to import from a module!
   -- I've had that and a few other ideas in the back of my head since the
   -- start, but I am not sure if they CANT do that already!


   -- V binders bind the scope of the RHS to a name in the current scope
   -- T binders will skip the RHS if the tag does not match
   | Abs (B x) (Decl x a)

   -- Block does not introduce a new scope, it only
   -- groups declarations together
   | Block [Decl x a]

   -- Changes to the scope made on the LHS are cleared after the RHS.
   -- That is, the RHS recieves the scope from of the the LHS, makes
   -- its own changes, then, every change from the LHS that has not
   -- not been overwritten by the RHS are discarded.
   --
   -- If we only ever use the LHS, scope behaves like a traditional
   -- block scope. If we only ever use the RHS, it behaves as-if we only
   -- have a single global scope.
   --
   | Scope (Decl x a) (Decl x a)
```
