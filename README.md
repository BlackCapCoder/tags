# Tags

This is a small toy language that I think is interesting.


Let's just start off with example some code!


~~~

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

{
  -- woah, look at me
  -- I'm in a block now

  | printing in a block, my oh my |
  | printing twice in fact, we're both in the block |

  -- oh no, looks like the block is about to end
}
-- oh no, it's over now, time to go home


-- A master class in language design if I may say so
-- myself, truely innovative!

-- Wait, what are those?
-- Those things down there .. in the block

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

-- yikes! binders in blocks!
-- Is this just javascript objects all over again?

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


-- Blocks can be nested of cource, and things
-- probably works pretty much how you imagine it.

-- The langauge does not have control flow,
-- or loops (not intensional ones, anyways >.<)
-- or anything like that- it is not trying to be
-- a programming language.

-- This is an experient with first class modules,
-- and it's designed so you can drop your own language
-- into it to add all the scope/module stuff. Neat!


-----------------

-- Well then, why is the project names "tags"?

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

[This, works, too, you, need, individual, blocks] {}


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


-- One major difference from #define directives, is that we have scope.
-- The tags you enable are local to the current scope, and also
-- they are in fact attached to to the code where you put them.
-- You can move a module around, extend it and change it, but the tags
-- will stay firmly in place where you put them.


-- I want to do much more with tags, I just haven't done so yet.
-- Here are some ideas! :

-- You could select things to import from a module based on the
-- on the tags on the stuff inside, something like `import Foo [DEBUG]`


-- TODO: I've been writing this README for a while now!

