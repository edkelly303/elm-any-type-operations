# Any-type operations

## What?

`Order` allows you to define comparison functions quickly for your custom types.
Look at the `Order` module first to see how this works.

`Enum` builds on `Order` with some additional useful functions for custom types 
that have a finite number of variants.

`Arithmetic` lets you define arithmetic operations for your custom types. (I'm 
less convinced that this one is actually useful).

## Why?

Sometimes you define a custom type, and you really want it to be `comparable` so 
that you can use comparison operators like `<` or `>=`. But it isn't. So you 
can't. 

Or you want to perform some kind of mathematical operations on them using 
arithmetic operators like `+` or `*`. But they aren't `number`s. So you can't.

This package provides a cheap way to define a bunch of functions that allow you 
to treat custom types _almost_ like `comparable` or `number` types.

## Are these like... _typeclasses_?

I don't think so ¯\\\_(ツ)\_/¯ 

Hopefully, they are still useful though.
