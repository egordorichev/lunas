# Lunas

Lunas is a small compiler, that compiles Lua code into pure JS.

This was a tiny side project I’ve been working for a few days, I had a sudden idea of writing a Lua into JavaScript compiler, and it ended up even working. I even implemented metatables, and two big thing the current implementation is missing are vargs and `for k, v in pairs(array) do` loop. But as the project grew, I started to fear the performance issues, because since JS doesn’t support operator overloading, I had to do things like replacing every operator with a function call, that checked if the value is an object with a metable and an `__add` function, and otherwise called the normal operator.

So yeah, it was a cool little experiment, but I don’t think I will ever get back to it.
