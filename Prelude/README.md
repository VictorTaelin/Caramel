## Prelude

This is a sketch of a Prelude, like that of Haskell, for the pure Lambda Calculus.

There is no `import` on Caramel - the command line tool reads every file, thus
every top-level symbol is available everywhere and must have an unique name.

It is lacking in many senses. It needs some major refactorings since most of
that was coded before Caramel and could be a lot improved in aesthetics. It
needs comments.

Yet, not much should be done before having a satisfactory solution for the ADT
problem mentioned on the project's README. Having a solid foundation for
datatypes and generic derivers is essential, or else one would lose way too
much time and write way too much code dealing with church/scott encodings
manually.

