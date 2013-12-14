ghc-edit
========

Python wrapper around ghc-mod, haskell-src-exts and other Haskell libraries to provide code completion and syntax highlighting in VIM.

Aim
---------
I am aiming to create a Python library that makes use of Haskell libraries like ghc-mod and haskell-src-exts to provide code completion
and syntax highlighting for myself, the way I would like it, when I use VIM to do Haskell hacking.

I am wrapping it all in Python so that it is easily accessible in VIM and so it might be used by other editors. 

Why ghc-edit and not haskell-edit
---------------------------------------
I use GHC as my Haskell compiler and like a lot of the extensions so I want to support a lot of GHC specific functionality and hence it
is an editor plug-in helper for GHC and not more generally Haskell.

It might never actually come to be
---------------------------------------
I am doing this as a holiday project for myself, and if I haven't achieved anything by the end of the holiday I'll just delete the
repository instead of leaving only a README behind.
