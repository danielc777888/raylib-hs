# raylib-hs
- haskell bindings to raylib 4.0.0.0 game library
- api attempts to stay faithful to the cheatsheet : https://www.raylib.com/cheatsheet/cheatsheet.html

## TODO
- use data records for structs, and make fields strict
- test all examples without manually running them one by one
- somehow not having to convert from T.Text to String when calling c lib
- best practices as per ghc dev blog : https://www.haskell.org/ghc/blog/20210709-capi-usage.html
- pass by value without a wrapper if possible : https://stackoverflow.com/questions/10903940/haskell-ffi-how-to-handle-c-functions-that-accept-or-return-structs-instead-of
- use defined values without recreating in haskell/ eg. colors
- use rayib parser to auto generate binding code
