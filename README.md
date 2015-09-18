# ten-twenty-five

An implementation of the [1025](http://www.discgolfbasketstore.com/1025-discgolfgame/) disc golf putting game using the [reflex-dom](https://hackage.haskell.org/package/reflex-dom) Haskell library.

## Building
Follow the instructions at [try-reflex](https://github.com/ryantrinkle/try-reflex) to get an environment that is capable of building this project

Then run
```
ghcjs --make Main.hs Router.hs Storage.hs JSBits.hs Game.hs
```

## Running
ghcjs will generate an index.html page in the Main.jsexe directory than can be run in the browser.
