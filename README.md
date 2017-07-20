# reflex-animated

AnimationFrame and Pointer events using [reflex framework](https://github.com/reflex-frp/reflex).

The library provides a simple and fast interface for rendering animated objects and interacting with a user.
It combines JS mouse, touch, and pointer events in a unified Haskell interface, which simplifies desktop-mobile compatibility.
Lastly, it encapsulates requestAnimationFrame mechanics into simple play-pause interface, allowing to peform rendering either via a direct rendering callback or using dedicated events.

### Using with stack

This repository includes stack configuration for a manually crafted GHCJS snapshot based on `lts-8.21`, it comes with no warranty.
Make sure you have installed `alex`, `happy`, and [any other programs required by GHCJS](https://github.com/ghcjs/ghcjs/tree/ghc-8.0#installation).
If this is done well, `stack build --install-ghc` should work.
