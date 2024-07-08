
In construction.


# Compiling

The prefered source is using `esy`, which can deal with all OCaml dependencies.
The native graphical output depends on the system package libsdl2-dev.

## Using `esy`

```bash
apt install libsdl2-dev
# Installing esy
npm install esy
# Installing the dependencies and compiling
esy
```

The compiled program will then be located in `_build/install/default/bin/potion-pandora`.
It can be called as-is, or alternatively as:
```bash
esy run
```

Alternatively, to run the program on a webbrowser, type:
```bash
# Run a local server on port 8000.
esy server > /dev/null &
# Then open the browser.
firefox localhost:8000 &
```


# Licence

This work in under the Creative Commons Attribution (CC-BY) licence.
See [LICENSE](./LICENSE) for more information.
Copyright (C) 2024 Martin Bodin

