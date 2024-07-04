
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

Then, to access it on a webbrowser, type:
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

