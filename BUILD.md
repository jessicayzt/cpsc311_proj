# Building the game (Elm 0.18)

This project predates Elm 0.19 and depends on `evancz/elm-graphics`, which was never ported to 0.19,
so it has to be built with the Elm 0.18 toolchain. The 0.18 package server is gone, so the
dependencies are fetched straight from GitHub and placed in `elm-stuff/` by hand. The compiler
itself never needs network access after that.

The compiled game is committed as `play/elm.js`; `play/index.html` loads it. GitHub Pages serves
the repository root, so the game's `../graphic/...` image paths resolve to the repo's `graphic/` folder.

## 1. Get the Elm 0.18 compiler

Binaries are still attached to the `0.18.0-exp` release of `elm-lang/elm-platform`:
<https://github.com/elm-lang/elm-platform/releases/tag/0.18.0-exp>

```sh
mkdir -p ~/elm-0.18 && cd ~/elm-0.18
gh release download 0.18.0-exp -R elm-lang/elm-platform -p elm-platform-macos.tar.gz   # or elm-platform-linux-64bit.tar.gz
tar xzf elm-platform-macos.tar.gz
# macOS only: the binaries are unsigned x86_64 (they run under Rosetta on Apple Silicon).
# Gatekeeper kills quarantined unsigned binaries, so clear the quarantine flag first.
xattr -d com.apple.quarantine elm elm-make elm-package elm-reactor elm-repl
./elm-make --help
```

## 2. Vendor the dependencies (from the repo root)

```sh
mkdir -p elm-stuff/packages
while read -r user repo ver; do
  dest="elm-stuff/packages/$user/$repo/$ver"
  git clone -q --depth 1 --branch "$ver" "https://github.com/$user/$repo" "$dest" && rm -rf "$dest/.git"
done <<'DEPS'
elm-lang core 5.1.1
elm-lang html 2.0.0
elm-lang virtual-dom 2.0.4
elm-lang animation-frame 1.0.1
elm-lang keyboard 1.0.1
elm-lang window 1.0.1
elm-lang dom 1.1.1
evancz elm-graphics 1.0.1
elm-community list-extra 6.1.0
DEPS

cat > elm-stuff/exact-dependencies.json <<'JSON'
{
    "elm-community/list-extra": "6.1.0",
    "elm-lang/animation-frame": "1.0.1",
    "elm-lang/core": "5.1.1",
    "elm-lang/dom": "1.1.1",
    "elm-lang/html": "2.0.0",
    "elm-lang/keyboard": "1.0.1",
    "elm-lang/virtual-dom": "2.0.4",
    "elm-lang/window": "1.0.1",
    "evancz/elm-graphics": "1.0.1"
}
JSON
```

## 3. Compile

```sh
~/elm-0.18/elm-make Main.elm --yes --output play/elm.js
```

Open `play/index.html` locally to test, then commit `play/elm.js`.
