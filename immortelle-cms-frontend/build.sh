../reflex-platform/work-on ./overrides.nix ./. --run "cabal configure --ghcjs && cabal build"
cp dist/build/immortelle-cms-frontend/immortelle-cms-frontend.jsexe/all.js ../immortelle-cms-server/static/all.js
