default:
  @just --list

build:
  ghc -O2 -o app -outputdir build src/Main.hs

dev:
  pnpx nodemon --exec 'ghc -o app -outputdir build src/Main.hs && app.exe' --ext .hs
