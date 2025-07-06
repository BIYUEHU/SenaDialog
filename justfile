default:
  @just --list

dev:
  pnpx nodemon --exec 'ghc -o app -outputdir build src/Main.hs && app.exe' --ext .hs

build:
  ghc -O2 -o app -outputdir build src/Main.hs
