# assign2-cad0p


To setup:
```sh
cd assign2-cad0p
stack setup
```

To run:
```sh
stack build
stack ghci
stack test
stack haddock
```

To debug:
```sh
stack ghci assign2-cad0p:assign2-cad0p-test
```

And if you want `:r` to work: [(source)](https://stackoverflow.com/questions/39938101/how-to-load-tests-in-ghci-with-stack)
```sh
stack ghci --ghci-options -isrc --ghci-options -itest assign2-cad0p:assign2-cad0p-test
```


