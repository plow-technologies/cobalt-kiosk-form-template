# cobalt-kiosk-form-template 
[![Circle CI](https://circleci.com/gh/plow-technologies/cobalt-kiosk-form-template.svg?style=svg)](https://circleci.com/gh/plow-technologies/cobalt-kiosk-form-template)
TODO: The basis for rendering kiosk forms to IPAD

## Add forms to Cobal Kiosk Backend

cabal repl
:l Kiosk.Backend.Generator.RockShore
import Data.Traversable
traverse (insertThisFormInRockShore "127.0.0.1" "2833") currentForms


## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

TODO: Write contribution instructions here

