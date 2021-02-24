# Santorini

## Basic Player

There are two executables within the basic player repository.

1. The `player` executable. Main is defined in `playerapp/PlayerMain.hs` and the 
library is `src/Player.hs`.

2. The `driver` executable. Main is defined in `driverapp/DriverMain.hs` and all code is contained
to this file. :warning: The driver program is largely unfinished. Currently, it does not check boards
returned from the players nor does it check for a win. :)

Other important libraries include:

* `src/SantoriniDefs.hs`

* `src/SantoriniUtils.hs`

### Building Basic Player

`stack build`

The executables are found in 
`./.stack-work/install/x86_64-osx/202c490e26cd90901fb3c6e9985a679cadc0e3588f4baafa7a56b5c0320dbd69/8.10.3/bin`

