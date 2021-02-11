# beebutils
Utilities for BBC Micro (Model B)

## butil.bas
BBC Basic program from copying floppies en masse. Proper description will come later.

## Building

To build 'butil.ssd' floppy image on a Linux machine, execute the following commands:

```shell-session
~/src$ git clone https://github.com/monkeyman79/beebutils.git
~/src$ cd beebutils
~/src/beebutils$ make
```

This will build prerequisite 'beebasm' in a build/beebasm subdirectory, and
produce new floppy image 'butil.ssd' in current directory.

Build on mingw was not tested yet.

### Next steps

* Try to add possibility to copy floppies to MMC
