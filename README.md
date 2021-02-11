# beebutils
Utilities for BBC Micro (Model B)

## butil.bas
BBC Basic program from copying floppies en masse. Proper description will come later.

## Building

### Linux

```shell-session
~/src$ git clone https://github.com/monkeyman79/beebutils.git
~/src$ cd beebutils
~/src/beebutils$ make
```

### Windows
```
...\src> git clone https://github.com/monkeyman79/beebutils.git
...\src> cd beebutils
...\src\beebutils> make.cmd
````

This will build prerequisite 'beebasm' in a build/beebasm subdirectory, and
produce a new bootable floppy image `butil.ssd` in current directory.

Build on mingw was not tested.

### Next steps

* Try to add possibility to copy floppies to MMC
