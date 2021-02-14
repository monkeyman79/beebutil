# beebutils
Utilities for BBC Micro (Model B)

## butil.bas
BBC Basic program from copying floppies en masse. It basically duplicates functionality of ``*BACKUP`` command, but does it in style.

Features:
* Display progress graphically
* Retry or skip bad sectors
* Display list of bad sectors and files to which those sectors belong
* Disable selected ROMs to allow copying DFS disks while MMC ROM is present in the system and vice-versa. Unfortunatelly it
  doesn't allow copying data between physical drive and MMC.
* Copying exotic FLEX OS disks

### main menu
<img src="doc/screen1.png" alt="main menu" width="640"/>

* ``S`` - Select source drive number. Displayed info is: number of tracks, number of files in catalog and disk title
  or error encountered number during catalog sectors read.
* ``D`` - Select destination drive number.
* ``T`` - Track mode - read a track at a time and if that fails, try to read each sector on track separately. There is
  no reason to turn it off.
* ``O`` - On error - action taken when bad sector is encoutered. Cycles between ``ask``, ``continue`` and ``fail``.
* ``M`` - Max tries - max. number of times each disk operation is attempted before giving up.
* ``P`` - Passes - max. number of retries for copy. Effective number of attempted reads or writes is ``passes * max. tries``.
* ``A`` - Auto verify - verify data during copy operation.
* ``F`` - Flex mode - for copying Flex OS disks.
* ``C`` - Copy disk
* ``V`` - Verify - read and compare source and destination track by track.
* ``R`` - Read disk - read source disk and check for bad sectors.
* ``U`` - Update info - read catalog sectors and update data displayed and ``'(S)ource'`` and ``(D)estination`` lines.
* ``B`` - Bad sectors and files - display list of bad sectors found during last operation.
* ``.`` - Catalog - display disk catalog
* ``*`` - OSCLI - execute arbitrary OSCLI command. Be carefule not to use a commands which overwrites BASIC program memory,
  such as ``*BACKUP``
* ``%`` - Toggle selected ROMs off and on. Useful when you have both (A)DFS and MMC ROMs installed.
* ``E`` - Exit program

### toggle ROMs
<img src="doc/screen2.png" alt="toggle ROMs" width="640"/>

Displays list of installed ROMs. Select ROM by hex number to toggle it on to off. ROMs are disabled by clearing memory location
``&2A1+rom``, this prevents MOS from dispatching ``OSWORD`` call ``&7F`` (low-level disk access) to those disabled ROMs. Initial
values are automatically restored when program exits.

In the screenshot above, MMC ROM has been disabled to allow copying DFS disks.

### disk operation screen
<img src="doc/screen3.png" alt="disk operation screen" width="640"/>

Each character represents single sector, horizontal strip of ten characters represents one track. Tracks are layed out from top-left,
down the screen and the to the right, in four columns. Meaning of the characters is:

* ``.`` - sector not read yet
* ``r`` - reading
* ``w`` - writing
* ``v`` - verifying
* ``?`` - program couldn't copy entire track at once, now copying sector by sector

To abort the operation, press ESCAPE key. You may have to try several times, because ESCAPE is active only during disk access.

### list of bad sectors and files
<img src="doc/screen5.png" alt="bad sectors" width="640"/>

Displays list of bad sectors, followed by list of files to which those sectors belong. Format of record on bad sectors list is track /
sector + number of consecutive bad sectors.

## building

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
