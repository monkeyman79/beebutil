# beebutils
Utilities for BBC Micro (Model B)

## butil.bas
BBC Basic program from copying floppies en masse. Proper description and build procedure will come later.

Provided Makefile is barely functional. It won't work for you without some effort. You will need to:
- Download **BEEBIM.EXE** from the net and put it in the **build** directory. This comes in binary form, although there are sources available somewhere on the net. On the other hand SSD files and DFS floppy format in general is fairly simple. It should be easy to produce a floppy image using e.g. a python script.
- Place **empty.ssd** in the **build**. It can be bought in any shop with empty SSD files, or produced with an emulator. It is only needed because I can't get the BEEBIM to create empty floppy properly.

After **make** is done you will have **new.ssd** - a bootable floppy image. On first boot, it will tokenize the **BUTIL_S** - program in ascii form and save it as **BUTIL** in proper form. 
