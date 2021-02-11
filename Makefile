SRCS = butil.bas bu_lib.asm
ASM_SRC = bu_lib.asm
SSD_OUT = butil.ssd

TITLE = Backup util
BOOT = butil.bas

EXEEXT = $(if $(findstring Windows_NT,$(OS)),.exe)

BEEBASM = build/beebasm/beebasm$(EXEEXT)

$(SSD_OUT): $(SRCS) $(BEEBASM)
	$(BEEBASM) -i $(ASM_SRC) -do $@ -opt 3 -title "$(TITLE)"

beebasm: $(BEEBASM)

$(BEEBASM):
	make -C build/beebasm code

clean:
	rm -f $(SSD_OUT)
