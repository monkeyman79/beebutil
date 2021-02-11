SRCS = butil.bas bu_lib.asm
ASM_SRC = bu_lib.asm
SSD_OUT = butil.ssd

TITLE = Backup util
BOOT = butil.bas

BEEBASM_DIR = build/beebasm

EXEEXT = $(if $(findstring Windows_NT,$(OS)),.exe)

BEEBASM = $(BEEBASM_DIR)/beebasm$(EXEEXT)

$(SSD_OUT): $(SRCS) $(BEEBASM)
	$(BEEBASM) -i $(ASM_SRC) -do $@ -opt 3 -title "$(TITLE)"

beebasm: $(BEEBASM)

$(BEEBASM):
	git submodule update --init
	make -C $(BEEBASM_DIR)/src code

clean:
	rm -f $(SSD_OUT)
