SRCS = butil.bas bu_lib.asm !boot
ASM_SRC = bu_lib.asm
SSD_OUT = butil.ssd

TITLE = Backup util

BEEBASM_DIR = build/beebasm

EXEEXT = $(if $(findstring Windows_NT,$(OS)),.exe)

BEEBASM = $(BEEBASM_DIR)/beebasm$(EXEEXT)

STRIPREM = s/:: REM .*$$// ; \
		s/ REM .*$$// ; \
		s/\\.*$$//

.DELETE_ON_ERROR:

$(SSD_OUT): $(SRCS) $(BEEBASM)
	@sed '$(STRIPREM)' < butil.bas | nl -ba -s ' ' | grep -v '^ *[0-9]* $$' > butil.bbas
	$(BEEBASM) -i $(ASM_SRC) -do $@ -opt 3 -title "$(TITLE)"
	@rm -f butil.bbas

beebasm: $(BEEBASM)

$(BEEBASM):
	git submodule update --init
	make -C $(BEEBASM_DIR)/src code

clean:
	rm -f $(SSD_OUT) butil.bbas
