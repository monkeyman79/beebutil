SRCS = butil.bas bu_lib.asm
BOOT = butil.bas
TITLE = Backup util

EXECS = $(patsubst %.asm,%.exec,$(SRCS:%.bas=%.exec))

NL2CR = tr -d '\r' | tr '\n' '\r'
STRIPREM = s/:: REM .*$$// ; \
		s/ REM .*$$// ; \
		s/\\.*$$//

.INTERMEDIATE: !BOOT !BOOT2 $(EXECS)

toupper = $(shell echo $(1) | LC_ALL=C tr '[:lower:]' '[:upper:]')
tolower = $(shell echo $(1) | LC_ALL=C tr '[:upper:]' '[:lower:]')

new.ssd: !BOOT !BOOT2
	cp ./build/empty.ssd new.ssd
	./build/BBCIM.EXE -ab new.ssd !BOOT !BOOT2 >/dev/null 2>&1
	./build/BBCIM.EXE -boot new.ssd 3 >/dev/null 2>&1

!BOOT: $(EXECS)
	( echo '*TITLE "Backup util"'; \
	cat $^; \
	echo 'NEW'; \
	echo '10 CLOSE#0'; \
	echo '20 OSCLI("DELETE !BOOT")'; \
	echo '30 OSCLI("RENAME !BOOT2 !BOOT")'; \
	echo '40 OSCLI("FX255, 247")'; \
	echo '60 CALL !-4'; \
	echo 'RUN'; ) | $(NL2CR) > $@

!BOOT2:
	echo 'CH."$(call toupper,$(BOOT:%.bas=%))"' | $(NL2CR) > $@

%.exec: %.bas Makefile
	( echo 'NEW' ; \
	sed '$(STRIPREM)' < $< | \
	nl -ba; \
	echo 'SAVE "$(call toupper,$*)"'; \
	) > $@

%.exec: %.asm Makefile
	( echo 'NEW' ; \
	sed '$(STRIPREM)' < $< | \
	nl -ba; \
	echo 'RUN'; \
	) > $@

clean:
	rm -f !BOOT !BOOT2 $(EXECS) new.ssd
