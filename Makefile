.INTERMEDIATE: BUTIL_S.hh !BOOT !BOOT2

PRG = BUTIL

new.ssd: !BOOT !BOOT2 $(PRG)_S
	cp ./build/empty.ssd new.ssd
	./build/BBCIM.EXE -ab new.ssd !BOOT !BOOT2 $(PRG)_S >/dev/null 2>&1
	./build/BBCIM.EXE -boot new.ssd 3 >/dev/null 2>&1

!BOOT:
	echo '*EXEC $(PRG)_S' | tr '\n' '\r' > $@

!BOOT2:
	echo 'CH."$(PRG)"' | tr '\n' '\r' > $@

$(PRG)_S: $(PRG)_S.hh
	nl -ba < $< | tr '\n' '\r' > $@
	( echo 'SAVE "$(PRG)"'; \
	echo '*TITLE "Backup util"'; \
	echo 'NEW'; \
	echo '10 CLOSE#0'; \
	echo '20 OSCLI("DELETE $(PRG)_S")'; \
	echo '30 OSCLI("DELETE !BOOT")'; \
	echo '40 OSCLI("RENAME !BOOT2 !BOOT")'; \
	echo '50 OSCLI("FX255, 247")'; \
	echo '60 CALL !-4'; \
	echo 'RUN'; ) | tr '\n' '\r' >>$@

$(PRG)_S.hh: $(PRG).bas Makefile
	./build/striprem.sh $< $@

clean:
	rm -f $(PRG)_S.hh $(PRG)_S !BOOT !BOOT2 new.ssd
