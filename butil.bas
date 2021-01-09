MODE 7
HIMEM=&7800
osargs=&FFDA
osbyte=&FFF4
PROCmcode
PROCmain
CLS
END

:: REM Prepare machine code procedures
DEF PROCmcode
MC%=&7800
DIM BF% 2560
DIM VB% 2560
DIM rwpar% 16
DIM sectn% 10
DIM inpbuf% 32
DIM bads% 100
inpp%=0
$sectn%=":0 00/00"
OSCLI "LOAD M.BU_LIB 7800"
memcmp    =MC%+3*0
memset    =MC%+3*1
hexdump   =MC%+3*2
printhnum =MC%+3*3
printhbyte=MC%+3*4
addrof    =MC%+3*5
XOSBYTE   =MC%+3*6
XOSWORD   =MC%+3*7
XOSCLI    =MC%+3*8
ENDPROC

:: REM Get current filing system number
DEF FNfs
A%=0:Y%=0:=(USR(osargs)) AND &FF

:: REM Get machine high order address
DEF FNhaddr
A%=130:=(USR(osbyte) AND &FFFF00) DIV 256

:: REM Format sector address
DEF FNsectn(D%,T%,S%,F%)
sectn%?1=48+D%
sectn%?3=48+(T% DIV 16)+(T% DIV 160)*7
sectn%?4=48+(T% MOD 16)+((T% MOD 16) DIV 10)*7
sectn%?6=48+(S% DIV 16)+(S% DIV 160)*7
sectn%?7=48+(S% MOD 16)+((S% MOD 16) DIV 10)*7
IF F% THEN A%=sectn% ELSE A%=sectn%+3
=$A%

:: REM Disk operation (read, write or verify sectors)
DEF FNdisk(drv%,trk%,sec%,cnt%,cmd%)
LOCAL S%,R%,T%,P%,V%
P%=POS:V%=VPOS
T%=0
REPEAT
rwpar%?0=drv%
rwpar%!1=BF%: IF cmd%=1 THEN rwpar%!1=VB%
rwpar%!3=haddr%
rwpar%?5=3:rwpar%?6=&53
IF cmd%=2 THEN rwpar%?6=&4B
rwpar%?7=trk%:rwpar%?8=sec%
rwpar%?9=cnt% OR &20
rwpar%?10=&FF
PRINT TAB(1, 22);
IF cmd%=0 THEN PRINT "Reading   ";
IF cmd%=2 THEN PRINT "Writing   ";
IF cmd%=1 THEN PRINT "Verifying ";
PRINT FNsectn(drv%,trk%,sec%,1);" +";FNnpad(cnt%,2);" ";
IF T%=0 THEN PRINT SPC(14);TAB(POS-10,VPOS);:ELSE PRINT "T";FNnpad(T%,2);" ";
X%=rwpar% AND &FF: Y%=rwpar% DIV 256
A%=&7F:S%=USR(XOSWORD):R%=rwpar%?10
IF S% AND &40000000 THEN R%=S% AND &FF
T%=T%+1
IF R%<>0 THEN PRINT "E";:PROCprinthbyte(R%)
UNTIL (R%<=0) OR (R%=17) OR (T%>try%)
IF (cmd%=1) AND (R%=0) THEN IF FNmemcmp(BF%,VB%,cnt%*256) <> 0 THEN R%=-1
IF R%=0 THEN PRINT "OK ";:ELSE IF R%=-1 PRINT "E";:PROCprinthbyte(R%)
PRINT ".";
PRINT TAB(P%, V%);
=R%

:: REM Read disk sector
DEF FNread(d%,t%,s%,c%):=FNdisk(d%,t%,s%,c%,0)

:: REM Write disk sector
DEF FNwrite(d%,t%,s%,c%):=FNdisk(d%,t%,s%,c%,2)

:: REM Verify disk sector (read and compare)
DEF FNverify(d%,t%,s%,c%):=FNdisk(d%,t%,s%,c%,1)

:: REM Read directory sectors
DEF FNreaddir(d%):=FNread(d%,0,0,2)

:: REM Get disk sector count from directory sectors in memory
DEF FNscnt:=(BF%?&107)+((BF%?&106) AND 3)*&100

:: REM Get file count from directory sectors in memory
DEF FNfcnt:=(BF%?&105) DIV 8

:: REM Get disk title from directory sectors in memory
DEF FNtitle
LOCAL I%
FOR I%=0 TO 7:inpbuf%?I%=BF%?I%:NEXT
FOR I%=0 TO 3:inpbuf%?(I%+8)=BF%?(I%+256):NEXT
inpbuf%?12=13
=$inpbuf%

:: REM Get n-th file name from directory
DEF FNfname(N%)
LOCAL I%
?inpbuf%=BF%?(15+N%*8)
inpbuf%?1=ASC"."
FOR I%=0 TO 6:inpbuf%?(2+I%)=BF%?(8+N%*8+I%):NEXT
inpbuf%?9=13
=$inpbuf%

:: REM Get size of n-th file from directory
DEF FNfsize(N%)
=((BF%!(&10C+N%*8)) AND &FFFF)+((BF%?(&10E+N%*8)) AND &30)*&10000

:: REM Get start sector of n-th file from directory
DEF FNfsect(N%)
=BF%?(&10F+N%*8)+(BF%?(&10E+N%*8) AND &3)*&100

:: REM Set error message
DEF PROCerrmsg(act$,D%)
msg$=act$+" error &"+STR$~(R% AND &FF)+" in "+FNsectn(D%,T%,S%,1)
ENDPROC

:: REM Show sector representation
DEF PROCssect(c$)
PRINT TAB(S%+(T% DIV 20)*10,(T% MOD 20)+1);
PRINT STRING$(B%,c$);
ENDPROC

:: REM Show initial disk representation
DEF PROCsdisk
PRINT TAB(0,0);"<Track  0><      20><      40><      60>";
S%=0:B%=10
FOR T%=0 TO (scnt% DIV 10)-1
PROCssect(".")
NEXT
ENDPROC

:: REM Set bad sectors bit
DEF PROCsetbad
bads%?(C% DIV 8)=bads%?(C% DIV 8) OR (2^(C% MOD 8))
ENDPROC

:: REM Clear bad sectors bit
DEF PROCclrbad
bads%?(C% DIV 8)=bads%?(C% DIV 8) AND (255-2^(C% MOD 8))
ENDPROC

:: REM Get bad sector bit
DEF FNisbad:=(bads%?(C% DIV 8) AND (2^(C% MOD 8)))>0

:: REM Get error action
DEF FNgetact
IF act%<>2 THEN a%=act%:ELSE a%=FNaskkey("Ignore/Retry/Fail?","IRF",1):IF a%=ASC"I" THEN a%=1:ELSE IF a%=ASC"F" THEN a%=0: ELSE a%=2
=a%

:: REM Copy and verify disk copy with retries
DEF PROCcopy(sdrv%,ddrv%,scnt%)
LOCAL R%,S%,T%,B%,P%,N%
CLS
PROCmemset(bads%,0,100)
msg$="":PROCmsg("")
PROCsdisk
B%=1:N%=0:C%=0
IF tmode% THEN B%=10
REPEAT
P%=1
S%=C% MOD 10: T%=C% DIV 10
REPEAT
PRINT TAB(1,21);:IF P%=1 THEN PRINT SPC(20): ELSE PRINT "Pass ";P%;
PROCssect("r"):R%=FNread(sdrv%,T%,S%,B%)
IF (R%<>0) THEN PROCerrmsg("Read",sdrv%)
IF (R%=0) THEN PROCssect("w"):R%=FNwrite(ddrv%,T%,S%,B%): IF (R%<>0) THEN PROCerrmsg("Write",ddrv%)
IF (R%=0) AND ver% THEN PROCssect("v"):R%=FNverify(ddrv%,T%,S%,B%): IF (R%>0) OR (R%<-1) THEN PROCerrmsg("Verify",ddrv%)
IF (R%=-1) THEN msg$="Verify mismatch at &"+STR$~(vmis%)+" in "+FNsectn(ddrv%,T%,S%,1)
IF (R%=0) THEN msg$="" ELSE IF (B%<>1) AND (R%<>17) THEN PROCssect("?"):B%=1 ELSE P%=P%+1
PROCmsg(msg$)
UNTIL (R%=0) OR (R%=17) OR (P%>rep%)
a%=0
IF (R%=0) THEN PROCssect("*"):PROCclrbad ELSE IF (R%<>17) THEN PROCssect("e"):PROCsetbad
IF (R%<>0) AND (R%<>17) THEN a%=FNgetact
IF (a%=1) THEN R%=0:N%=N%+1
IF (a%=2) THEN R%=0 ELSE C%=C%+B%:IF ((C% MOD 10)=0) AND tmode% THEN B%=10
UNTIL (R%<>0) OR (C%>=scnt%)
PRINT TAB(1,21);SPC(20);
IF R%=0 AND N%=0 THEN msg$="Disk copy successful"
IF R%=0 AND N%<>0 THEN msg$=STR$(N%)+" sector errors"
PROCaskret(msg$+".",1):msg$=""
lastc%=C%
ENDPROC

:: REM Verify disk (compare sectors)
DEF PROCverify(sdrv%,ddrv%,scnt%)
LOCAL R%,S%,T%,B%,N%
CLS
PROCmemset(bads%,0,100)
PROCmsg("")
PROCsdisk
B%=1:N%=0:C%=0
IF tmode% THEN B%=10
REPEAT
S%=C% MOD 10: T%=C% DIV 10
PROCssect("r"):R%=FNread(sdrv%,T%,S%,B%)
IF (R%<>0) THEN PROCerrmsg("Read",sdrv%)
IF (R%=0) THEN PROCssect("v"):R%=FNverify(ddrv%,T%,S%,B%): IF (R%>0) OR (R%<-1) THEN PROCerrmsg("Verify",ddrv%)
IF (R%=-1) THEN msg$="Verify mismatch at &"+STR$~(vmis%)+" in "+FNsectn(ddrv%,T%,S%,1)
IF (R%=0) THEN msg$="":PROCssect("*"):PROCclrbad ELSE IF (B%<>1) AND (R%<>17) THEN PROCssect("?") ELSE IF (R%=-1) THEN PROCssect("x"):PROCsetbad ELSE IF (R%<>17) PROCssect("e"):PROCsetbad
IF (R%=0) OR (R%=17) THEN a%=0 ELSE IF (B%<>1) THEN a%=2 ELSE a%=FNgetact
IF (a%=1) THEN R%=0:N%=N%+1
IF (a%=2) THEN B%=1:R%=0 ELSE C%=C%+B%:IF ((C% MOD 10)=0) AND tmode% THEN B%=10
PROCmsg(msg$)
UNTIL (R%<>0) OR (C%=scnt%)
IF R%=0 AND N%=0 THEN msg$="Disk verify successful"
IF R%=0 AND N%<>0 THEN msg$=STR$(N%)+" sector errors"
PROCaskret(msg$+".",1):msg$=""
ENDPROC

:: REM Scan disk (read all sectors)
DEF PROCscan(drv%,scnt%)
LOCAL R%,S%,T%,B%,N%
CLS
PROCmemset(bads%,0,100)
PROCmsg("")
PROCsdisk
B%=1:N%=0:C%=0
IF tmode% THEN B%=10
REPEAT
S%=C% MOD 10: T%=C% DIV 10
PROCssect("r"):R%=FNread(sdrv%,T%,S%,B%)
IF (R%<>0) THEN PROCerrmsg("Read",sdrv%)
IF (R%=0) THEN msg$="":PROCssect("*"):PROCclrbad ELSE IF (B%<>1) AND (R%<>17) THEN PROCssect("?") ELSE PROCssect("e"):PROCsetbad
IF (R%=0) OR (R%=17) THEN a%=0 ELSE IF (B%<>1) THEN a%=2 ELSE a%=FNgetact
IF (a%=1) THEN R%=0:N%=N%+1
IF (a%=2) THEN B%=1:R%=0 ELSE C%=C%+B%:IF ((C% MOD 10)=0) AND tmode% THEN B%=10
PROCmsg(msg$)
UNTIL (R%<>0) OR (C%=scnt%)
IF R%=0 AND N%=0 THEN msg$="Disk read successful"
IF R%=0 AND N%<>0 THEN msg$=STR$(N%)+" sector errors"
PROCaskret(msg$+".",1):msg$=""
ENDPROC

:: REM Ask user for confirmation and proceed with disk copy
DEF PROCaskcopy(sdrv%,ddrv%,scnt%)
LOCAL P%
P%=FNaskbool("Proceed with copying to drive #"+STR$(ddrv%)+"?")
IF P% THEN PROCcopy(sdrv%,ddrv%,scnt%):update%=1:ELSE msg$="Cancelled"
ENDPROC

:: REM Display text as menu title
DEF PROCmtitle(t$)
FOR I%=0 TO 1
PRINT TAB(0,I%+1);
PRINT CHR$(132);CHR$(157);CHR$(131);CHR$(141);TAB(20-LEN(t$)/2,I%+1);t$;TAB(39,I%+1);CHR$(156);
NEXT
ENDPROC

:: REM Display status message on the bottom of the screen
DEF PROCmsg(msg$)
PRINT TAB(39,23);CHR$(156);
PRINT TAB(0,23);SPC(38);TAB(0,23);CHR$(132);CHR$(157);CHR$(130);msg$;
ENDPROC

:: REM Update displayed title and sector count
DEF PROCupdate
PROCmsg("")
stitle$="":sscnt%=800:strk%=0:sfcnt%=0
dtitle$="":dscnt%=800:dtrk%=0:dfcnt%=0
IF FNreaddir(sdrv%)=0 THEN stitle$=FNtitle:sscnt%=FNscnt:sfcnt%=FNfcnt
IF (sscnt%<>400) AND (sscnt%<>800) THEN sscnt%=800
IF FNreaddir(ddrv%)=0 THEN dtitle$=FNtitle:dscnt%=FNscnt:dfcnt%=FNfcnt
IF (dscnt%<>400) AND (dscnt%<>800) THEN dscnt%=800
strk%=sscnt% DIV 10:dtrk%=dscnt% DIV 10
msg$="Free memory "+STR$((!4AND&FFFF)-(!2AND&FFFF))+" bytes"
ENDPROC

:: REM Wait for key from allowed set
DEF FNgetkey(set$,M%)
PROCcuron(M%)
REPEAT
ak%=GET
IF (ak%>=ASC"a") AND (ak%<=ASC"z") THEN ak%=ak%-32
UNTIL INSTR(set$,CHR$(ak%))<>0
PROCcuron(0)
=ak%

:: REM Ask user for selection
DEF FNaskkey(ms$,set$,M%)
PROCmsg(ms$)
ak%=FNgetkey(set$,M%)
PROCmsg("")
=ak%

:: REM Ask user for confirmation
DEF FNaskbool(ms$):=(FNaskkey(ms$,"YN",1)=ASC"Y")

:: REM Ask user for a drive number
DEF FNaskdr(ms$):=FNaskkey(ms$,"0123",1)-ASC("0")

:: REM Input line
DEF FNaskln(ms$)
PROCmsg(ms$)
PROCcuron(1)
inpp%=0
N%=0
REPEAT
ak%=GET
IF (ak%=127) AND (inpp%>0) THEN inpp%=inpp%-1:VDU8,32,8
IF (ak%>=32) AND (ak%<127) AND (inpp%<30) THEN inpbuf%?inpp%=ak%:VDUak%:inpp%=inpp%+1
UNTIL ak%=13
inpbuf%?inpp%=13
PROCcuron(0)
PROCmsg("")
=$inpbuf%

:: REM Input number
DEF FNasknum(ms$)
PROCmsg(ms$)
PROCcuron(1)
inpp%=0
N%=0
REPEAT
ak%=GET
IF (ak%=127) AND (inpp%>0) THEN inpp%=inpp%-1:VDU8,32,8
IF (ak%>=ASC"0") AND (ak%<=ASC"9") AND (inpp%<7) THEN inpbuf%?inpp%=ak%:VDUak%:inpp%=inpp%+1
UNTIL ak%=13
inpbuf%?inpp%=13
IF inpp%>0 THEN N%=VAL($inpbuf%)
PROCcuron(0)
PROCmsg("")
=N%

:: REM Prompt user for RETURN key
DEF PROCaskret(ms$,M%)
IF FNaskkey(ms$,CHR$(13),M%)=0 :
ENDPROC

:: REM Ask user for source drive number
DEF PROCsrc
K%=FNaskdr("Enter source drive #")
IF K%=ddrv% THEN ddrv%=sdrv%
sdrv%=K%:msg$=""
ENDPROC

:: REM Ask user for destination drive number
DEF PROCdst
K%=FNaskdr("Enter destination drive #")
IF K%=sdrv% THEN sdrv%=ddrv%
ddrv%=K%:msg$=""
ENDPROC

:: REM Ask user for number of copy retries
DEF PROCpass
rep%=FNasknum("Max. number of passes for copy: ")
IF rep%<1 THEN rep%=1
IF rep%>1000 THEN rep%=1000
ENDPROC

:: REM Ask user for max retries on each access
DEF PROCtries
try%=FNasknum("Max. number of retries: ")
IF try%<1 THEN try%=1
IF try%>10 THEN try%=10
ENDPROC

:: REM Check is file has bad sectors
DEF FNhasbad
LOCAL N%
size%=FNfsize(F%):sect%=FNfsect(F%)
size%=(size%+255) DIV 256
FOR C%=sect% TO sect%+size%-1
IF FNisbad THEN N%=N%+1
NEXT
=N%

:: REM Display list of bad sectors and files
DEF PROCbads
PROCmsg("")
VDU 28,0,22,39,0
CLS:PRINT TAB(0,0)
IF (lastc%<>800) AND (lastc%<>400) THEN PRINT "WARNING! Disk scan incomplete."
C%=0
N%=0
fbad%=-1
PRINT "Bad sectors list:"
REPEAT
isbad%=FNisbad
IF isbad% THEN N%=N%+1:IF (fbad%=-1) THEN fbad%=C%
IF (fbad%<>-1) AND (NOT isbad%) THEN PRINT FNsectn(sdrv%,fbad% DIV 10,fbad% MOD 10,0);"+";FNnpad(C%-fbad%,2);"  ";:fbad%=-1
IF (isbad%) AND ((C% MOD 10) = 9) THEN PRINT FNsectn(sdrv%,fbad% DIV 10,fbad% MOD 10,0);"+";FNnpad(C%-fbad%+1,2);"  ";:fbad%=-1
IF ((C% AND 31)=0) AND (bads%!(C% DIV 32)=0) THEN C%=C%+32 ELSE IF ((C% AND 7)=0) AND (bads%?(C% DIV 8)=0) THEN C%=C%+8 ELSE C%=C%+1
UNTIL C%>=sscnt%
IF (N% MOD 4)<>0 THEN PRINT
PRINT STR$(N%);" bad sectors"
C%=0:isbad%=FNisbad
C%=1:isbad%=isbad% OR FNisbad
IF isbad% THEN PRINT "Catalog damaged" ELSE PRINT "Catalog not damaged"
N%=0
PRINT "Damaged files list:"
FOR F%=sfcnt%-1 TO 0 STEP -1
IF F%>=0 THEN name$=FNfname(F%):IF FNhasbad THEN N%=N%+1:PRINT name$;SPC(10-LEN(name$));
NEXT
IF (N% MOD 4)<>0 THEN PRINT
PRINT STR$(N%);" damaged files"
VDU 28,0,24,39,0
PROCaskret("Press RETURN",1)
ENDPROC

:: REM Display main menu
DEF PROCshmenu
CLS
PROCmtitle("Backup utility")
PRINT
PRINT " (S)rc drive:   ";CHR$(131);sdrv%;" ";FNspad(STR$(strk%),2);"T ";FNspad(STR$(sfcnt%),2);"F ";stitle$
PRINT " (D)st drive:   ";CHR$(131);ddrv%;" ";FNspad(STR$(dtrk%),2);"T ";FNspad(STR$(dfcnt%),2);"F ";dtitle$
PRINT " (T)rack mode:  ";CHR$(131);:IF tmode% THEN PRINT "on": ELSE PRINT "off"
PRINT " (P)asses:      ";CHR$(131);rep%
PRINT " (M)ax tries:   ";CHR$(131);try%
PRINT " (O)n error:    ";CHR$(131);
IF act%=0 THEN PRINT "fail":ELSE IF act%=1 THEN PRINT "ignore":ELSE PRINT "ask"
PRINT " (A)uto verify: ";CHR$(131);:IF ver% THEN PRINT "on": ELSE PRINT "off"
PRINT ""
PRINT " (C)opy disk";TAB(20);" (V)erify"
PRINT " (R)ead disk";TAB(20);" (U)pdate info"
PRINT " (H)exdump"
PRINT " (B)ad sectors and files"
PRINT
PRINT " (.) Catalog"
PRINT " (*) OSCLI"
PRINT " (E)xit"
PROCmsg(msg$)
ENDPROC

:: REM Main menu
DEF PROCmain
ON ERROR PROCerror
*FX 200,1
*FX 4,2
PROCcuron(0)
lastc%=0
sdrv%=0:ddrv%=1
sscnt%=800:dscnt%=800
tmode%=1
rep%=3:try%=3
act%=2:ver%=0
msg$=""
haddr%=FNhaddr
done%=0:update%=1:show%=1
PROCmtitle("Backup utility")
PROCmsg("")
REPEAT
IF update% THEN PROCupdate: update%=0: show%=1
IF show% THEN PROCshmenu: show%=0
key$=GET$
msg$=""
IF (key$>="a") AND (key$<="z") THEN key$=CHR$(ASC(key$)-32)
IF (key$="S") THEN update%=1:PROCsrc
IF (key$="D") THEN update%=1:PROCdst
IF (key$="T") THEN show%=1:tmode%=(tmode%=0)
IF (key$="P") THEN show%=1:PROCpass
IF (key$="M") THEN show%=1:PROCtries
IF (key$="O") THEN show%=1:act%=(act%+1) MOD 3
IF (key$="A") THEN show%=1:ver%=(ver%=0)
IF (key$="C") THEN PROCaskcopy(sdrv%,ddrv%,sscnt%)
IF (key$="V") THEN update%=1:PROCverify(sdrv%,ddrv%,sscnt%)
IF (key$="R") THEN update%=1:PROCscan(sdrv%,sscnt%)
IF (key$="B") THEN show%=1:PROCbads
IF (key$="U") THEN update%=1
IF (key$="H") THEN update%=1:PROChexmain
IF (key$=".") THEN show%=1:PROCcat
IF (key$="*") THEN show%=1:PROCusrcli
IF (key$="E") THEN done%=1
UNTIL done%
*FX 200,0
*FX 4,0
PROCcuron(1)
ENDPROC

:: REM Print hex byte
DEF PROCprinthbyte(A%):CALL printhbyte:ENDPROC

:: REM Print hex number
DEF PROCprinthnum(V%,L%)
!rwpar%=V%
X%=rwpar% AND 255:Y%=rwpar% DIV 256:A%=L%:CALL printhnum
ENDPROC

:: REM Format number and pad with leading zeros
DEF FNnpad(S%,len%):LOCAL st$:st$=STR$(S%)
=STRING$(len%-LEN(st$),"0")+st$

:: REM Format hex number and pad with leading zeros
DEF FNhpad(S%,len%):LOCAL st$:st$=STR$~(S%)
=STRING$(len%-LEN(st$),"0")+st$

:: REM Pad string with spaces
DEF FNspad(st$,len%):=STRING$(len%-LEN(st$)," ")+st$

:: REM Hexdump main loop
DEF PROChexmain
hdrv%=sdrv%:htrk%=0:hsec%=0:hcnt%=10:hoff%=0:ist%=0:msg$="<RETURN>"
hstat%=-1:hact%=0
CLS
W%=0:G%=0
PROChexhdr
REPEAT
k$=GET$
IF (k$>="a") AND (k$<="z") THEN k$=CHR$(ASC(k$)-32)
hv%=-1
IF (k$>="0") AND (k$<="9") THEN hv%=ASC(k$)-48
IF (k$>="A") AND (k$<="F") THEN hv%=ASC(k$)-65+10
IF (ist%=0) AND (k$="D") AND (hdrv%=sdrv%) THEN hdrv%=ddrv%:G%=1:k$=""
IF (ist%=0) AND (k$="D") THEN hdrv%=sdrv%:G%=1
IF (ist%=0) AND (k$="T") THEN ist%=1:PRINT TAB(9,0);
IF (ist%=0) AND (k$="S") THEN ist%=3:PRINT TAB(15,0);
IF (ist%=0) AND (k$="C") THEN ist%=4:PRINT TAB(20,0);
IF (ist%=0) AND (ASC(k$)=13) THEN PROChexld
IF (ist%=0) AND (W%=1) AND (k$="W") THEN PROChexwr
IF (ist%=0) AND (k$="N") AND (hoff%+128<hact%) THEN hoff%=hoff%+128:PRINT TAB(27,0);:PROCprinthnum(hoff%,2):PROChexsh
IF (ist%=0) AND (k$="P") AND (hoff%>=128) THEN hoff%=hoff%-128:PRINT TAB(27,0);:PROCprinthnum(hoff%,2):PROChexsh
IF (ist%=2) AND (hv%>=0) THEN htrk%=(htrk% AND &F0) OR hv%:ist%=0:G%=1:k$=""
IF (ist%=2) AND (ASC(k$)=127) THEN ist%=1:PRINT CHR$(8);:k$=""
IF (ist%=1) AND (hv%>=0) AND (hv%<=4) THEN htrk%=(htrk% AND &F) OR (hv%*16):ist%=2:G%=1
IF (ist%=3) AND (hv%>=0) AND (hv%<=9) THEN hsec%=hv%:ist%=0:G%=1:IF hsec%+hcnt%>10 THEN hcnt%=10-hsec%
IF (ist%=4) AND (hv%>=1) AND (hv%<=10) THEN hcnt%=hv%:ist%=0:G%=1
IF (ASC(k$)=127) THEN ist%=0
IF G% THEN W%=0:G%=0:msg$="<RETURN>":PROChexhdr:IF (ist%=2) THEN PRINT TAB(10,0);
IF (ist%=0) THEN PRINT TAB(0, 24);:PROCcuron(0): ELSE PROCcuron(1)
UNTIL (k$="X") OR ((k$="E") AND (ist%=0))
msg$=""
ENDPROC

:: REM Read sectors for hexdump
DEF PROChexld
hres%=FNread(hdrv%,htrk%,hsec%,hcnt%)
IF hres%<>0 THEN msg$="Error &"+FNhpad((hres% AND &FF),2):ELSE msg$="OK"
IF hres%=0 THEN hact%=hcnt%*256:IF hoff%>hact%-128 THEN hoff%=hact%-128
IF (hres%=0) AND (hdrv%=sdrv%) THEN W%=1: ELSE W%=0
PROChexhdr
IF hres%=0 THEN PROChexsh
ENDPROC

:: REM Copy displayed sectors to destination
DEF PROChexwr
LOCAL P%,R%
PRINT TAB(1,24);SPC(38);
PRINT TAB(0,24);CHR$(157);CHR$(132);
PRINT "Copy ";FNsectn(sdrv%,htrk%,hsec%);" +";FNnpad(hcnt%,2);" to ";FNsectn(ddrv%,htrk%,hsec%);"?";
P%=(FNgetkey("YN",1)=ASC"Y")
IF NOT P% THEN msg$="Cancelled"
IF P% THEN R%=FNwrite(ddrv%,htrk%,hsec%,hcnt%)
IF (R%<>0) THEN msg$="Error &"+FNhpad((R% AND &FF),2)
IF P% AND (R%=0) THEN msg$="Saved"
PROChexhdr
ENDPROC

:: REM Show current sector data hexdump
DEF PROChexsh
PRINT TAB(0, 2);
PROCdump(BF% + hoff%, htrk%*2560+hsec%*256+hoff%, 128)
PRINT TAB(0, 24);
ENDPROC

:: REM Display header and footer in hexdump
DEF PROChexhdr
PRINT TAB(0,0);CHR$(157);CHR$(129);"D";CHR$(132);~hdrv%;CHR$(129);" T";CHR$(132);FNhpad(htrk%,2);CHR$(129);" S";CHR$(132);~hsec%;
PRINT CHR$(129);" C";CHR$(132);~hcnt%;CHR$(129);" off";CHR$(132);FNhpad(hoff%,4);
PRINT TAB(0,24);CHR$(157);CHR$(129);"E";CHR$(132);"xit ";CHR$(129);"N";CHR$(132);"ext ";CHR$(129);"P";CHR$(132);"rev ";
IF W% THEN PRINT CHR$(129);"W";CHR$(132);"rite";:ELSE PRINT STRING$(7," ");
PRINT CHR$(132);FNspad(msg$,9);
ENDPROC

:: REM Compare memory
DEF FNmemcmp(ptr1%,ptr2%,size%)
LOCAL R%
!rwpar%=ptr1%
rwpar%!2=ptr2%
rwpar%!4=size
X%=rwpar% AND 255:Y%=rwpar% MOD 256
R%=USR(memcmp)
vmis%=size%-(((R% + &100) AND &FFFF00) DIV 256)
=R% AND &FF

:: REM Fill memory
DEF PROCmemset(ptr%,val%,size%)
LOCAL R%
!rwpar%=ptr%
rwpar%?2=val%
rwpar%!3=size%
X%=rwpar% AND 255:Y%=rwpar% MOD 256
CALL memset
ENDPROC

:: REM Hexadecimal memory dump
DEF PROCdump(addr%,vaddr%,cnt%)
!rwpar%=vaddr%
rwpar%!4=addr%
rwpar%!6=cnt%
X%=rwpar% AND 255:Y%=rwpar% MOD 256
CALL hexdump
ENDPROC

:: REM Enable or disable cursor
DEF PROCcuron(on%)
VDU 23,1,on%;0;0;0;
ENDPROC

:: REM Execute specified OSCLI
DEF PROCoscli(L$)
LOCAL R%,S%
L$=L$+CHR$(13)
VDU 28,0,21,39,4
CLS
CALL addrof,S%,L$
X%=S% AND 255:Y%=S% DIV 256
R%=USR(XOSCLI) AND &400000FF
IF R%<>0 THEN REPORT:PRINT
VDU 28,0,24,39,0
PROCaskret("Press RETURN",0)
ENDPROC

:: REM Display disk catalog
DEF PROCcat
PROCoscli(".:"+STR$(FNaskdr("Drive #")))
ENDPROC

:: REM Ask user and execute arbitrary OSCLI
DEF PROCusrcli
PROCoscli(FNaskln("*"))
ENDPROC

:: REM Global error handler
DEF PROCerror
ON ERROR OFF
*FX 200,0
*FX 4,0
CLEAR
MODE 7
REPORT:PRINT " in line ";ERL
END
ENDPROC
