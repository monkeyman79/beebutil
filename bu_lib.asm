HIMEM=&7700
MC%=HIMEM

osrdrm=&FFB9
osargs=&FFDA
osasci=&FFE3
osword=&FFF1
osbyte=&FFF4
oscli=&FFF7
escf=&FF
brkv=&202
fx200=&258
stack=&100

zp=&A8

ORG MC%
GUARD &7C00

\ Y,X = 6 bytes block: WORD ptr1, WORD ptr2, WORD size
.memcmp_vect     JMP memcmp
\ Y,X = 5 bytes block: WORD ptr1, BYTE val, WORD size
.memset_vect     JMP memset
\ Y,X = 8 bytes block: DWORD vaddr, WORD addr, WORD size
.hexdump_vect    JMP hexdump
\ Y,X = address of integer number to show, A = number of digits
.printhnum_vect  JMP hnum
\ A = byte to show
.printhbyte_vect JMP hbyte
\ Uses CALL block at address &600
.addrof_vect     JMP addrof
\ X = rom number
.printrom_vect   JMP printrom

\ Same parameters as OSBYTE
.XOSBYTE_vect    JMP XOSBYTE
\ Same parameters as OSWORD
.XOSWORD_vect    JMP XOSWORD
\ Same parameters as OSCLI
.XOSCLI_vect     JMP XOSCLI


\ Machine code part of the FNmemcmp function
.memcmp
  PHA:PHA
  LDA zp+6:PHA
  LDA zp+5:PHA:LDA zp+4:PHA
  LDA zp+3:PHA:LDA zp+2:PHA
  LDA zp+1:PHA:LDA zp:PHA
  STX zp:STY zp+1
  LDY #0:LDA (zp),Y:STA zp+2
  INY:LDA (zp),Y:STA zp+3
  INY:LDA (zp),Y:STA zp+4
  INY:LDA (zp),Y:STA zp+5
  INY:LDA (zp),Y:TAX:INX
  INY:LDA (zp),Y:TAY:INY:STY zp+6
  LDY #0
.mc1
  DEX:BEQ mc4
.mc3
  LDA (zp+2),Y
  CMP (zp+4),Y:BNE mc2
  INY:BNE mc1:INC zp+3:INC zp+5
  JMP mc1
.mc4
  DEC zp+6:BNE mc3
.mc2
  PHP:LDY zp+6:DEY:DEX:TXA:TSX:STA stack+9,X
  PLA:AND #2:LSR A:EOR #1:STA stack+10,X
  PLA:STA zp:PLA:STA zp+1
  PLA:STA zp+2:PLA:STA zp+3
  PLA:STA zp+4:PLA:STA zp+5
  PLA:STA zp+6
  PLA:TAX:PLA
.exec
  RTS

.memset
  LDA zp+5:PHA:LDA zp+4:PHA
  LDA zp+3:PHA:LDA zp+2:PHA
  LDA zp+1:PHA:LDA zp:PHA
  STX zp:STY zp+1
  LDY #0:LDA (zp),Y:STA zp+2
  INY:LDA (zp),Y:STA zp+3
  INY:INY:LDA (zp),Y:TAX:INX
  INY:LDA (zp),Y:TAY:INY:STY zp+4
  LDY #2:LDA (zp),Y
  LDY #0
.ms1
  DEX:BEQ ms2
  STA (zp+2),Y
  INY:BNE ms1:INC zp+2
  JMP ms1
.ms2
  DEC zp+4:BNE ms1
  PLA:STA zp:PLA:STA zp+1
  PLA:STA zp+2:PLA:STA zp+3
  PLA:STA zp+4:PLA:STA zp+5
  RTS

\ Display byte as hexadecimal
.hbyte
  PHA:LSR A:LSR A:LSR A:LSR A
  JSR hb1:PLA
.hb1
  PHA:AND #&F
  CMP #10:BCC P%+4:ADC #6
  ADC #&30:JSR osasci
  PLA:RTS

\ Display long hexadecimal number
.hnum
  PHA
  LDA &81:PHA:LDA &80:PHA
  STX &80:STY &81
  TSX:LDA &0103,X:TAY
.hn1
  DEY:BMI hn2
  LDA (&80),Y:JSR hbyte
  JMP hn1
.hn2
  LDY &81:LDX &80
  PLA:STA &80:PLA:STA &81
  PLA:RTS

\ Display string of hex bytes
.hbytes
  PHA
  LDA &82:PHA:LDA &81:PHA:LDA &80:PHA
  STX &80:STY &81
  TSX:LDA stack+4,X
  CMP #0:BEQ hbs2
  STA &82
  LDY #0
.hbs1
  LDA (&80),Y:JSR hbyte
  INY:CPY &82:BEQ hbs2
  LDA #32:JSR osasci
  JMP hbs1
.hbs2
  LDY &81:LDX &80
  PLA:STA &80:PLA:STA &81:PLA:STA &82
  PLA:RTS

\ Display ascii chars or "." if not readable
.hascii
  PHA
  LDA &82:PHA:LDA &81:PHA:LDA &80:PHA
  STX &80:STY &81
  TSX:LDA stack+4,X
  STA &82
  LDY #0
.ha1
  CPY &82:BEQ ha4
  LDA (&80),Y
  CMP #32:BCC ha2
  CMP #127:BCC ha3
.ha2
  LDA #'.'
.ha3
  JSR osasci
  INY:BNE ha1
.ha4
  LDY &81:LDX &80
  PLA:STA &80:PLA:STA &81:PLA:STA &82
  PLA:RTS

\ Print space
.spc
  PHA:LDA #32:JSR osasci:PLA:RTS

\ Print number of spaces
.spcs
  CPX #0:BEQ sp2
.sp1
  JSR spc:DEX:BNE sp1
.sp2
  RTS

.vaddr EQUD 0
.haddr EQUW 0
.hsize EQUW 0

\ Hexadecimal dump
.hexdump
  LDA zp+2:PHA:LDA zp+1:PHA:LDA zp:PHA
  STX zp:STY zp+1
  LDY #0:LDA (zp),Y:STA vaddr
  INY:LDA (zp),Y:STA vaddr+1
  INY:LDA (zp),Y:STA vaddr+2
  INY:LDA (zp),Y:STA vaddr+3
  INY:LDA (zp),Y:STA haddr
  INY:LDA (zp),Y:STA haddr+1
  INY:LDA (zp),Y:STA hsize
  INY:LDA (zp),Y:STA hsize+1
.hd1
  LDY #8
  LDA hsize+1:BNE hd3
  LDA hsize:BNE hd2
  PLA:STA zp:PLA:STA zp+1:PLA:STA zp+2
  RTS
.hd2
  CMP #8:BCS hd3
  TAY
.hd3
  STY zp+2
  LDY #vaddr DIV 256:LDX #vaddr AND 255
  LDA #3
  JSR hnum
  JSR spc
  LDY haddr+1:LDX haddr:LDA zp+2
  JSR hbytes
  SEC
  LDA #25:SBC zp+2:SBC zp+2:SBC zp+2
  TAX
  JSR spcs
  LDY haddr+1:LDX haddr:LDA zp+2
  JSR hascii
  SEC
  LDA #8:SBC zp+2
  TAX
  JSR spcs
  LDA #&D:JSR osasci
  SEC
  LDA hsize:SBC zp+2:STA hsize
  BCS P%+5:DEC hsize+1
  CLC
  LDA haddr:ADC zp+2:STA haddr
  BCC P%+5:INC haddr+1
  CLC
  LDA vaddr:ADC zp+2:STA vaddr
  BCC hd4:INC vaddr+1
  BNE hd4:INC vaddr+2
  BNE hd4:INC vaddr+3
.hd4
  JMP hd1

\ Print string from ROM A at address (&F6)
.printrom_str
  PHA
  TAY
  JSR osrdrm
  INC &F6
  CMP #0
  BEQ prs1
  CMP #32
  BCC prs1
  CMP #127
  BCS prs1
  JSR osasci
  PLA
  JMP printrom_str
.prs1
  PLA
  RTS

\ Print " / "
.sep
  PHA
  LDA #'/'
  JSR osasci
  PLA
  RTS


\ Print ROM title, version and copyright
.printrom
  LDA #'(':JSR osasci
  TXA
  JSR hb1
  LDA #')':JSR osasci
  JSR spc
  LDA &02A1,X
  BEQ pr2
  JSR spc
  JSR spc
  JSR spc
  JMP pr3
.pr2
  LDA #'O'
  JSR osasci
  LDA #'F'
  JSR osasci
  JSR osasci
.pr3
  JSR spc
  LDA &F6:PHA
  LDA &F7:PHA
  LDA &80:PHA
  LDA #&80:STA &F7
  LDA #&07:STA &F6
  TXA
  PHA
  TAY
  JSR osrdrm
  STA &80
  LDA #&09:STA &F6
  PLA
  JSR printrom_str
  LDX &80
  INX
  CPX &F6
  BEQ pr1
  JSR sep
  JSR printrom_str
.pr1
  JSR sep
  JSR printrom_str
  LDA #13
  JSR osasci
  PLA:STA &80
  PLA:STA &F7
  PLA:STA &F6
  RTS

.oldSP EQUB 0

\ Based on beebwiki.mdfs.net/Catching_errors
.xos_call
  PHA:TXA:PHA
  LDA fx200:PHA
  LDA brkv+1:PHA:LDA brkv:PHA
  LDA oldSP:PHA:TSX:STX oldSP
  LDA #0:STA fx200
  LDA #error DIV 256:STA brkv+1
  LDA #error AND 255:STA brkv
  LDA #(return-1)DIV 256:PHA
  LDA #(return-1)AND 255:PHA
  PHA:PHA:PHA:PHA
  LDA zp+1:PHA:LDA zp:PHA:CLC
  LDA stack+7,X:STA zp:ADC #2:STA stack+7,X
  LDA stack+8,X:STA zp+1:ADC #0:STA stack+8,X
  TYA:PHA:TSX
  LDY #2:LDA (zp),Y:STA stack+7,X
  DEY:LDA (zp),Y:STA stack+6,X
  LDA stack+15,X:STA stack+5,X
  LDA stack+14,X:STA stack+4,X
\ Stack holds Y, zp, X, A, oscall, ret, oldSP, oldbrkv, oldfx200, X, A, main
  PLA:TAY:PLA:STA zp:PLA:STA zp+1
  PLA:TAX:PLA:PHP
  RTI
  
\ Normal return from os call
\ Stack holds oldSP, oldbrkv, X, A, main
.return
  BIT escf:BMI escape:
  PHA:TXA:TSX:STA stack+6,X
  PLA:STA stack+6,X
  PLA:STA oldSP
  PLA:STA brkv:PLA:STA brkv+1:PLA:STA fx200
  PLA:TAX:PLA
  CLV:RTS
\ Escape captured
.escape
  BRK
  EQUB &11
  EQUD &61637345 \ "Escape"
  EQUD &00004570
\ Error return from os call
.error
  LDX oldSP:TXS:PLA:STA oldSP
  PLA:STA brkv:PLA:STA brkv+1:PLA:STA fx200
  PLA:PLA:LDY #0:STY escf:LDA (&FD),Y
  BIT P%-1:RTS

\ Call OSBYTE and catch errors
.XOSBYTE
  JSR xos_call:EQUW osbyte:RTS

\ Call OSWORD and catch errors
.XOSWORD
  JSR xos_call:EQUW osword:RTS

\ Call OSCLI and catch errors
.XOSCLI
  JSR xos_call:EQUW oscli
  BVS P%+4:LDA #&0
  RTS

\ Get address of variable
.addrof
  LDA &600:CMP #2:BNE ad3
  LDA &603:CMP #4:BNE ad3
  LDA &606:CMP #129:BEQ ad2
  LDX &604:LDY &605
.ad1
  LDA &81:PHA:LDA &80:PHA
  LDA &601:STA &80
  LDA &602:STA &81
  TYA:LDY #1:STA (&80),Y
  TXA:DEY:STA (&80),Y
  PLA:STA &80:PLA:STA &81
  RTS
.ad2
  LDA &81:PHA
  LDA &80:PHA
  LDA &604:STA &80
  LDA &605:STA &81
  LDY #0:LDA (&80),Y:TAX
  INY:LDA (&80),Y:TAY
  PLA:STA &80
  PLA:STA &81
  JMP ad1
.ad3
  BRK
  EQUB 31
  EQUS "Arguments", 0

.end

SAVE "M.BU_LIB", MC%, end, exec
PUTBASIC "butil.bbas", "BUTIL"
PUTTEXT "!boot", "!BOOT", 0
