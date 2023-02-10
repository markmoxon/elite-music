;******************************************************************
; 6502 BBC Micro Compressed VGM (VGC) Music Player
; By Simon Morris
; https://github.com/simondotm/vgm-player-bbc
; https://github.com/simondotm/vgm-packer
;******************************************************************


;----------------------------------------------------------------------------------------------------------
; Common code headers
;----------------------------------------------------------------------------------------------------------
; Include common code headers here - these can declare ZP vars from the pool using SKIP...

INCLUDE "lib/vgcplayer_config.h.asm"

; Allocate vars in ZP
.zp_start
ORG &92                 \ MM - changed to match zero page free space in disc Elite
GUARD &9E

INCLUDE "lib/vgcplayer.h.asm"
.zp_end


;-------------------------------------------
; swram bank
;-------------------------------------------

ORG &8000
GUARD &C000

.start


;-------------------------------------------
; main
;-------------------------------------------

.jumptable
jmp init_tune1  ; &8000
jmp init_tune2  ; &8003
jmp vgm_update  ; &8006
jmp ResetMusic  ; &8009 \ MM - added to reset Elite envelopes after music stops

; code routines

.init_tune1
{
    ; initialize the vgm player with a vgc data stream
    lda #hi(vgm_stream_buffers)
    ldx #lo(vgm_data1)
    ldy #hi(vgm_data1)
    sec ; set carry to enable looping
    jmp vgm_init
}

; set carry to enable looping
.init_tune2
{
    ; initialize the vgm player with a vgc data stream
    lda #hi(vgm_stream_buffers)
    ldx #lo(vgm_data2)
    ldy #hi(vgm_data2)
    sec ; set carry to enable looping
    jmp vgm_init
}

.ResetMusic
{
                        \ MM - routine added to enable sounds to be reset, using
                        \ the envelope definitions from elite-loader3.asm

 musicStatus = &9B
 PlayMusic = &11FE
 play1 = &120F
 OSWORD = &FFF1

 MACRO FNE I%

  LDX #LO(E%+I%*14)     \ Set (Y X) to point to the I%-th set of envelope data
  LDY #HI(E%+I%*14)     \ in E%

  LDA #8                \ Call OSWORD with A = 8 to set up sound envelope I%
  JSR OSWORD

 ENDMACRO

 LDA #0                 \ Clear the status flag to indicate we are not playing
 STA musicStatus        \ any music

 LDY #8                 \ Terminate the currently selected music
 STY vgm_finished
 JSR sn_reset

 LDA #%11000000         \ Set tone 1 frequency first byte (lower 4 bits in bits 0-3)   $802D
 JSR sn_write
 LDA #%00000101         \ Set tone 1 frequency second byte (upper 6 bits in bits 0-5)  $8032
 JSR sn_write

 LDA #%11101111         \ Set noise to white noise with tone 1 frequency
 JSR sn_write

 LDA #3                 \ Select the docking music
 JSR PlayMusic

 LDA #6                 \ Modify the PlayMusic routine so it plays music on the
 STA play1+1            \ next call

 FNE 0                  \ Set up sound envelopes 0-3 using the FNE macro
 FNE 1
 FNE 2
 FNE 3

 RTS                    \ Return from the subroutine

.E%

 EQUB 1, 1, 0, 111, -8, 4, 1, 8, 8, -2, 0, -1, 126, 44
 EQUB 2, 1, 14, -18, -1, 44, 32, 50, 6, 1, 0, -2, 120, 126
 EQUB 3, 1, 1, -1, -3, 17, 32, 128, 1, 0, 0, -1, 1, 1
 EQUB 4, 1, 4, -8, 44, 4, 6, 8, 22, 0, 0, -127, 126, 0
}

; library code

INCLUDE "lib/vgcplayer.asm"

; tune data

.vgm_data1
INCBIN "music/Elite.C64.Track1.vgc"

PRINT "      vgc tune 1 size is",P%-vgm_data1,"bytes"

.vgm_data2
INCBIN "music/Elite.C64.Track2.vgc"

PRINT "      vgc tune 2 size is",P%-vgm_data2,"bytes"

H%=P%
ALIGN 256
PRINT "          alignment lost",(P%-H%),"bytes"
.vgm_buffer_start

; reserve space for the vgm decode buffers (8x256 = 2Kb)
.vgm_stream_buffers
    skip 256
    skip 256
    skip 256
    skip 256
    skip 256
    skip 256
    skip 256
    skip 256

.vgm_buffer_end
.end

PRINT "           total size is",(end-start),"bytes"

; save file for SWRAM.
SAVE "elite-music.rom", start, end, start


; test program.
PUTBASIC "test-music.bas", "Test"
PUTFILE "elite-music.rom", "Music", start, start
