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

 LDA #0                 \ Clear the status flag to indicate we are not playing
 STA musicStatus        \ any music

 LDY #8                 \ Terminate the currently selected music
 STY vgm_finished
 JSR sn_reset

 LDA #%11100110         \ Set noise control (%1110) to white noise (%x1) with
 JSR sn_write           \ high frequency (%10), so the launch sound doesn't get
                        \ corrupted (this sets the sound chip to the same value
                        \ as SOUND &10, &F1, &06, &0C (for the launch sound)

 LDA #3                 \ Select the docking music
 JSR PlayMusic

 LDA #6                 \ Modify the PlayMusic routine so it plays music on the
 STA play1+1            \ next call

 RTS                    \ Return from the subroutine
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
