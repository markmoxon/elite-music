;******************************************************************
; 6502 BBC Micro Compressed VGM (VGC) Music Player
; By Simon Morris
; https://github.com/simondotm/vgm-player-bbc
; https://github.com/simondotm/vgm-packer
;******************************************************************


\ MM - Set addresses from main Elite source

INCLUDE "elite-music-build-options.asm"

_DISC_VERSION           = (_VARIANT = 1)
_MASTER_VERSION         = (_VARIANT = 2)
_6502SP_VERSION         = (_VARIANT = 3)

IF _DISC_VERSION

 DL             = &008B
 musicWorkspace = &0092
 musicStatus    = &009B
 musicOptions   = &03C4     \ NOSTM+1, which is unused in the disc version
 DNOIZ          = &03C6
 PlayMusic      = &11FE
 play1          = &120F

 keyE           = &22
 keyM           = &65
 keyQ           = &10

ELIF _MASTER_VERSION

 DL             = &005B
 musicWorkspace = &008C
 musicStatus    = &0095
 musicOptions   = &2C41     \ COMC+1, which is unused in the Master version
 DNOIZ          = &2C55
 PlayMusic      = &2D5F
 play1          = &2D70

 keyE           = &45
 keyM           = &4D
 keyQ           = &51

ELIF _6502SP_VERSION

 DL             = &008B
 musicWorkspace = &0092
 musicStatus    = &009B
 musicOptions   = &03C4
 DNOIZ          = &03C6
 PlayMusic      = &11FE
 play1          = &120F

 keyE           = &45
 keyM           = &4D
 keyQ           = &51

ENDIF

;----------------------------------------------------------------------------------------------------------
; Common code headers
;----------------------------------------------------------------------------------------------------------
; Include common code headers here - these can declare ZP vars from the pool using SKIP...

INCLUDE "lib/vgcplayer_config.h.asm"

; Allocate vars in ZP
.zp_start
ORG musicWorkspace      \ MM - changed to match zero page free space in Elite
GUARD musicWorkspace+8

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
jmp init_tune1      ; &8000
jmp init_tune2      ; &8003
jmp PlayCurrentTune ; &8006 \ MM - added check to skip playing if sound is disabled
jmp StopCurrentTune ; &8009 \ MM - moved into sideways RAM to save a few bytes
jmp ProcessOptions  ; &800C \ MM - process enhanced music-related pause options

; code routines

.init_tune1
{
    BIT musicOptions    \ MM - If bit 6 of musicOptions is set then tunes are
    BVS init_tune2s     \ swapped, so initialise tune 2 instead

.^init_tune1s

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
    BIT musicOptions    \ MM - If bit 6 of musicOptions is set then tunes are
    BVS init_tune1s     \ swapped, so  initialise tune 1 instead

.^init_tune2s

    ; initialize the vgm player with a vgc data stream
    lda #hi(vgm_stream_buffers)
    ldx #lo(vgm_data2)
    ldy #hi(vgm_data2)
    sec ; set carry to enable looping
    jmp vgm_init
}

.StopCurrentTune
{

                        \ MM - routine added to stop music, to save a few bytes
                        \ in the main Elite codebase

 LDA #0                 \ Clear the status flag to indicate we are not playing
 STA musicStatus        \ any music

 LDY #8                 \ Terminate the currently selected music
 STY vgm_finished
 JSR sn_reset

 LDA #%11100110         \ Set noise control (%1110) to white noise (%x1) with
 JSR sn_write           \ high frequency (%10), so the launch sound doesn't get
                        \ corrupted (this sets the sound chip to the same value
                        \ as SOUND &10, &F1, &06, &0C (for the launch sound)

 JSR init_tune2         \ Select the docking music

 LDA #6                 \ Modify the PlayMusic routine so it plays music on the
 STA play1+1            \ next call

 RTS                    \ Return from the subroutine

}

.PlayCurrentTune
{

                        \ MM - routine added to play music, which checks to see
                        \ whether sound is enabled before playing anything

 LDA DNOIZ              \ If DNOIZ is non-zero, then sound is disabled, so
 BNE play1              \ return from the subroutine

 BIT musicOptions       \ If bit 7 of musicOptions is set then music is
 BMI play1              \ disabled, so return from the subroutine

 JMP vgm_update         \ Otherwise sound is enabled, so jump to vgm_update to
                        \ play the music

.play1

 RTS                    \ Sound is disabled, so return from the subroutine

}

.ProcessOptions
{

                        \ MM - routine added to process music-related pause
                        \ options, as well as the "Q" option where the patch is
                        \ injected
                        \
                        \ We store the music options in musicOptions as follows:
                        \
                        \   * Bit 7 set = disable music
                        \           clear = enable music (default)
                        \
                        \   * Bit 6 set = swap tunes 1 and 2
                        \           clear = default tunes (default)

                        \ We start with the "Q" logic that we replaced with the
                        \ injected call to this routine

 CPX #keyQ              \ If "Q" is not being pressed, skip to DK7
 BNE DK7

 STX DNOIZ              \ "Q" is being pressed, so set DNOIZ to X, which is
                        \ non-zero (&10), so this will turn the sound off

 JSR StopCurrentTune    \ Stop the current tune

.DK7

                        \ The new "M" option switched music on and off

 CPX #keyM              \ If "M" is not being pressed, skip to opts1
 BNE opts1

 JSR StopCurrentTune    \ Stop the current tune

 LDA #%10000000         \ "M" is being pressed, so flip bit 7 of musicOptions
 EOR musicOptions
 STA musicOptions

 JMP opts4              \ Return from the subroutine with the C flag set, so
                        \ can make a beep and delay for a bit

.opts1

                        \ The new "E" option swaps the docking and title tunes

 CPX #keyE              \ If "E" is not being pressed, skip to opts3
 BNE opts3

 LDA musicStatus        \ Store the flags for musicStatus on the stack 
 PHA

 JSR StopCurrentTune    \ Stop the current tune

 LDA #%01000000         \ "E" is being pressed, so flip bit 7 of musicOptions
 EOR musicOptions
 STA musicOptions

 JSR init_tune2         \ Select the docking music

 PLA                    \ If we were not playing music before we switched tunes,
 BEQ opts2              \ jump to opts2

 LDA #6                 \ Modify the PlayMusic routine so it plays music on the
 STA play1+1            \ next call

 STA musicStatus        \ Start playing music again by setting musicStatus to
                        \ a non-zero value

.opts2

 JMP opts4              \ Return from the subroutine with the C flag set, so
                        \ can make a beep and delay for a bit

.opts3

 LDA #6                 \ Modify the PlayMusic routine so it plays music on the
 STA play1+1            \ next call

 CLC                    \ Return from the subroutine with the C flag clear so
 RTS                    \ we know not to make a beep

.opts4

 LDA #6                 \ Modify the PlayMusic routine so it plays music on the
 STA play1+1            \ next call

 SEC                    \ Return from the subroutine with the C flag set, so
 RTS                    \ can make a beep and delay for a bit

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

IF _DISC_VERSION

 SAVE "elite-music-disc.rom", start, end, start

ELIF _MASTER_VERSION

 SAVE "elite-music-master.rom", start, end, start

ELIF _6502SP_VERSION

 SAVE "elite-music-6502sp.rom", start, end, start

ENDIF
