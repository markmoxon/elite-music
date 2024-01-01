;******************************************************************
; 6502 BBC Micro Compressed VGM (VGC) Music Player
; By Simon Morris
; https://github.com/simondotm/vgm-player-bbc
; https://github.com/simondotm/vgm-packer
;******************************************************************

\ MM - Set addresses from main Elite source

 musicWorkspace = &0086
 musicStatus    = &008F

\ MM - Enable volume control code

_ENABLE_VOLUME = TRUE

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

\ MM - Added a lookup table for addresses and constants that differ between the
\ three platforms (BBC Micro, 6502SP, BBC Master)
\
\ The default values are those for the BBC Micro, so only the 6502SP and BBC
\ Master versions need to change these values (which is done once the ROM image
\ has been loaded into sideways RAM)
\
\ This table runs from &800F for 11 (&B) bytes

.addrDNOIZ      EQUW &03C6      \ Store DNOIZ here
.addrplay1      EQUW &120F+1    \ Store play1+1 here
.addrVOL        EQUW localVOL   \ Store the address of the volume variable here

.keyE           EQUB &22        \ "E" = &22 BBC Micro, &45 Master
.keyM           EQUB &65        \ "M" = &65 BBC Micro, &4D Master
.keyQ           EQUB &10        \ "Q" = &10 BBC Micro, &51 Master
.keyVolDown     EQUB &66        \ "<" = &66 BBC Micro, &2C Master
.keyVolUp       EQUB &67        \ ">" = &67 BBC Micro, &2E Master

\ MM - Volume variable for BBC Micro and 6502SP (BBC Master already has a
\ volume variable VOL)

.localVOL       EQUB 7          \ Starting volume is 7 (full)

; code routines

.init_tune1
{
IF _ENABLE_VOLUME

    JSR vgm_set_volume  \ MM - Initialise the volume table

ENDIF ; _ENABLE_VOLUME

    JSR ModifyCode      \ MM - Modify code to contain correct addresses

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
IF _ENABLE_VOLUME

    JSR vgm_set_volume  \ MM - Initialise the volume table

ENDIF ; _ENABLE_VOLUME

    JSR ModifyCode      \ MM - Modify code to contain correct addresses

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

.ModifyCode
{
 LDA addrDNOIZ          \ Modify LDA &FFFF to LDA DNOIZ
 STA modifyPlayDNOIZ1+1 \         STA &FFFF to STA DNOIZ
 STA modifyPlayDNOIZ2+3  
 STA modifyStopDNOIZ1+1
 STA modifyStopDNOIZ2+3  
 STA modifyOptsDNOIZ1+3
 LDA addrDNOIZ+1
 STA modifyPlayDNOIZ1+2
 STA modifyPlayDNOIZ2+4
 STA modifyStopDNOIZ1+2
 STA modifyStopDNOIZ2+4  
 STA modifyOptsDNOIZ1+4

 LDA addrVOL            \ Modify LDY &FFFF to LDY VOL
 STA modifyOptsVOL1+1   \        STY &FFFF to STY VOL
 STA modifyOptsVOL2+1
 LDA addrVOL+1
 STA modifyOptsVOL1+2
 STA modifyOptsVOL2+2

 LDA addrplay1          \ Modify STA &FFFF to STA play1+1
 STA modifyPlayPlay1+3
 STA modifyOptsPlay1+3
 STA modifyOptsPlay2+3
 STA modifyOptsPlay3+3
 STA modifyOptsPlay4+3
 LDA addrplay1+1
 STA modifyPlayPlay1+4
 STA modifyOptsPlay1+4
 STA modifyOptsPlay2+4
 STA modifyOptsPlay3+4
 STA modifyOptsPlay4+4

 RTS
}

.StopCurrentTune
{

                        \ MM - routine added to stop music, to save a few bytes
                        \ in the main Elite codebase

 LDA #0                 \ Clear the status flag to indicate we are not playing
 STA musicStatus        \ any music

.^modifyStopDNOIZ1

 LDA &FFFF              \ If DNOIZ is 1, then sound was enabled before we
 CMP #1                 \ disabled it for the music, so keep going to enable the
 BNE stop1              \ sound effects

.^modifyStopDNOIZ2

 LDA #0                 \ Set DNOIZ = 1 to disable sound effects while the
 STA &FFFF              \ music is playing

.stop1

 LDY #8                 \ Terminate the currently selected music
 STY vgm_finished
 JSR sn_reset

 LDA #%11100110         \ Set noise control (%1110) to white noise (%x1) with
 JSR sn_write           \ high frequency (%10), so the launch sound doesn't get
                        \ corrupted (this sets the sound chip to the same value
                        \ as SOUND &10, &F1, &06, &0C (for the launch sound)

 JSR init_tune2         \ Select the docking music

.^modifyPlayPlay1

 LDA #6                 \ Modify the PlayMusic routine so it plays music on the
 STA &FFFF              \ next call

 RTS                    \ Return from the subroutine

}

.PlayCurrentTune
{

                        \ MM - routine added to play music, which checks to see
                        \ whether sound is enabled before playing anything
                        \
                        \ We repurpose DNOIZ so it has the following values:
                        \
                        \   * 0 = sound is configured on (default)
                        \
                        \   * 1 = sound is configured on but music is playing,
                        \         so disable sound effects while the music plays
                        \
                        \   * &FF = sound is configured off
                        \
                        \ When DNOIZ is non-zero, sound effects are not made, so
                        \ we do not get sound effects during music play, as
                        \ DNOIZ is 1

.^modifyPlayDNOIZ1

 LDX &FFFF              \ If DNOIZ is &FF, then sound is disabled, so
 CPX #&FF               \ return from the subroutine
 BEQ tune1

 BIT musicOptions       \ If bit 7 of musicOptions is set then music is
 BMI tune1              \ disabled, so return from the subroutine

.^modifyPlayDNOIZ2

 LDA #1                 \ Set DNOIZ = 1 to disable sound effects while the
 STA &FFFF              \ music is playing

 JMP vgm_update         \ Otherwise sound is enabled, so jump to vgm_update to
                        \ play the music

.tune1

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

 CPX keyQ               \ If "Q" is not being pressed, skip to DK7
 BNE DK7

.^modifyOptsDNOIZ1

 LDA #&FF
 STA &FFFF              \ "Q" is being pressed, so set DNOIZ to X, which is
                        \ non-zero (keyQ), so this will turn the sound off

 JSR StopCurrentTune    \ Stop the current tune

.DK7

                        \ Next we implement the volume logic from the Master

.^modifyOptsVOL1

 LDY &FFFF              \ Fetch the current volume setting into Y

 CPX keyVolUp           \ If "." is being pressed (i.e. the ">" key) then jump
 BEQ DOVOL1             \ to DOVOL1 to increase the volume

 CPX keyVolDown         \ If "," is not being pressed (i.e. the "<" key) then
 BNE DOVOL4             \ jump to DOVOL4 to skip the following

 DEY                    \ The volume down key is being pressed, so decrement the
                        \ volume level in Y

 EQUB &24               \ Skip the next instruction by turning it into &24 &1A,
                        \ or BIT &001A, which does nothing apart from affect the
                        \ flags

.DOVOL1

 INY                    \ The volume up key is being pressed, so increment the
                        \ volume level in Y

 TYA                    \ Copy the new volume level to A

 AND #%11111000         \ If any of bits 3-7 are set, skip to DOVOL3 as we have
 BNE DOVOL3             \ either increased the volume past the maximum volume of
                        \ 7, or we have decreased it below 0 to -1, and in
                        \ neither case do we want to change the volume as we are
                        \ already at the maximum or minimum level

.^modifyOptsVOL2

 STY &FFFF              \ Store the new volume level in VOL

.DOVOL3

.^modifyOptsPlay1

 LDA #6                 \ Modify the PlayMusic routine so it plays music on the
 STA &FFFF              \ next call

 BIT setVFlag           \ Set the V flag and clear the C flag

 RTS                    \ Return from the subroutine to make a high beep to
                        \ indicate a volume change

.DOVOL4

                        \ The new "M" option switches music on and off

 CPX keyM               \ If "M" is not being pressed, skip to opts1
 BNE opts1

 JSR StopCurrentTune    \ Stop the current tune

 LDA #%10000000         \ "M" is being pressed, so flip bit 7 of musicOptions
 EOR musicOptions
 STA musicOptions

 JMP opts4              \ Return from the subroutine with the C flag set, so we
                        \ can make a beep and delay for a bit

.opts1

                        \ The new "E" option swaps the docking and title tunes

 CPX keyE               \ If "E" is not being pressed, skip to opts3
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

.^modifyOptsPlay2

 LDA #6                 \ Modify the PlayMusic routine so it plays music on the
 STA &FFFF              \ next call

 STA musicStatus        \ Start playing music again by setting musicStatus to
                        \ a non-zero value

.opts2

 JMP opts4              \ Return from the subroutine with the C flag set, so we
                        \ can make a beep and delay for a bit

.opts3

IF _ENABLE_VOLUME

 JSR vgm_set_volume     \ Update the volume table in case volume or music
                        \ settings have changed

ENDIF ; _ENABLE_VOLUME

.^modifyOptsPlay3

 LDA #6                 \ Modify the PlayMusic routine so it plays music on the
 STA &FFFF              \ next call

 CLC                    \ Return from the subroutine with the C flag clear so
 CLV                    \ we know not to make a beep, and the V flag clear so
 RTS                    \ we know not to make a high beep

.opts4

IF _ENABLE_VOLUME

 JSR vgm_set_volume     \ Update the volume table in case volume or music
                        \ settings have changed

ENDIF ; _ENABLE_VOLUME

.^modifyOptsPlay4

 LDA #6                 \ Modify the PlayMusic routine so it plays music on the
 STA &FFFF              \ next call

 SEC                    \ Return from the subroutine with the C flag set, so we
 CLV                    \ can make a beep and delay for a bit, and the V flag
 RTS                    \ clear so we know not to make a high beep

.setVFlag

 EQUB %01000000         \ Used to set the V flag to denote a volume change
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

SAVE "elite-music.rom", start, end, start
