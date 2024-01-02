;******************************************************************
; 6502 BBC Micro Compressed VGM (VGC) Music Player
; By Simon Morris
; https://github.com/simondotm/vgm-player-bbc
; https://github.com/simondotm/vgm-packer
;******************************************************************

\ MM - Set addresses from main Elite source

 musicWorkspace = &0086 \ Must be the same in all platforms
 musicStatus    = &008F \ Must be the same in all platforms

 DL             = &0095 \ Disc version only, for use in localDELAY

 OSWORD = &FFF1         \ The address for the OSWORD routine

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
\ This table runs from &800F

.platform       EQUB 0          \ 0 = BBC Micro, 64 = 6502SP, 128 = Master
.addrDNOIZ      EQUW &03C6      \ Store DNOIZ here
.addrplay1      EQUW &120F+1    \ Store play1+1 here
.addrDELAY      EQUW localDELAY \ Store DELAY here
.addrSFX        EQUW &121D      \ Store SFX here
.addrBEEP       EQUW localBEEP  \ Store BEEP here

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
 STA modifyPlayDNOIZ1+1 \        STA &FFFF to STA DNOIZ
 STA modifyPlayDNOIZ2+3  
 STA modifyStopDNOIZ1+1
 STA modifyStopDNOIZ2+3  
 STA modifyOptsDNOIZ1+3
 STA modifyBeepDNOIZ1+1
 LDA addrDNOIZ+1
 STA modifyPlayDNOIZ1+2
 STA modifyPlayDNOIZ2+4
 STA modifyStopDNOIZ1+2
 STA modifyStopDNOIZ2+4  
 STA modifyBeepDNOIZ1+2

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

 LDA addrBEEP           \ Modify JSR FFFF to JSR BEEP
 STA modifyOptsBEEP1+1
 LDA addrBEEP+1
 STA modifyOptsBEEP1+2

 LDA addrDELAY          \ Modify JSR FFFF to JSR DELAY
 STA modifyOptsDELAY1+3
 LDA addrDELAY+1
 STA modifyOptsDELAY1+4

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
 CMP #1                 \ disabled it for the music, so keep going to disable
 BNE stop1              \ the sound effects

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

 BIT platform           \ Update the sound effects volume in SFX (but not
 BMI P%+5               \ on a Master, i.e. when bit 7 of platform is set)
 JSR SetSoundFXVolume

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

 TXA                    \ Store X on the stack so we can retrieve it below after
 PHA                    \ making a beep

.^modifyOptsBEEP1

 JSR &FFFF              \ Call the BEEP subroutine to make a short, high beep at
                        \ the new volume level

.^modifyOptsDELAY1

 LDY #10                \ Wait for 10/50 of a second (0.2 seconds)
 JSR &FFFF
 
 PLA                    \ Restore the value of X we stored above
 TAX

 CLC                    \ Clear the C flag so we don't make both a low and high
                        \ beep at the same time

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
}

.localDELAY
{
 JSR localWSCAN         \ Call WSCAN to wait for the vertical sync, so the whole
                        \ screen gets drawn

 DEY                    \ Decrement the counter in Y

 BNE localDELAY         \ If Y isn't yet at zero, jump back to DELAY to wait
                        \ for another vertical sync

 RTS                    \ Return from the subroutine
}

.localWSCAN
{
 LDA #0                 \ Set DL to 0
 STA DL

 LDA DL                 \ Loop round these two instructions until DL is no
 BEQ P%-2               \ longer 0 (DL gets set to 30 in the LINSCN routine,
                        \ which is run when vertical sync has occurred on the
                        \ video system, so DL will change to a non-zero value
                        \ at the start of each screen refresh)

 RTS                    \ Return from the subroutine
}

.localBEEP
{
.^modifyBeepDNOIZ1
 LDX &FFFF              \ Set X to the DNOIZ configuration setting

 BNE KYTB               \ If DNOIZ is non-zero, then sound is disabled, so
                        \ return from the subroutine (as KYTB contains an RTS)

 LDX #LO(localXX16)     \ Otherwise set (Y X) to point to the sound block in
 LDY #HI(localXX16)     \ XX16

 LDA #7                 \ Call OSWORD 7 to makes the sound, as described in the
 JMP OSWORD             \ documentation for variable SFX, and return from the
                        \ subroutine using a tail call
.KYTB

 RTS                    \ Return from the subroutine
}
.localXX16

 EQUW &0003             \ The SOUND block for NOISE #32 - Short, high beep
 EQUW &FFF1
 EQUW &00BC
 EQUW &0001

.SetSoundFXVolume
{
 LDA addrVOL            \ Modify LDA &FFFF to LDA VOL
 STA modifyFXVOL1+1
 STA modifyFXVOL2+1
 STA modifyFXVOL3+1
 LDA addrVOL+1
 STA modifyFXVOL1+2
 STA modifyFXVOL2+2
 STA modifyFXVOL3+2

.modifyFXVOL1

 LDA &FFFF              \ If volume is zero, jump to zero1 to set the SOUND
 BEQ zero1              \ volume to 0

 LDX #0                 \ Equivalent to SOUND volume of &F1 (-15)

 LDA volume_table,X     \ Convert to 1 to 15 depending on volume setting

 EOR #&F0               \ Flip 1-15 to 15-1 and negate to -15 to -1
 CLC
 ADC #1

.zero1

 PHA                    \ Store new volume on stack

 BIT platform           \ If this is not 6502SP, jump to doDisc1 for disc code
 BVC doDisc1

                        \ This is run for the 6502SP version; we update the
                        \ volAddr1,2,4 and volAddr3 addresses so the new volume
                        \ levels get stored in addrSFX and addrSFX+1, where
                        \ they can be used by the NWOSWD handler


 LDA addrSFX            \ Set volAddr4 = addrSFX
 STA volAddr1+1
 STA volAddr2+1
 STA volAddr4+1
 LDA addrSFX+1
 STA volAddr1+2
 STA volAddr2+2
 STA volAddr4+2

 LDA addrSFX            \ Set volAddr3 = addrSFX + 1
 CLC
 ADC #1
 STA volAddr3+1
 LDA addrSFX+1
 ADC #0
 STA volAddr3+2

 JMP updateVolumes      \ Store the new volumes

.doDisc1

                        \ This is run for the disc version; it modifies the
                        \ volume settings in the SFX table

 LDA addrSFX            \ Set volAddr1 = addrSFX + 13
 CLC
 ADC #13
 STA volAddr1+1
 LDA addrSFX+1
 ADC #0
 STA volAddr1+2

 LDA addrSFX            \ Set volAddr2 = addrSFX + 17
 CLC
 ADC #17
 STA volAddr2+1
 LDA addrSFX+1
 ADC #0
 STA volAddr2+2

 LDA addrSFX            \ Set volAddr3 = addrSFX + 21
 CLC
 ADC #21
 STA volAddr3+1
 LDA addrSFX+1
 ADC #0
 STA volAddr3+2

 LDA addrSFX            \ Set volAddr4 = addrSFX + 25
 CLC
 ADC #25
 STA volAddr4+1
 LDA addrSFX+1
 ADC #0
 STA volAddr4+2

.updateVolumes

 LDA #&FF
 STA localXX16+3        \ Scale beep in localBEEP (overwriting the &FF)

 PLA                    \ Fetch new volume from stack

 BNE P%+5               \ If volume is 0 we need to overwrite both bytes, so
 STA localXX16+3        \ scale beep in localBEEP (overwriting the &FF)

 STA localXX16+2        \ Scale beep in localBEEP (overwriting the &F1)

.volAddr1

 STA &FFFF              \ Store in SFX+13 (overwriting the &F1)

.volAddr2

 STA &FFFF              \ Store in SFX+17 (overwriting the &F1)

.volAddr4

 STA &FFFF              \ Store in SFX+25 (overwriting the &F1)

.modifyFXVOL2

 LDA &FFFF              \ If volume is zero, jump to zero2 to set the SOUND
 BEQ zero2              \ volume to 0

 LDX #3                 \ Equivalent to SOUND volume of &F4 (-12)

 LDA volume_table,X     \ Convert to 1 to 15 depending on volume setting

 EOR #&F0               \ Flip 1-15 to 15-1 and negate to -15 to -1
 CLC
 ADC #1

.zero2

.volAddr3

 STA &FFFF              \ Store in SFX+21 (overwriting the &F4)

                        \ We now update the envelope volumes in &08C0

                        \ All envelope amplitudes (last 6 bytes of each
                        \ envelope, AA, AD, AS, AR, ALA, ALD):
                        \
                        \ 8, -2, 0,   -1, 126,  44
                        \ 6,  1, 0,   -2, 120, 126
                        \ 1,  0, 0,   -1,   1,   1
                        \ 22, 0, 0, -127, 126,   0
                        \
                        \ We can change the volumes by scaling the last two
                        \ values, specifically the 120/126 and 44 values
.modifyFXVOL3

 LDA &FFFF              \ If volume is zero, jump to zero2 to set the SOUND
 BEQ zero3              \ volume to 0

 TAX                    \ Save VOL in X

 ASL A                  \ A = 6 * VOL + 2 (so it's in range 0 to 44)
 ASL A
 STA addTemp
 TXA
 ASL A
 CLC
 ADC addTemp
 ADC #2

 STA &08CC              \ Change 4 in ALA and ALD to new volume

 TXA                    \ A = 18 * VOL (so it's in range 0 to 126)
 ASL A
 ASL A
 ASL A
 ASL A
 STA addTemp
 TXA
 ASL A
 CLC
 ADC addTemp

 STA &08CB              \ Change 120 and 126 values in ALA and ALD to new volume
 STA &08DB
 STA &08DC
 STA &08FB

 LDX #1                 \ Set ALA and ALD to 1 for envelope 3
 STX &08EB
 STX &08EC

 RTS                    \ Return from the subroutine

.zero3

 LDA #0                 \ Zero ALA and ALD in all four envelopes to silence them

 STA &08CB
 STA &08CC

 STA &08DB
 STA &08DC

 STA &08EB
 STA &08EC

 STA &08FB
 STA &08FC

 RTS                    \ Return from the subroutine

.addTemp

 EQUB 0

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
