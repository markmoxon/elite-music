# Music for Elite on the BBC Micro and BBC Master

This repository contains source code for a sideways ROM that plays the music from the Commodore 64 version of Elite, backported to run on the BBC Micro and BBC Master. It also supports changing the music volume.

For more information on the musical version of BBC Elite, which incorporates this ROM, see the [hacks section of the accompanying website](https://www.bbcelite.com/hacks/bbc_elite_with_music.html).

Build the ROM image using `make`.

The same ROM can be used on the BBC Micro, 6502 Second Processor and BBC Master. The default ROM will work on the BBC Micro version of Elite, but on the 6502SP and BBC Master versions, the ROM needs to be configured with the relevant addresses in the lookup table at &800F (which is done once the ROM image has been loaded into sideways RAM).

* The 6502 Second Processor only needs to update the first two table entries, addrDNOIZ and addrplay1.

* The BBC Master needs to update the whole table, from addrDNOIZ to keyVolUp.

See [the main source file](elite-music.asm) for details.

---

Right on, Commanders!

_Mark Moxon_
