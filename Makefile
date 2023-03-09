BEEBASM?=beebasm
PYTHON?=python

ifeq ($(variant), master)
  variant-music=2
else ifeq ($(variant), 6502sp)
  variant-music=3
else
  variant-music=1
endif

.PHONY:all
all:
	echo _VARIANT=$(variant-music) > elite-music-build-options.asm
	$(BEEBASM) -i elite-music.asm -v > compile.txt
