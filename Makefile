BEEBASM?=beebasm
PYTHON?=python

ifeq ($(variant), master)
  variant=2
else ifeq ($(variant), 6502sp)
  variant=3
else
  variant=1
endif

.PHONY:all
all:
	echo _VARIANT=$(variant) > elite-music-build-options.asm
	$(BEEBASM) -i elite-music.asm -v > compile.txt
