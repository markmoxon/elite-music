BEEBASM?=beebasm
PYTHON?=python

.PHONY:all
all:
	$(BEEBASM) -i elite-music.asm -v > compile.txt
