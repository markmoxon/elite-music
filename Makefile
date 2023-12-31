BEEBASM?=beebasm

.PHONY:all
all:
	$(BEEBASM) -i elite-music.asm -v > compile.txt
