PROJECT = studio

$(PROJECT).bin: $(PROJECT).asm include/bios.inc include/kernel.inc
	asm02 -b -L $(PROJECT).asm

clean:
	-rm -f $(PROJECT).bin
	-rm -f $(PROJECT).lst

