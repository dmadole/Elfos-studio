# Elfos-studio

This is an interrupt-driven driver for the 1854 UART under Elf/OS. This is specifically intended for the forthcoming 1802/Mini UART card. This is actually a hybrid polled/interrupt driver, using polled I/O for the transmit data, and interrupt-driven for the input data. This was found to give the best compromise of performance and function.

This is still a work in progress, but works well for what is implemented. This requires Elf/OS Kernel 0.4.0 which as of the time of this writing has not been released yet. Features include:

* Full buffered type-ahead capability
* Output hardware flow control via 1854 ES pin
* Output software XON/XOFF flow control
* Efficient design using "thin call" subroutines
* Only uses 512 bytes of memory including buffer
* Supports up to 38,400 baud at 4 Mhz clock

Input flow control is planned still, as well as an attempt to get the speed up to 57,600 baud if possible.

**Q: Why is it named "studio"?**  
A: Because I create a lot of programs that end in "o"; since it is a console driver, as a play on stdio.h which is a standard library for I/O in the C language; and because it is for the 18**54** UART, as an homage to "Studio 54," the former New York City disco nightclub.  
