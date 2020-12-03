// Raster Bars for Atari XL / XE

#pragma target(atarixl)
#pragma emulator("65XEDebugger")
#include <atari-xl.h>

void main() {
    // Disable IRQ
    asm { sei }
    // Enable DMA, Narrow Playfield - ANTIC Direct Memory Access Control
    ANTIC->DMACTL = 0x21; 
    // Set ANTIC Display List Pointer
    ANTIC->DLIST  = DISPLAY_LIST;
    // Set colors
    GTIA->COLPF0 = 0x28;
    GTIA->COLPF1 = 0x48;
    GTIA->COLPF2 = 0x80;
    GTIA->COLPF3 = 0xc8;
    
    // Loop forever - Display raster bars
    char col = 0;
    for(;;) {      
      while(ANTIC->VCOUNT!=40) ;
      char c = col++;
      for( char l=0;l<100;l++) {
        ANTIC->WSYNC = c;
        GTIA->COLBK = c;
        c++;
      }
      GTIA->COLBK = 0;
    };
}

// Message to show
char TEXT[] = "HELLO atari 8BIT"
              "Demonstrates ANTIC display list!"
            ;

// ANTIC Display List Program
// https://en.wikipedia.org/wiki/ANTIC
char DISPLAY_LIST[] = {
   BLANK8, BLANK8, BLANK8,              // 3* 8 blank lines
   LMS|MODE7, <TEXT, >TEXT,             // Load memory address and set to charmode 7 (16/20/24 chars wide, 16 lines per char)
   BLANK4,                              // 4 blank lines
   MODE2,                               // Charmode 2 (32/40/48 chars wide, 8 lines per char)
   JVB, <DISPLAY_LIST, >DISPLAY_LIST    // Wait for VBLANK and jump
};
