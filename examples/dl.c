#pragma target(atarixl)
#pragma zp_reserve(0x00..0x7f)

#include <atari-xl.h>
#include <printf.h>
#include <conio.h>

char * const ramto = 0x6a;
char * const colorpm0 = 704;
char * const colorpm1 = 705;
char * const colorpm2 = 706;
char * const colorpm3 = 707;

const char pmdata[] = { 255,129,129,129,129,129,129,255};

void main() {
  word i, pmgmem, zahl;
  char j, px0 , py0;

  *SDMCTL = 45; // 0x2D = 0x21 + PM enable
  *SDLST = DISPLAY_LIST;

  px0 = 100;
  py0 = 70;

  j = *ramto-8;
  ANTIC->PMBASE = j;

  pmgmem = j * 256;

  GTIA->GRACTL = 3;
  GTIA->SIZEP0 = 0;
  *colorpm0 = 183;
  GTIA->HPOSP0 = px0 ;

  char *pmadr = pmgmem + 512 + py0;

  pmadr[0] = pmdata[0];
  pmadr[1] = pmdata[1];
  pmadr[2] = pmdata[2];
  pmadr[3] = pmdata[3];
  pmadr[4] = pmdata[4];
  pmadr[5] = pmdata[5];
  pmadr[6] = pmdata[6];
  pmadr[7] = pmdata[7];

  waitkey();
}

void waitkey() {
    while(!kbhit()) ;
    clrkb();
}

char TEXT[] = "hello atari 8bit"
              "Demonstrates ANTIC display list "
              "HELLO atari 8BIT"
            ;

char DISPLAY_LIST[] = {
   BLANK8, BLANK8, BLANK8,
   LMS|MODE7, <TEXT, >TEXT,
   BLANK8,BLANK8,
   MODE2,
   BLANK8,BLANK8,
   MODE7,
   JVB, <DISPLAY_LIST, >DISPLAY_LIST
};
