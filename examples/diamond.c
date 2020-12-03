// Draws a diamond shape in graphics mode 7+16, color 1.

#pragma target(atarixl)
#pragma encoding(atascii)
#pragma zp_reserve(0x00..0x7f)

#define ICCOM_OPEN_CHANNEL          0x03;
#define ICCOM_PUT_BINARY_RECORD     0x0B;
#define ICCOM_CLOSE                 0x0C;
#define ICCOM_DRAW_LINE             0x11;

char * ROWCRS = 0x54;
word * COLCRS = 0x55;

char * const ATACHR = 0x2FB;
char * const CH = 0x2FC;

void * CIOV   = 0xE456;

struct ATARI_IOCB {
	char ICHID;
	char ICDNO;
	char ICCOM;
	char ICSTA;
	char* ICBA;
	char* ICPT;
	word ICBL;
	char ICAX1;
	char ICAX2;
	char ICAX3;
	char ICAX4;
	char ICAX5;
	char ICAX6;
};

struct ATARI_IOCB * const IOCB0 = 0x340;
struct ATARI_IOCB * const IOCB6 = 0x3A0;

char *sDrive = "S:";

void main() {
	graphics(7+16);
	color(1);
	plot(79, 0);
	drawTo(159, 47);
	drawTo( 79, 95);
	drawTo(  0, 47);
	drawTo( 79, 0);
	waitkey();
}

void closeChannel() {
	// for now, assume channel 6
	IOCB6->ICCOM = ICCOM_CLOSE;
	asm(clobbers "AXY") {
		ldx #$60 // iocb 6
		jmp CIOV  // let the rts from the CIOV return us too
	};
}

void graphics(char mode) {
	closeChannel();
	IOCB6->ICCOM = ICCOM_OPEN_CHANNEL;
	IOCB6->ICAX1 = 0x0C; // (IOCB_ICAX_READ | IOCB_ICAX_WRITE)
	IOCB6->ICAX2 = mode;
	IOCB6->ICBA  = sDrive;
	asm(clobbers "AXY") {
		ldx #$60
		jmp CIOV
	};
}

void color(char c) {
	*ATACHR = c;
}

void position(word x, char y) {
	*COLCRS = x;
	*ROWCRS = y;
}

void plot(word x, char y) {
	position(x, y);
	IOCB6->ICCOM = ICCOM_PUT_BINARY_RECORD;
	IOCB6->ICBL = 0;
	asm(clobbers "AXY") {
		ldx #$60
		lda ATACHR
		jmp CIOV
	};
}

void drawTo(word x, char y) {
	position(x, y);
	IOCB6->ICCOM = ICCOM_DRAW_LINE;
	IOCB6->ICAX1 = 0x0C; // (IOCB_ICAX_READ | IOCB_ICAX_WRITE)
	IOCB6->ICAX2 = 0;
	asm(clobbers "AXY") {
		ldx #$60
		jmp CIOV
	};
}

void waitkey() {
	while(!kbhit()) ;
	clrkb();
}

unsigned char kbhit() {
	if (*CH == 0xff) return 0; else return 1;
}

inline void clrkb() {
	*CH = 0xff;
}
