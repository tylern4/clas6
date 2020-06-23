
#include "dc3.h"
#include "tcl_procs.h"


extern unsigned char dc3_4bit_image_data[];
extern int N_dc3_4bit;

FILE *dc3_4bit_file;
char dc3_4bit_fname[256];

extern unsigned char dc3_image_data[];
extern int N_dc3;

FILE *dc3_file;
char dc3_fname[256];

extern "C" {
char* dc3_4bit_image_init(void);
char* dc3_image_init(void);
}


void SplashScreen(void)
{
	char *fname;
	char cmd[256];
	int bpp;

	/* Evaluate this now so splash_win Tcl variable is initialized */
	Tcl_Evaluate(logo_tcl_proc);

	/* Get the screen depth in bits per pixel so we can decide which image to show */
	bpp = Tk_Depth(Tk_MainWindow(interp));

	/* try to create a temporary image file */
	if(bpp<=8)
		fname=dc3_4bit_image_init();
	else
		fname=dc3_image_init();
		
	if(!fname)return;

	/* if successful, call Tcl proc to display */
	sprintf(cmd,"dc3_splash %s",fname);
	Tcl_Evaluate(cmd);

	/* Unlinking the file deletes it when all processes are done with it. */
	unlink(fname);
}


