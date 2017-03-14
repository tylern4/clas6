
#include "dc3.h"

#ifdef VERSION
char VERSIONSTR[]=VERSION;
#else
char VERSION[]="unkown";
char VERSIONSTR[]="unkown";
#endif

#ifndef COMPILE_USER
char COMPILE_USER[]="unkown";
#endif
#ifndef COMPILE_HOST
char COMPILE_HOST[]="unkown";
#endif

#ifdef DC_DCH_VERSION_MAJOR_h
int DC3_DCH_VERSION_MAJOR = DC_DCH_VERSION_MAJOR_h;
int DC3_DCH_VERSION_MINOR = DC_DCH_VERSION_MINOR_h;
#endif

void PrintVersion(void)
{
   char str[256],fname[256],*ptr;
   int i;

   /*-------------- Print version info etc --------*/
   cout<<"\n--------------- dc3--------------- \n";
   cout<<"Version: "<<ansi_bold<<VERSION<<ansi_normal<<"\n";
   cout<<"libdc Version: ";
#ifdef DC_DCH_VERSION_MAJOR_h
	cout<<DC_DCLIB_VERSION_MAJOR<<"."<<DC_DCLIB_VERSION_MINOR<<"\n";
#else
	cout<<"Not available\n";
#endif
   cout<<"dc.h Versions: ";
#ifdef DC_DCH_VERSION_MAJOR_h
	cout<<DC_DCH_VERSION_MAJOR<<"."<<DC_DCH_VERSION_MINOR<<"(dclib) ";
	cout<<DC3_DCH_VERSION_MAJOR<<"."<<DC3_DCH_VERSION_MINOR<<"(dc3)\n";
#else
	cout<<"Not available\n";
#endif
   cout<<"Compiled on: "<<ansi_red<<__DATE__<<" "<<__TIME__<<ansi_normal;
   cout<<" by: "<<ansi_blue<<COMPILE_USER<<ansi_normal;
   cout<<" on "<<ansi_green<<COMPILE_HOST<<ansi_normal<<"\n\n";


}

void Usage(void)
{
   cerr<<"\nUsage:\n";
   cerr<<"\t dc3 [options] [file.hbook]\n\n";
   cerr<<"\t file.hbook should be a file produced by \"trk_mon\".\n";
   cerr<<"\n";
   cerr<<"\t options are:\n";
   cerr<<"\t -h            Print this message\n";
   cerr<<"\t -v            Print version info\n";
   cerr<<"\t -twtype=type  Override the timewalk function type stored in\n";
	cerr<<"\t               the file with this one (use CAUTIOUSLY!!)\n";
   cerr<<"\n\n";

}

