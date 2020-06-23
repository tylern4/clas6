//  event_monitor

//  analyzes events, creates hist

//  still to do
//    timed wait not working?

//  ejw, 21-dec-99


// for posix
#define _POSIX_SOURCE_ 1
#define __EXTENSIONS__


// for smartsockets
#include <rtworks/cxxipc.hxx>


// for et
extern "C" {
#include <et.h>
}


// system stuff
#include <fstream.h>
#include <iomanip.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <signal.h>
#include <string.h>
#include <stdarg.h>
#include <macros.h>
#include <time.h>
#include <ctype.h>
#include <pthread.h>


// for tcl
extern "C"{
#include "tcl.h"
}


// for CLAS ipc
#include <clas_ipc_prototypes.h>


// for bos
#define NBCS 800000
extern "C"{
#include <bosio.h>
#include <bosfun.h>
#include <bos.h>
#include <bcs.h>
}


// misc variables
Tcl_Interp *interp;  
static char *init_tcl_script  = NULL;
static char *application      = "clastest";
static char *session          = NULL;
static char *uniq_name        = "level3";
static char *unique_id        = (char *) malloc(64);
static int evt_from_file      = 0;
static char *input_file       = NULL;
static int max_event          = -1;
static int wait_time          = 100;    // msec
static int et_ok              = 0;
static int debug              = 0;
static int done               = 0;
static time_t start_time      = time(NULL);
static int descriptor;
static char temp[256];
static char filename[256];


// et stuff
static char *et_station_name        = "level3";
static et_openconfig openconfig;
static et_sys_id et_system_id;
static char et_filename[128];
static et_stat_id et_station_id;
static et_statconfig et_station_config;
static et_att_id et_attach_id;
static et_event *et_event_ptr;


// for hbook
static int no_global          = -1;
static char *global_sect      = "none";
static int reset_hist_bor     = 0;
static int no_archive         = 0;
static int no_snapshot        = 0;
static char *archive_dir      =".";
static time_t last_archive    = time(NULL);
static time_t last_reset      = 0;
static int archive_time       = 600;       // secs
static int archive_evt        = 1000000;   // events


// event counts, rates, etc.
static int last_run           = 0;
static double et_rate         = 0.;
static double proc_rate       = 0.;
static time_t last_time       = time(NULL);
static int last_nevet         = 0;
static int last_nevproc       = 0;
static time_t delta;


// fortran common block
extern "C" {
struct {
  long nevet;
  long nevproc;
  long nev_this_run;
  long nrun_proc;

  long current_run;
  long current_event;

  long no_cc;
  long no_dc;
  long no_ec;
  long no_sc;
  long no_st;

  long no_lac;
  long no_trig;
  long no_tg;
  long no_photon;
  long no_call;
  long no_l2;

  long no_scaler;
  long no_hist;
  long no_timeline;

  long event_length;
} event_monitor_ = {0,0,0,0, 0,0, 0,0,0,0,0,  0,0,0,0,0,0,  0,0,0, 0};
}


// prototypes
void decode_command_line(int argc, char **argv);
void init_tcl(void);
void init_et();
void connect_et();
void *control_thread(void *param);
void process_events(void);
void quit_callback(int sig);
void status_poll_callback(T_IPC_MSG msg);
void control_message_callback(T_IPC_CONN conn,
			      T_IPC_CONN_PROCESS_CB_DATA data,
			      T_CB_ARG arg);
void decode_hbook_id(char *cid, long *id, char *dir);
extern "C" {
int et2bos(long *evt, int *jw, char *list);
Tcl_Interp *tclinterp_init(void);
void tclipc_init(Tcl_Interp *interp);
char *strdupf(char *fstring, int len);
int insert_msg(char *name, char *facility, char *process, char *msgclass, 
	       int severity, char *status, int code, char *message);
}


// fortran prototypes
extern "C" {
void reset_hist_(char *flag, int len);
void hcdir_(char *dir, char *flag, int l1, int l2);
void hrput_(long *id, char *filename, char *flag, int len1, int len2);
void hbook1_(long*,char*,int*,float*,float*,float*,int);
void hbook2_(long*,char*,int*,float*,float*,int*,float*,float*,float*,int);
void hidall_(long*,int*);
void hgive_(long*,char*,int*,float*,float*,int*,float*,float*,int*,int*,int);
void hrdir_(int*,char*,int*,int);
void cltou_(char *flag, int len);
void dd2bos_(long *evlen, long *pdata, char *rt, int len);
}


// level3 prototypes
extern "C"{
void level3_init_(void);
void level3_packev_(int *ctl, int *data, int *len);
void level3_analyze_(void);
void level3_stop_(void);
void level3_done_(void);
long *find_bank_in_dd_(long *fevent, char *fbank, int *fnbank, int *fnwrds, int fblen);
char *strdupf(char *fortran_string, int len);
}


// tcl command prototypes
int tcl_help(ClientData clientdata, Tcl_Interp *interp, int argc, char **argv);
int tcl_reset_hist(ClientData clientdata, Tcl_Interp *interp, int argc, char **argv);
int tcl_hrput(ClientData clientdata, Tcl_Interp *interp, int argc, char **argv);
int tcl_hbook1(ClientData clientdata, Tcl_Interp *interp, int argc, char **argv);
int tcl_hbook2(ClientData clientdata, Tcl_Interp *interp, int argc, char **argv);
int tcl_hlist(ClientData clientdata, Tcl_Interp *interp, int argc, char **argv);
int tcl_ldir(ClientData clientdata, Tcl_Interp *interp, int argc, char **argv);
int tcl_quit(ClientData clientdata, Tcl_Interp *interp, int argc, char **argv);


// ref to IPC server (connection created later)
TipcSrv &server=TipcSrv::Instance();


//--------------------------------------------------------------------------


int main(int argc, char **argv) {

  int status;
  pthread_t t1;
  int r1=0;


  // synch with c i/o
  ios::sync_with_stdio();


  // decode command line
  decode_command_line(argc,argv);


  // get session name
  if(session==NULL)session="clasprod";

  
  // create unique_id from uniq_name and session name
  strcpy(unique_id,uniq_name);
  strcat(unique_id,"_");
  strcat(unique_id,getenv("HOST")+4);
  strcat(unique_id,"_");
  strcat(unique_id,session);
  

  // get Tcl interp, create tcl commands, link vars, process Tcl startup script, etc.
  interp=tclinterp_init();
  init_tcl();
  if(init_tcl_script!=NULL)Tcl_EvalFile(interp,init_tcl_script);
  
  
  // init ipc thread package
  thr_setconcurrency(thr_getconcurrency()+2);
  if(!TipcInitThreads()) {
    cerr << "Unable to init IPC thread package" << endl;
    exit(EXIT_FAILURE);
  }


  // set ipc parameters, connect to server, etc.
  ipc_set_application(application);
  ipc_set_user_status_poll_callback(status_poll_callback);
  ipc_set_quit_callback(quit_callback);
  status=ipc_init(unique_id,"level3");
  if(status<0) {
    cerr << "\n?Unable to connect to server..."
	 << "probably duplicate unique id\n"
	 << "   ...check for another level3 connected to "
	 << session << " using ipc_info\n"
	 << "   ...only one such process allowed!" << endl << endl;
    exit(EXIT_FAILURE);
  }
  server.SubjectSubscribe("evt_system",TRUE);
  server.Flush();


  // launch ipc thread
  if(pthread_create(&t1,NULL,control_thread,(void *)&r1)!=0) {
    cerr << "\nUnable to create ipc thread" << endl << endl;
    exit(EXIT_FAILURE);
  }


  // initialize bos
  bosInit(bcs_.iw,NBCS);


  // init et structures, etc;
  init_et();


  // create tcl_request callback
  tclipc_init(interp);
 
  
  // initialize hbook, packages
  level3_init_();


  // init ipc control callback, or open file
  if(evt_from_file==0) {
    ipc_set_control_message_callback(control_message_callback);
    server.Flush();
  } else {
    //  status=bosOpen(input_file,"r",&descriptor);
  }


  // post startup message
  sprintf(temp,"Process startup:    %15s  in application:  %s",unique_id,application);
  status=insert_msg("level3",unique_id,unique_id,"status",0,"START",0,temp);


  // flush output
  fflush(NULL);


  // process events
  process_events();


  // last eor
  if(event_monitor_.nev_this_run>0) {
    // nothing
    cout << "End of run       " << event_monitor_.current_run << endl;
  }


  // final package cleanup
  level3_done_();


  // close data source
  if(evt_from_file==0) {
    et_close(et_system_id);
  } else {
    //  bosClose(descriptor);
  }
  

  // last archive hist
  if(no_archive==0) {
    hcdir_("//PAWC"," ",6,1);
    sprintf(filename,"%s/%s_%s_%06d.hbook",archive_dir,session,uniq_name,
	    event_monitor_.current_run);
    long id=0;
    hrput_(&id, filename, "T", strlen(filename), 1);
  }


  // print stats
  if(evt_from_file!=0) {
    cout << "\n\n   level3 read events from file:  " << input_file << endl << endl;
  } else {
    cout << "\n\n   level3 received " << event_monitor_.nevet 
	 << " events from et" << endl << endl;
  }
  cout << "   Events processed:   " << event_monitor_.nevproc << endl
       << "   Runs processed:     " << event_monitor_.nrun_proc << endl << endl;
  
    
  // post shutdown message
  sprintf(temp,"Process shutdown:  %15s",unique_id);
  status=insert_msg("level3",unique_id,unique_id,"status",0,"STOP",0,temp);


  // close ipc and exit
  ipc_close();
  exit(EXIT_SUCCESS);
}


//--------------------------------------------------------------------------


void *control_thread(void *param) {

  while(done==0) {

    server.MainLoop(2.0);


    // calc event rates every 10 seconds
    delta=time(NULL)-last_time;
    if(delta>10) {
      et_rate =(double)(event_monitor_.nevet-last_nevet)/delta;
      proc_rate=(double)(event_monitor_.nevproc-last_nevproc)/delta;
      last_time=time(NULL);
      last_nevet=event_monitor_.nevet;
      last_nevproc=event_monitor_.nevproc;
    }      
  }


  return (void *)0;
}


//------------------------------------------------------------------------


void init_et() {
  

  et_ok=0;


  // create et file name, etc.
  sprintf(et_filename,"/tmp/et_sys_%s",session);
  et_open_config_init(&openconfig);


  // get et library name
  char *et_user_library     = (char *) malloc(64);
  strcpy(et_user_library,getenv("CLON_LIB"));
  strcat(et_user_library,"/et_user_library.so");


  // create station config in case no station exists
  et_station_config_init(&et_station_config);
  et_station_config_setblock(et_station_config,ET_STATION_BLOCKING);
  et_station_config_setselect(et_station_config,ET_STATION_SELECT_ALL);
  et_station_config_setuser(et_station_config,ET_STATION_USER_SINGLE);
  et_station_config_setrestore(et_station_config,ET_STATION_RESTORE_OUT);
  et_station_config_setcue(et_station_config,100);
  et_station_config_setprescale(et_station_config,1);
  et_station_config_setlib(et_station_config,et_user_library);
  et_station_config_setfunction(et_station_config,"et_mon_function");


  return;
}


//--------------------------------------------------------------------------


void connect_et() {
  
  int status;
  sigset_t sigblock;
  

  et_ok=0;

  
  // open et system
  if(et_open(&et_system_id,et_filename,openconfig)!=ET_OK)return;
  

  // create station if not already created
  status=et_station_create(et_system_id,&et_station_id,et_station_name,et_station_config);
  if((status!=ET_OK)&&(status!=ET_ERROR_EXISTS)) { 
      cout << status << endl;
      et_close(et_system_id);
      cerr << "Unable to create station " << et_station_name << endl;
      done=1;
      return;
  }
  
  
  // block signals to THIS thread and any thread created by this thread
  // needed to keep signals from et threads
  sigfillset(&sigblock);
  pthread_sigmask(SIG_BLOCK,&sigblock,NULL);
  
  
  // attach to station
  status=et_station_attach(et_system_id,et_station_id,&et_attach_id);
  if(status!=ET_OK) {
      et_close(et_system_id);
      cerr << "Unable to attach to station " << et_station_name << endl;
      done=1;
      return;
  }
  

  // unblock signals
  pthread_sigmask(SIG_UNBLOCK,&sigblock,NULL);


  // success
  et_ok=1; 
  cout << "...now connected to ET system: " << et_filename 
       << ",   station: " << et_station_name << endl;

  return;
}


//--------------------------------------------------------------------------


void process_events() {

  long *evt;
  int status,len;
  timespec wait_spec;


  while(done==0) {

    // clean bos common
    bosLdrop(bcs_.iw,"E");
    bosNgarbage(bcs_.iw);


    // get an event from et or file
    if(evt_from_file==0) {
      
      // check et system
      if(et_ok==0) {
	connect_et();
	if(et_ok==0) {
	  sleep(1);
	  continue;
	}
      } else if(et_alive(et_system_id)==0) {
	done=1;
	return;
      }	

      
      // et system ok...try to get event...sleep if none available
      wait_spec.tv_sec =(time_t)(wait_time/1000);
      wait_spec.tv_nsec=(long)((wait_time%1000)*1000000);
      status=et_event_get(et_system_id,et_attach_id,&et_event_ptr,ET_ASYNC,NULL);
      if (status==ET_OK) {
	event_monitor_.nevet++;
	et_event_getdata(et_event_ptr,(void**)&evt);
	//	et2bos(evt, bcs_.iw, "E");
			 
	et_event_getlength(et_event_ptr,&len);
 	event_monitor_.event_length=len/4;

	//	analyze_event();

	event_monitor_.nevproc++;
	event_monitor_.nev_this_run++;
	//	event_monitor_.current_run=bcs_.iw[head+1];
	//	event_monitor_.current_event=bcs_.iw[head+2];
	int llen=len/4;
	level3_packev_((int*)0,(int*)evt,&llen);
	level3_analyze_();
	
	// check if processed enough events
	if((max_event>0)&&(event_monitor_.nevproc>=max_event))done=1;
  
	et_event_put(et_system_id,et_attach_id,et_event_ptr);


      } else if((status==ET_ERROR_TIMEOUT)||(status==ET_ERROR_EMPTY)||(status==ET_ERROR_BUSY)) {
	/*	nanosleep(&wait_spec,NULL); */
	continue;

      } else {
	cerr << "?error return from et_event_get: " << status << endl;
	done=1;
	return;
      }


    } else {

      //      if(bosRead(descriptor,bcs_.iw,"E")!=0) {
      //	done=1;
      //	break;
      //      }
      event_monitor_.event_length=0.;
      // analyze_event();
    }

  }


  return;
}


//-------------------------------------------------------------------


void quit_callback(int sig) {
  
  // received signal or quit control command
  cout << "...stopping, received signal: " << sig << endl;
  done=1;
  
  return;
}


//----------------------------------------------------------------------


void status_poll_callback(T_IPC_MSG msg) {
  
  char p[27];

  
  TipcMsgAppendStr(msg,"Session");
  TipcMsgAppendStr(msg,session);
  TipcMsgAppendStr(msg,"application");
  TipcMsgAppendStr(msg,application);
  TipcMsgAppendStr(msg,"unique_id");
  TipcMsgAppendStr(msg,unique_id);

  TipcMsgAppendStr(msg,"evt_from_file");
  TipcMsgAppendInt4(msg,evt_from_file);
  if(evt_from_file==1) {
    TipcMsgAppendStr(msg,"input_file");
    TipcMsgAppendStr(msg,input_file);
  }

  TipcMsgAppendStr(msg,"");
  TipcMsgAppendStr(msg,"");

  TipcMsgAppendStr(msg,"start_time");
  strcpy(p,ctime(&start_time)); *strchr(p,'\n')='\0';
  TipcMsgAppendStr(msg,p);
  TipcMsgAppendStr(msg,"current_run");
  TipcMsgAppendInt4(msg,event_monitor_.current_run);
  TipcMsgAppendStr(msg,"current_event");
  TipcMsgAppendInt4(msg,event_monitor_.current_event);
  TipcMsgAppendStr(msg,"last_run");
  TipcMsgAppendInt4(msg,last_run);
  TipcMsgAppendStr(msg,"last_archive");
  strcpy(p,ctime(&last_archive)); *strchr(p,'\n')='\0';
  TipcMsgAppendStr(msg,p);
  TipcMsgAppendStr(msg,"last_reset");
  strcpy(p,ctime(&last_reset)); *strchr(p,'\n')='\0';
  TipcMsgAppendStr(msg,p);

  TipcMsgAppendStr(msg,"");
  TipcMsgAppendStr(msg,"");

  TipcMsgAppendStr(msg,"nevet");
  TipcMsgAppendInt4(msg,event_monitor_.nevet);
  TipcMsgAppendStr(msg,"nevproc");
  TipcMsgAppendInt4(msg,event_monitor_.nevproc);
  TipcMsgAppendStr(msg,"nev_this_run");
  TipcMsgAppendInt4(msg,event_monitor_.nev_this_run);
  TipcMsgAppendStr(msg,"nrun_proc");
  TipcMsgAppendInt4(msg,event_monitor_.nrun_proc);

  TipcMsgAppendStr(msg,"");
  TipcMsgAppendStr(msg,"");

  if(init_tcl_script!=NULL) {
    TipcMsgAppendStr(msg,"init_tcl_script");
    TipcMsgAppendStr(msg,init_tcl_script);
  }
  TipcMsgAppendStr(msg,"no_global");
  TipcMsgAppendInt4(msg,no_global);
  TipcMsgAppendStr(msg,"global_sect");
  TipcMsgAppendStr(msg,global_sect);
  TipcMsgAppendStr(msg,"archive_time");
  TipcMsgAppendInt4(msg,archive_time);
  TipcMsgAppendStr(msg,"archive_evt");
  TipcMsgAppendInt4(msg,archive_evt);
  TipcMsgAppendStr(msg,"max_event");
  TipcMsgAppendInt4(msg,max_event);
  TipcMsgAppendStr(msg,"wait_time");
  TipcMsgAppendInt4(msg,wait_time);
  TipcMsgAppendStr(msg,"reset_hist_bor");
  TipcMsgAppendInt4(msg,reset_hist_bor);
  TipcMsgAppendStr(msg,"no_archive");
  TipcMsgAppendInt4(msg,no_archive);
  TipcMsgAppendStr(msg,"archive_dir");
  TipcMsgAppendStr(msg,archive_dir);
  TipcMsgAppendStr(msg,"no_snapshot");
  TipcMsgAppendInt4(msg,no_snapshot);
  TipcMsgAppendStr(msg,p);
  TipcMsgAppendStr(msg,"debug");
  TipcMsgAppendInt4(msg,debug);

  TipcMsgAppendStr(msg,"");
  TipcMsgAppendStr(msg,"");

  TipcMsgAppendStr(msg,"no_cc");
  TipcMsgAppendInt4(msg,event_monitor_.no_cc);
  TipcMsgAppendStr(msg,"no_ec");
  TipcMsgAppendInt4(msg,event_monitor_.no_ec);
  TipcMsgAppendStr(msg,"no_sc");
  TipcMsgAppendInt4(msg,event_monitor_.no_sc);
  TipcMsgAppendStr(msg,"no_dc");
  TipcMsgAppendInt4(msg,event_monitor_.no_dc);
  TipcMsgAppendStr(msg,"no_lac");
  TipcMsgAppendInt4(msg,event_monitor_.no_lac);
  TipcMsgAppendStr(msg,"no_st");
  TipcMsgAppendInt4(msg,event_monitor_.no_st);
  TipcMsgAppendStr(msg,"no_tg");
  TipcMsgAppendInt4(msg,event_monitor_.no_tg);
  TipcMsgAppendStr(msg,"no_trig");
  TipcMsgAppendInt4(msg,event_monitor_.no_trig);
  TipcMsgAppendStr(msg,"no_photon");
  TipcMsgAppendInt4(msg,event_monitor_.no_photon);
  TipcMsgAppendStr(msg,"no_call");
  TipcMsgAppendInt4(msg,event_monitor_.no_call);
  TipcMsgAppendStr(msg,"no_l2");
  TipcMsgAppendInt4(msg,event_monitor_.no_l2);
  TipcMsgAppendStr(msg,"no_scaler");
  TipcMsgAppendInt4(msg,event_monitor_.no_scaler);
  TipcMsgAppendStr(msg,"no_timeline");
  TipcMsgAppendInt4(msg,event_monitor_.no_timeline);
  TipcMsgAppendStr(msg,"no_hist");
  TipcMsgAppendInt4(msg,event_monitor_.no_hist);

  return;
}


//--------------------------------------------------------------------------


void control_message_callback(T_IPC_CONN conn,
			      T_IPC_CONN_PROCESS_CB_DATA data,
			      T_CB_ARG arg) {

  T_STR string;


  // get first string
  TipcMsgSetCurrent(data->msg,0);
  TipcMsgNextStr(data->msg,&string);
  
  
  //  evt status request
  if(strcasecmp(string,"evt_status_poll")==0) {

    T_STR srvnode = server.Node();

    TipcMsg status("evt_status");
    status.Dest("/evt_system/status");
    status.Sender(unique_id);
    sprintf(temp,"et:%s",et_station_name);
    status << unique_id << getenv("HOST") << session << srvnode 
	   << (T_INT4) event_monitor_.nevet << et_rate 
	   << (T_INT4) event_monitor_.nevproc << proc_rate
	   << (T_INT4) et_ok << temp;
    server.Send(status,TRUE);
    server.Flush();


  //  don't understand message...ship to smartsockets interpreter
  } else {
    TutCommandParseStr(string);
  }

  return;
}


//----------------------------------------------------------------------


void decode_command_line(int argc, char**argv) {
  
  char *help = 
    "\nusage:\n\n  level3 [-a application] [-s session] [-u uniq_name] [-g global_sect] [-i input_file]\n"
    "          [-stat et_station_name] [-adir archive_dir] [-at archive_time] [-ae archive_evt\n"
    "          [-m max_event] [-r(eset_hist_bor)] [-no_archive] [-no_snapshot] [-wait wait_time] [-debug]\n"
    "          [-no_cc] [-no_dc] [-no_ec] [-no_sc] [-no_st] [-no_lac]\n"
    "          [-no_trig] [-no_tg] [-no_photon] [-no_call] [-no_l2] [-no_scaler] [-no_timeline] [-no_hist]\n";
  
  
  
  // loop over all arguments, except the 1st (which is program name)
  int i=1;
  while (i<argc) 
    {
      if (strncasecmp(argv[i],"-h",2)==0)
	{
	  cout << help << endl;
	  exit(EXIT_SUCCESS);
	}
      else if (strncasecmp(argv[i],"-debug",6)==0) {
	debug=1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-no_archive",11)==0) {
	no_archive=1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-no_snapshot",12)==0) {
	no_snapshot=1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-adir",5)==0) {
	archive_dir=strdup(argv[i+1]);
	i=i+2;
      }
      else if (strncasecmp(argv[i],"-stat",5)==0) {
	et_station_name=strdup(argv[i+1]);
	i=i+2;
      }
      else if (strncasecmp(argv[i],"-no_cc",6)==0) {
	event_monitor_.no_cc=-1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-no_dc",6)==0) {
	event_monitor_.no_dc=-1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-no_ec",6)==0) {
	event_monitor_.no_ec=-1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-no_sc",6)==0) {
	event_monitor_.no_sc=-1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-no_st",6)==0) {
	event_monitor_.no_st=-1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-no_lac",7)==0) {
	event_monitor_.no_lac=-1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-no_trig",8)==0) {
	event_monitor_.no_trig=-1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-no_tg",6)==0) {
	event_monitor_.no_tg=-1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-no_photon",10)==0) {
	event_monitor_.no_photon=-1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-no_call",8)==0) {
	event_monitor_.no_call=-1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-no_l2",6)==0) {
	event_monitor_.no_l2=-1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-no_scaler",10)==0) {
	event_monitor_.no_scaler=-1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-no_timeline",12)==0) {
	event_monitor_.no_timeline=-1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-no_hist",7)==0) {
	event_monitor_.no_hist=-1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-wait",5)==0) {
	wait_time=atoi(argv[i+1]);
	i=i+2;
      }
      else if (strncasecmp(argv[i],"-at",3)==0) {
	archive_time=atoi(argv[i+1]);
	i=i+2;
      }
      else if (strncasecmp(argv[i],"-ae",3)==0) {
	archive_evt=atoi(argv[i+1]);
	i=i+2;
      }
      else if (strncasecmp(argv[i],"-a",2)==0) {
	application=strdup(argv[i+1]);
	i=i+2;
      }
      else if (strncasecmp(argv[i],"-s",2)==0) {
	session=strdup(argv[i+1]);
	i=i+2;
      }
      else if (strncasecmp(argv[i],"-u",2)==0) {
	uniq_name=strdup(argv[i+1]);
	i=i+2;
      }
      else if (strncasecmp(argv[i],"-t",2)==0) {
	init_tcl_script=strdup(argv[i+1]);
	i=i+2;
      }
      else if (strncasecmp(argv[i],"-m",2)==0) {
	max_event=atoi(argv[i+1]);
	i=i+2;
      }
      else if (strncasecmp(argv[i],"-i",2)==0) {
	evt_from_file=1;
	input_file=strdup(argv[i+1]);
	i=i+2;
      }
      else if (strncasecmp(argv[i],"-g",2)==0) {
	no_global=0;
	global_sect=strdup(argv[i+1]);
	i=i+2;
      }
      else if (strncasecmp(argv[i],"-r",2)==0) {
	reset_hist_bor=1;
	i=i+1;
      }
      else if (strncasecmp(argv[i],"-",1)==0) {
	cout << "Unknown command line arg: " << argv[i] 
	     << argv[i+1] << endl << endl;
	i=i+2;
      }
    }
  
  return;
}


//-----------------------------------------------------------------


void init_tcl() {
  
  
  // link c and Tcl variables
  Tcl_LinkVar(interp,"application",     (char *)&application,     TCL_LINK_STRING);
  Tcl_LinkVar(interp,"session",         (char *)&session,         TCL_LINK_STRING);
  Tcl_LinkVar(interp,"uniq_name",  	(char *)&uniq_name,    	  TCL_LINK_STRING);  
  Tcl_LinkVar(interp,"unique_id",       (char *)&unique_id,       TCL_LINK_STRING);
  Tcl_LinkVar(interp,"evt_from_file",   (char *)&evt_from_file,   TCL_LINK_INT);
  Tcl_LinkVar(interp,"archive_time",  	(char *)&archive_time,     TCL_LINK_INT);
  Tcl_LinkVar(interp,"archive_evt",  	(char *)&archive_evt,      TCL_LINK_INT);
  Tcl_LinkVar(interp,"max_event",    	(char *)&max_event,       TCL_LINK_INT);
  Tcl_LinkVar(interp,"wait_time",    	(char *)&wait_time,       TCL_LINK_INT);
  Tcl_LinkVar(interp,"init_tcl_script",	(char *)&init_tcl_script, TCL_LINK_STRING);
  Tcl_LinkVar(interp,"input_file",	(char *)&input_file,	  TCL_LINK_STRING);   
  Tcl_LinkVar(interp,"no_global",       (char *)&no_global,       TCL_LINK_INT);
  Tcl_LinkVar(interp,"global_sect",     (char *)&global_sect,     TCL_LINK_STRING);
  Tcl_LinkVar(interp,"reset_hist_bor",  (char *)&reset_hist_bor,  TCL_LINK_BOOLEAN);
  Tcl_LinkVar(interp,"archive_dir",     (char *)&archive_dir,     TCL_LINK_STRING);
  Tcl_LinkVar(interp,"no_archive", 	(char *)&no_archive,	  TCL_LINK_INT);
  Tcl_LinkVar(interp,"no_snapshot",	(char *)&no_snapshot,	  TCL_LINK_INT);
  Tcl_LinkVar(interp,"debug",           (char *)&debug,           TCL_LINK_BOOLEAN);

  Tcl_LinkVar(interp,"no_cc",  	   (char *)&event_monitor_.no_cc, 	  TCL_LINK_INT);
  Tcl_LinkVar(interp,"no_ec",  	   (char *)&event_monitor_.no_ec, 	  TCL_LINK_INT);
  Tcl_LinkVar(interp,"no_sc",  	   (char *)&event_monitor_.no_sc, 	  TCL_LINK_INT);
  Tcl_LinkVar(interp,"no_dc",      (char *)&event_monitor_.no_dc, 	  TCL_LINK_INT);
  Tcl_LinkVar(interp,"no_lac", 	   (char *)&event_monitor_.no_lac,	  TCL_LINK_INT);
  Tcl_LinkVar(interp,"no_st",  	   (char *)&event_monitor_.no_st, 	  TCL_LINK_INT);
  Tcl_LinkVar(interp,"no_tg",  	   (char *)&event_monitor_.no_tg, 	  TCL_LINK_INT);
  Tcl_LinkVar(interp,"no_trig",	   (char *)&event_monitor_.no_trig,   	  TCL_LINK_INT);
  Tcl_LinkVar(interp,"no_photon",  (char *)&event_monitor_.no_photon,   	  TCL_LINK_INT);
  Tcl_LinkVar(interp,"no_call",	   (char *)&event_monitor_.no_call,   	  TCL_LINK_INT);
  Tcl_LinkVar(interp,"no_l2",	   (char *)&event_monitor_.no_l2,   	  TCL_LINK_INT);
  Tcl_LinkVar(interp,"no_scaler",  (char *)&event_monitor_.no_scaler, 	  TCL_LINK_INT);
  Tcl_LinkVar(interp,"no_timeline",(char *)&event_monitor_.no_timeline,	  TCL_LINK_INT);
  Tcl_LinkVar(interp,"no_hist",	   (char *)&event_monitor_.no_hist,   	  TCL_LINK_INT);

  Tcl_LinkVar(interp,"nevet",       (char *)&event_monitor_.nevet, TCL_LINK_INT||TCL_LINK_READ_ONLY);
  Tcl_LinkVar(interp,"nevproc",     (char *)&event_monitor_.nevproc,TCL_LINK_INT||TCL_LINK_READ_ONLY);
  Tcl_LinkVar(interp,"nrun_proc",   (char *)&event_monitor_.nrun_proc,TCL_LINK_INT||TCL_LINK_READ_ONLY);
  Tcl_LinkVar(interp,"nev_this_run",(char *)&event_monitor_.nev_this_run,TCL_LINK_INT||TCL_LINK_READ_ONLY);

  Tcl_LinkVar(interp,"last_run",     	(char *)&last_run,        TCL_LINK_INT||TCL_LINK_READ_ONLY);
  Tcl_LinkVar(interp,"start_time", 	(char *)&start_time,      TCL_LINK_INT||TCL_LINK_READ_ONLY);
  Tcl_LinkVar(interp,"last_archive", 	(char *)&last_archive,   TCL_LINK_INT||TCL_LINK_READ_ONLY);
  Tcl_LinkVar(interp,"last_reset",  	(char *)&last_reset,      TCL_LINK_INT||TCL_LINK_READ_ONLY);
  Tcl_LinkVar(interp,"current_run",     (char *)&event_monitor_.current_run,  TCL_LINK_INT||TCL_LINK_READ_ONLY);
  Tcl_LinkVar(interp,"current_event",   (char *)&event_monitor_.current_event,TCL_LINK_INT||TCL_LINK_READ_ONLY);


  // define functions
  Tcl_CreateCommand(interp,"help",tcl_help,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"reset",tcl_reset_hist,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"hrput",tcl_hrput,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"hbook1",tcl_hbook1,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"hbook2",tcl_hbook2,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"hlist",tcl_hlist,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"ldir",tcl_ldir,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"quit",tcl_quit,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"exit",tcl_quit,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"stop",tcl_quit,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

  return;
}


//--------------------------------------------------------------------------


int tcl_hbook1(ClientData clientdata, Tcl_Interp *interp,
		int argc, char **argv) {

  char dir[256];

  long id;
  int nx;
  float xlow;
  float xhigh;
  float vmx;


  // check for enough args 
  if(argc<7) {
    interp->result="Usage:   hbook1 id title nx xlow xhigh vmx \n\n"
                   "  id formats:  id   subdir/id   /subdir/id   //dir/subdir/id\n";
    return (TCL_OK);
  }


  // decode id spec 
  decode_hbook_id(argv[1],&id,dir);


  // change to specified directory 
  hcdir_(dir," ",strlen(dir),1);


  // get remaining args 
  sscanf(argv[3],"%d",&nx);
  sscanf(argv[4],"%f",&xlow);
  sscanf(argv[5],"%f",&xhigh);
  sscanf(argv[6],"%f",&vmx);


  // rebin hist 
  hbook1_(&id,argv[2],&nx,&xlow,&xhigh,&vmx,strlen(argv[2]));


  // change dir back to PAWC 
  hcdir_("//PAWC"," ",6,1);


  return (TCL_OK);
}


//---------------------------------------------------------------------


int tcl_hbook2(ClientData clientdata, Tcl_Interp *interp,
		int argc, char **argv) {

  char dir[256];

  long id;
  int nx;
  float xlow;
  float xhigh;
  int ny;
  float ylow;
  float yhigh;
  float vmx;


  // check for enough args 
  if(argc<10) {
    interp->result="Usage:   hbook2 id title nx xlow xhigh ny ylow yhigh vmx \n\n"
                   "  id formats:  id   subdir/id   /subdir/id   //dir/subdir/id\n";
    return (TCL_OK);
  }


  // decode id spec 
  decode_hbook_id(argv[1],&id,dir);


  // change to specified directory 
  hcdir_(dir," ",strlen(dir),1);


  // get remaining args 
  sscanf(argv[3],"%d",&nx);
  sscanf(argv[4],"%f",&xlow);
  sscanf(argv[5],"%f",&xhigh);
  sscanf(argv[6],"%d",&ny);
  sscanf(argv[7],"%f",&ylow);
  sscanf(argv[8],"%f",&yhigh);
  sscanf(argv[9],"%f",&vmx);


  // rebin hist 
  hbook2_(&id,argv[2],&nx,&xlow,&xhigh,&ny,&ylow,&yhigh,&vmx,strlen(argv[2]));


  // change dir back to PAWC 
  hcdir_("//PAWC"," ",6,1);


  return (TCL_OK);
}


//---------------------------------------------------------------------


int tcl_hlist(ClientData clientdata, Tcl_Interp *interp,
		int argc, char **argv) {

  long *idlist;
  char *dir;
  char *ftitle;
  char begin[32];
  int nhist;
  int i;
  long id;

  char *title;
  int nx,ny;
  float xlo,ylo,xhi,yhi;
  int nwt,loc;
  long tempid;


  // check for enough args 
  if(argc<2) {
    interp->result="Usage:   hlist id \n\n"
                   "  id formats:  id   subdir/id   /subdir/id   //dir/subdir/id\n";
    return (TCL_OK);
  }


  // get some memory 
  idlist  = (long *)malloc(4*256);
  dir     = (char *)malloc(256);
  ftitle  = (char *)malloc(85);


  // decode id spec 
  decode_hbook_id(argv[1],&id,dir);


  // change to specified directory 
  hcdir_(dir," ",strlen(dir),1);


  // get list of hist id's 
  if(id!=0) {
    nhist=1;
    idlist[0]=id;
  } else {
    hidall_(idlist,&nhist);
  }


  // get hist info 
  Tcl_AppendResult(interp,"\n\n    ID     Title\n    --     -----\n\n",(char *)NULL);
  if(nhist>0) {
    for (i=0; i<nhist; i++) {
      tempid=idlist[i];
      hgive_(&tempid,ftitle,&nx,&xlo,&xhi,&ny,&ylo,&yhi,&nwt,&loc,80);
      title=strdupf(ftitle,80);
      if(strlen(title)<=0)strcpy(title," does not exist!");
      sprintf(begin,"%6i     ",idlist[i]);
      Tcl_AppendResult(interp,begin,title,"\n",(char *)NULL);
      free(title);
    }
  } else {
    Tcl_AppendResult(interp,"Illegal dir or no hist in: ",dir,(char *)NULL);
  }
  Tcl_AppendResult(interp,"\n",(char *)NULL);


  // change dir back to PAWC 
  hcdir_("//PAWC"," ",6,1);


  // free memory 
  free(idlist);
  free(dir);
  free(ftitle);


  return (TCL_OK);
}


//---------------------------------------------------------------------


int tcl_hrput(ClientData clientdata, Tcl_Interp *interp,
		int argc, char **argv) {

  char dir[256];
  long id=0;
  char *space=" ";
  char *flag=space;
  

  // check for enough args 
  if(argc<3) {
    interp->result="Usage:   hrput id file [T]\n\n"
                   "  id formats:  id   subdir/id   /subdir/id   //dir/subdir/id\n";
    return (TCL_OK);
  }


  // check for optional flag, convert to upper case 
  if(argc>3) {
    flag=argv[3];
    cltou_(flag,1);
  }


  // decode id spec 
  decode_hbook_id(argv[1],&id,dir);


  // change to specified directory 
  hcdir_(dir," ",strlen(dir),1);


  // write out hist 
  hrput_(&id,argv[2],flag,strlen(argv[2]),1);

    
  // change dir back to PAWC 
  hcdir_("//PAWC"," ",6,1);


  return (TCL_OK);
}


//---------------------------------------------------------------------


int tcl_ldir(ClientData clientdata, Tcl_Interp *interp,
		int argc, char **argv) {

  char *dir,*cdir;
  char *fdirlist;
  char *thisdir;
  long id;
  int nd = 100;
  int ndir;
  int i;


  // get some memory 
  dir     = (char *)malloc(64);
  cdir    = (char *)malloc(64);
  fdirlist = (char *)malloc(6400);


  // decode id spec if specified 
  if(argc>1) {
    decode_hbook_id(argv[1],&id,dir);
  } else {
    strcpy(dir,"//PAWC");
  }


  // change to specified directory and see if it worked
  hcdir_(dir," ",strlen(dir),1);
  hcdir_(cdir,"R",64,1);

  // get and  process dir list if it worked 
  if(strncasecmp(dir,cdir,strlen(dir))==0) {

    // get list of sub-dirs 
    hrdir_(&nd,fdirlist,&ndir,64);
    Tcl_AppendResult(interp,"\nList of sub-dirs in ",dir,":\n\n",(char *)NULL);
    for (i=0; i<ndir; i++) {
      thisdir=strdupf((fdirlist+i*64),64);
      Tcl_AppendResult(interp,"      ",thisdir,"\n",(char *)NULL);
      free(thisdir);
    }

  } else {
      Tcl_AppendResult(interp,"?no such dir: ",dir,"\n",(char *)NULL);
  }


  // change dir back to PAWC 
  hcdir_("//PAWC"," ",6,1);


  // free memory 
  free(dir);
  free(cdir);
  free(fdirlist);


  return (TCL_OK);
}


//---------------------------------------------------------------------


int tcl_quit(ClientData clientdata, Tcl_Interp *interp,
		int argc, char **argv) {


  quit_callback(0);
  return (TCL_OK);
}


//---------------------------------------------------------------------


int tcl_reset_hist(ClientData clientdata, Tcl_Interp *interp,
		int argc, char **argv) {

  char *flagf;
  long flen;
  int i;

  // check for args 
  if(argc<=1) {
    interp->result="Usage:   reset id1 [id2 ...]\n\n   id formats:  id   subdir/id   /subdir/id   //dir/subdir/id\n";
    return (TCL_OK);
  }


  // loop over each arg 
  for(i=1; i<argc; i++) {

    // copy string and get length 
    flagf=strdup(argv[i]);
    flen=strlen(flagf);
    
    // call fortran routine 
    reset_hist_(flagf,flen);
    
    // free flagf 
    free(flagf);

  }

  return (TCL_OK);

}


//---------------------------------------------------------------------


int tcl_help(ClientData clientdata, Tcl_Interp *interp,
		int argc, char **argv){

  char *help =
    "\nTcl commands available in the level3 program:\n\n"
    " help                  print this message\n"
    " hlist id              list histogram information\n"
    " reset id1 [id2 ...]   reset histograms\n"
    " hrput id file [T]     write hist to file\n"
    " hbook1 [many args]    1-d hist rebinning \n"
    " hbook2 [many args]    2-d hist rebinning \n"
    " hlist id              list hist id's\n"
    " ldir dir              list subdir's of dir (default is //PAWC)'s\n"
    " stop                  stop monitor program\n"
    " quit                  stop monitor program\n"
    " exit                  stop monitor program\n"
    "\n\n Type command that require args with NO args for more information\n"
    "\n";

  Tcl_SetResult(interp,help,TCL_STATIC);

  return (TCL_OK);

}


//---------------------------------------------------------------------


void decode_hbook_id(char *cid, long *id, char *dir) {


  int last_slash=0;
  int id_exists=0;
  char *idp;
  char *dp;


  /* find last slash in cid, set id pointer, and decode id...0 if no id */
  *id=0;
  for(last_slash=strlen(cid); (last_slash>=0)&&((cid+last_slash)[0]!='/'); last_slash--);
  idp=cid+last_slash+1;
  if( ((strlen(idp)>0)&&(isdigit(*idp)!=0)) || 
      ((strlen(idp)>1)&&(strncmp(idp,"-",1)==0)&&(isdigit(*(idp+1))!=0)) ) {
    id_exists=1;
    sscanf(idp,"%d",id);
  }
  

  /* returned dir always start with //pawc */
  strcat(dir,"//PAWC");


  /* null, //, //pawc, //pawc/, id */
  if(
     (strlen(cid)<=0)         	   	    ||
     (strcmp(cid,"//")==0)    	   	    ||  
     (strcasecmp(cid,"//pawc")==0) 	    ||    
     (strcasecmp(cid,"//pawc/")==0)         || 
     ((last_slash<0)&&(id_exists!=0)) )
    return;


  /* more than just //pawc, add slash */
  strcat(dir,"/");


  /* get pointer to after //pawc stuff and reset last_slash */
  dp=cid;
  if(strncasecmp(dp,"//PAWC",6)==0){
    dp=cid+6;
    last_slash-=6;
  }

  /* move dir pointer if leading slash */
  if(strncmp(dp,"/",1)==0)dp++;


  /* append remainder excluding id */
  if(id_exists==0){
    strcat(dir,dp);
  } else {
    strncat(dir,dp,last_slash);
  }


  /* chop off trailing / if it exists */
  if(dir[strlen(dir)-1]=='/')dir[strlen(dir)-1]='\0';


  /* convert dir to upper case */
  cltou_(dir,strlen(dir));


  return;

}


//------------------------------------------------------------------------


unsigned long *find_bank_in_dd(long event[], char *bank, int nbank, int &nwrds) {


  char *name;
  int nbnk,ncols,nrows;

  int ind   = 11;
  int evlen = event[10] + 11;


  // search all banks in DD event

  while (ind<evlen) {
    
    name  = (char *)&event[ind+1];
    nbnk  = event[ind+3];
    ncols = event[ind+4];
    nrows = event[ind+5];
    nwrds = event[ind+8];


    // index of 1st data word
    ind  += event[ind];
    
    // return pointer to data words if bank found (check name AND number)
    if ((strncmp(name,bank,strlen(bank))==0) && (nbank==nbnk)) return(unsigned long *)&event[ind];
    
    // index of header of next bank
    ind += nwrds;
  }
  

  // didn't find the bank
  return (unsigned long *)0;

}


//-------------------------------------------------------------------


//  fortran interface, returns array element, not pointer
long *find_bank_in_dd_(long *fevent, char *fbank, int *fnbank, int *fnwrds, int fblen) {

  int nwrds;
  long *bankptr;

  char *bank=strdupf(fbank,fblen);
  bankptr=(long *)find_bank_in_dd(fevent, bank, *fnbank, nwrds);
  free(bank);

  if(bankptr!=0) {
    *fnwrds=nwrds;
    return((long *)(bankptr-fevent));
  } else {
    *fnwrds=0;
    return((long *)0);
  }
}


//-------------------------------------------------------------------
