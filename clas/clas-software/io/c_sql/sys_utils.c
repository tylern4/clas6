#include <stdio.h>
#include <unistd.h>
#include <pwd.h>
#include <sys/types.h>
#include "csql.h"



void get_user_name(char *u_name){
  struct passwd *pw_ent;
  int            u_id;
  u_id   = getuid();
  pw_ent = getpwuid(u_id);
  strcpy(u_name,pw_ent->pw_name);
  return;
} 

void get_table_name(char *tbl_name, char *nodename, int max_len){
int    proc_id, j;
char   host_name[64];

 
 proc_id = getpid();
 
 /*====== Geting Host name ======*/
 
 if(gethostname(host_name,40) != 0){
   strcpy(nodename,"unknown");} else {
     strcpy(nodename,host_name); 
  }
  
  

 /*====== Separate Host name and domain name ====*/

 for(j = 0; j < strlen(nodename); j++)
   if(nodename[j] == '.'){ 
	 nodename[j]='\0'; 
	 break; 
   }
 
 
 /* strcpy(nodename,host_name);*/

 /*====== Creating table name as: =============
 //  mon_[hostname]_[processID] 
 //============================================*/
 /* strcpy(NodeName,host_name); */

 sprintf(tbl_name,"mon_%s_%d",nodename,proc_id);

 return;

}


void create_sys_group(char *jobname){

char uname[10];
char fnodename[32];


get_table_name(CSQL_MonTable,fnodename,32);

 add_group("SYST");
 get_user_name(uname);

 add_column("SYST","time");
 set_column_char("SYST","time",""); 
 init_column("SYST","time","TIMESTAMP(14)",20,COL_C_TBL);

 add_column("SYST","user");
 init_column("SYST","user","CHAR(12)",20,COL_CW_TBL);
 set_column_char("SYST","user",uname);
 
 add_column("SYST","jobname");
 init_column("SYST","jobname","CHAR(32)",20,COL_CW_TBL);
 set_column_char("SYST","jobname",jobname);

 add_column("SYST","node");
 init_column("SYST","node","CHAR(32)",20,COL_CW_TBL);
 set_column_char("SYST","node",fnodename);


/*
  //=======================================================================
  //  This piece is added to include Calibration database information
  // into tables. (requested by H.Avakian)
  // Added : 02/01/2002 by G.Gavalian
  //=======================================================================
*/

}

void create_sys_group_(char *jobname,int jb_len){

 char *jb_name;
 int   slen;

 slen = get_len(jobname,jb_len);

 if(slen>0){
   jb_name = (char *) malloc(slen+1);
   strncpy(jb_name,jobname,slen);
   jb_name[slen] = '\0';
   create_sys_group(jb_name);
   free(jb_name);
 } else {
   printf("CREATE_SYS_GROUP(WARNING): Bad Argument Passed for Job name..\n");
   printf("CREATE_SYS_GROUP(WARNING): Creating SYST group with default job name..\n");
   printf("CONTROL PRINTOUT: FORTLEN = (%d) SLEN(%d)\n",jb_len,slen);
   create_sys_group("PRODUCTION");
 }
 return;

}


int get_len(char *strp, int plen){
  int alen;
  alen = plen;
  while((strp[alen-1]==' ')&&(alen>0)) alen--;
  return alen;
}

int get_fort_len_(char *tt,int tlen){
int hlen;
hlen = get_len(tt,tlen);
printf("GET_LEN =  %d\n",hlen);
return;
}









