
#include "dc3.h"


/* routines required by dc_timewalk which are normally supplied */
/* by recsis */

void recmes_(char *crname,char *flag,char *mess
   ,int len_crname,int len_flag,int len_mess)
{
   char str[512],*ptr;
   
   bzero(str,256);
   
   strncpy(str,crname,len_crname);
   if(strlen(str)){
      ptr=&str[strlen(str)-1];
      while(ptr>str && isspace(*ptr))*(ptr--)=0;
   }
   strcat(str,":");
   strncpy(&str[strlen(str)],flag,len_flag);
   strcat(str,":");
   strncpy(&str[strlen(str)],mess,len_mess);
   if(strlen(str)){
      ptr=&str[strlen(str)-1];
      while(ptr>str && isspace(*ptr))*(ptr--)=0;
   }
   
   printf("%s\n",str);
}


void rernev_(int *irun,int *ievnt,int *irec)
{
   *irun=_dc_run_number_;
   
   /* I don't think these are important for us */
   *ievnt=0;
   *irec=0;

}

void revinm_(char *envar,char *appstr,char *dest
   ,int len_envar,int len_appstr,int len_dest)
{
   char *ptr;
   char envar_c[64],appstr_c[64];
   
   strncpy(envar_c,envar,len_envar);envar_c[len_envar]=0;
   strncpy(appstr_c,appstr,len_appstr);appstr_c[len_appstr]=0;
   /* I guess this is what this is supposed to do */
   
   ptr=getenv(envar_c);
   sprintf(dest,"%s/%s",ptr ? ptr:"",appstr_c);

}












