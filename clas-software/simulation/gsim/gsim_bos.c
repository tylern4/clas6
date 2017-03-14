/*
 * $Id: gsim_bos.c,v 1.10 2002/04/09 19:15:14 fklein Exp $
 *
 * Documentation for subroutine gsim_bos.c
 *
 * Purpose:
 * --------
 *  Starts up the BOS code.
 *
 *  Re-write of the original init_bos.F
 *
 *  This code replaces the old one, and at some point should get absorbed into
 *  a single file dealing with all BOS related calls.
 *
 *  This rewrite was forced because the old code garbled the data before writing it
 *  to disk.
 *
 * Author: Maurik Holtrop     Created: Fri April 10 1998
 *
 *_end_doc
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "ntypes.h"
#include "bostypes.h"
#include "gsim_c_common.h"
#include "geant_commons.h"

static char cfile[] =  "$RCSfile: gsim_bos.c,v $";
static char crevis[] = "$Revision: 1.10 $";
static char cstate[] = "$State: Exp $";
static char cdate[] =  "$Date: 2002/04/09 19:15:14 $";
static char cautho[] = "$Author: fklein $";
/*   Module information: */
static char crname[] = "gsim_bos.c";
static char crauth[] = "Maurik Holtrop";

extern GSIMOUT_t gsimout_;
extern GSIMCONTROL_t gsimcontrol_;

void create_backup_version(char *file);                  /* Make backup if file exists. */

void init_bos_(void){   /*  Fortran callable. */

  char tmpstr[256];
  char *gsim_bosout_flag=GSIM_BOSOUT_FLAG;
  char *gsim_bosout_env=GSIM_BOSOUT_ENV;
  int iflag, ilenddl;
  int ilen=0;
  int max =300;
  int nbcs=700000;
  int nwcs=400000;
  char *dir, *file;
  char ddl_file[]={"clasbanks.ddl"};

  bnames_(&max);
  bos_(bcs_.iw, &nbcs);
  bos_(wcs_.iw, &nwcs);

  ilenddl=strlen(ddl_file);

  dir=getenv("CLAS_PACK");
  if(dir!=NULL) {
    ilen=strlen(dir)+ilenddl+10;
    file=(char *)malloc(ilen+1);
    sprintf(file,"%s/bankdefs/%s",dir,ddl_file);
    if(access(file,F_OK)==EOF) ilen=0;
  }
  if(!ilen) {
    dir=getenv("CLAS_PARMS");
    if(dir!=NULL) {
      ilen=strlen(dir)+ilenddl+1;
      file=(char *)malloc(ilen+1);
      sprintf(file,"%s/%s",dir,ddl_file);
      if(access(file,F_OK)==EOF) ilen=0;
    }
  }
  if(ilen){
    txt2bos_(file, ilen);
    free(file);
  }
  else {
    sprintf(tmpstr,"ddl-file not given: CLAS_PACK and CLAS_PARMS were not defined");
    sprintf(tmpstr," looking for either CLAS_PACK/bankdefs/clasbanks.ddl");
    sprintf(tmpstr," or CLAS_PARMS/clasbanks.ddl");
    recmes_(crname,"F",tmpstr,strlen(crname),1,strlen(tmpstr));
  }

  bkfmt_("MCIN","(9F,I)",4,6);     /* old MC track parameters. This will be removed at some point.  */

  if(gsimcontrol_.nobosout[ALL])return; /* No BOS output needed. */

  gsimout_.gsimout_unit=2;              /* Pick an output unit the easy way. */

  /* An unfortunate call to get the filename, until we restructure the call to main */

  get_param_(gsim_bosout_flag,gsim_bosout_env,gsimout_.gsimout_filename,&iflag,
         strlen(gsim_bosout_flag),strlen(gsim_bosout_env),100);

  fix_fortran_string(gsimout_.gsimout_filename);

  if(iflag < 0){
    strcpy(gsimout_.gsimout_filename,GSIM_BOSOUT_NAME);
  }

  create_backup_version(gsimout_.gsimout_filename);

  strcpy(gsimout_.gsimout_daname,"GSIMOUT");

  sprintf(tmpstr,"Opening BOS output file '%s' on unit 2 BOSNAME %s\n",gsimout_.gsimout_filename,gsimout_.gsimout_daname);
  recmes_(crname,"I",tmpstr,strlen(crname),1,strlen(tmpstr));

  sprintf(tmpstr, "OPEN %s UNIT=%d FILE=\"%s\" WRITE STATUS=NEW RECL=32760 SPLITMB=2047 FORM=BINARY",
      gsimout_.gsimout_daname,gsimout_.gsimout_unit,gsimout_.gsimout_filename);
  if (!fparm_c(tmpstr)) {
    fprintf(stderr,"Unable to open file %s\n",gsimout_.gsimout_filename);
    exit(1);
  }



  return;
}


void bos_out_(void){      /* Callable from FORTRAN. */

  int icode;
  int unit;

  unit = gsimout_.gsimout_unit;

  icode = putBOS(&bcs_,unit,"E");

  if(!icode){
    fprintf(stdout,"ERROR - Trouble writing out BOS bank. \n");
  }

  dropAllBanks(&bcs_,"E");
  cleanBanks(&bcs_);


  return;
}



#include <unistd.h>

int fix_fortran_string(char *str){

  int i;

  i=strlen(str);
  while(i>=0 && (str[i]==32 || str[i]==6 || str[i] == 0))i--;
  i++;
  str[i]='\0';
  return(i);
}

void create_backup_version(char *file){
/*
 * Create a backup copy of "file" before writing a new "file".
 *
 * This routine checks whether "file" exists. If it doesn't do nothing.
 * If the file exists, it will rename the file to file.~#~ with the lowest
 * # so that the file is unique.
 */

  int i;
  char copyfile[256];

  if(access(file,F_OK)!=EOF){       /* Old file exists, so make it a backup. */
    i =1;                           /* Starting version one, scan for other versions. */
    do{
      sprintf(copyfile,"%s.~%d~",file,i);
      i++;
    }while(access(copyfile,R_OK)!=EOF);     /* get the version number */

    if(rename(file,copyfile)==-1){        /* Rename file to copyfile. */
      fprintf(stderr,"Could not create backup copy %s\nfrom %s\n",
          copyfile,file);
      exit(1);
    }
  }
}
