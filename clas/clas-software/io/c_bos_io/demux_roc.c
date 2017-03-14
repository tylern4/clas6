/*********************************************************************************
 *                                                                               *
 *           DEMUX_ROC  "Routines for unscrambling raw SC, EC, EC1,CC  banks"    *
 *                                                                               *
 *   Version 0.9                                                                 *
 *                                                                               *
 *  (Later to be extended to DC0 banks also....)                                 *
 *                                                                               *
 *                                                                               *
 *  Author:  Maurik Holtrop        7/7/97                                        *
 *           maurik.holtrop@unh.edu        http://www.physics.unh.edu/Maurik     *
 *                                                                               *
 *                                                                               *
 *  Short Commings:                                                              *
 *           1) Doesn't yet handle drift chambers. Just haven't had time.        *
 *           2) So far only UNSCRAMBLES. Should also be able to SCRAMBLE         *
 *              (i.e. produce a RAW bank from bossified banks.)                  *
 *           3) Could use some testing on banks other than SC.                   *
 *                                                                               *
 *********************************************************************************
 */

/*********************************************************************************
 *                                                                               *
 * Notes:                                                                        *
 *                                                                               *
 * I try to be somewhat space contious by only allocating the space that is      *
 * needed for each module encountered. It would be safer to allocate space       *
 * for an entire crate all at once, but very wastefull (therefore slow.)         *
 * There may be ways to optimize space further, but at this point I prefer       *
 * to be safe, and avoid crashes.                                                *
 * This does mean that sometimes you have to check the table to make sure        *
 * the entry you want to access actually exists: Check for NULL pointers !       *
 *                                                                               *
 * These routines have been extensively tested on the SC raw banks. The other    *
 * banks (except for DC) should work similarly well, but I've not tested as      *
 * much.                                                                         *
 *                                                                               *
 * Notice:                                                                       *
 * -------                                                                       *
 *                                                                               *
 * demux_roc.h contains some VERY ARCHITECTURE DEPENDANT structures.             *
 * These structures are not essential for any of these routines, but usefull     *
 * for decoding the raw data from the ADCs and TDCs. The problem is that bit-    *
 * wise structures depend on the BYTE ORDER of the system you use. I've tested   *
 * This on Linux and Sun systems, which have opposite byte orders.               *
 ********************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "ntypes.h"
#include "bostypes.h"
#include "demux_roc.h"


/* GLOBAL VARIABLE. */

int quiet_flag;         /* Set the level of quietness of the codes. */

/* Templates for Parsing Utilities. (Routines are all in this file. ) */

int kill_left_space(char *string);
char *quoted_strtok(char *instr,const char *tokenstring,const char *sptokstring);
char *squeeze_space(char *string);
char *fgets_clean(char *s, int size, FILE *pt);
void free_n_tokens(int num_tokens,char **token);
int read_n_tokens(int num_tokens,char **token,FILE *infp);


/***************** Main Routines ****************************/

/************ init_decode_roc *******************************/

void init_decode_roc(char * rocfile,    /* The name (+path) of the ROCxx.tab file. */
                     ROC_DMUX_t *rtab,  /* Output demuxing table, or NULL          */
                     ROC_MUX_t *dtab)   /* NULL (NOT IMPLEMENTED= always NULL )    */
{
  /* Initialize the decoding tables by reading the ROCxx.tab file. 
   * 
   * Search algorithm for this file is:
   *      A) Current directory.
   *      B) $CLON_PARMS/TT 
   *      C) $CLAS_PARMS/TT
   *
   * Note that on the CLON cluster there is a lot of confusion as of which directory
   * the ROCxx.tab files belong. It is safest to specify a full path name.
   *
   * Muxing (i.e. creating a RAW bank from a coocked one, is not implemented, and won't be
   *         for a while. If you REALLY REALLY need it, contact me.... )
   */

  FILE *fpt;
  char *envdir;
  char *envdir2;
  char line[256];
  char line2[256];
  char *tokens[MAX_NUM_TOK];
  int isector,ilayer,islab,iplace,icrate,islot,itype,ichan;
  int idummy;
  int i;

  for(i=0;i<MAX_NUM_TOK;i++)tokens[i]=malloc(sizeof(char)*TOKEN_DEPTH);
  memset(line,0,sizeof(line));
  memset(line2,0,sizeof(line2));

/* Find the file with the decoding information. */

  if( strchr(rocfile,'/') == NULL && access(rocfile,4)==EOF){  
                                      /* Is rocfile a path + filename ? */
                                      /* No, search . $CLON_PARMS and $CLAS_PARMS */
    envdir=getenv("CLON_PARMS");      /* Not in . , check environments next. */
    envdir2=getenv("CLAS_PARMS");
      
    if(envdir!=NULL){        /* We have a $CLON_PARMS so look in TT subdir */
      strcpy(line,envdir);
      strcat(line,"/TT/");
      strcat(line,rocfile);
    }
    if(envdir2!=NULL){       /* We have a $CLAS_PARMS so look in TT subdir */
      strcpy(line2,envdir2);
      strcat(line2,"/TT/");
      strcat(line2,rocfile);
    }
    else if(envdir==NULL){   /* envdir==NULL && envdir2==NULL */
      if(quiet_flag<3)fprintf(stderr,"Neither CLAS_PARMS nor CLON_PARMS is defined, I can not find the file %s\n",rocfile);
      exit(1);
    }
    
    if(access(line,4)!=EOF){
      if(!quiet_flag)fprintf(stderr,"Retreiving %s from %s/TT \n",rocfile,envdir);
    }
    else if(access(line2,4)!=EOF){
      if(!quiet_flag)fprintf(stderr,"Retreiving %s from %s/TT \n",rocfile,envdir2);
      strcpy(line,line2);
    }
    else {
      if(quiet_flag<3)fprintf(stderr,"Roc table file %s not was not found in ./ , CLON_PARMS/TT or CLAS_PARMS/TT\n",rocfile);
      exit(1);
    }
  }
  else{
    strcpy(line,rocfile);   /* rocfile contains full path + filename */
  }

/* Found the file, now open it and read in the information. */
  
  if( (fpt = fopen(line,"r"))==NULL){
    if(quiet_flag<3)fprintf(stderr,"Could not open file: %s \n",line);
    exit(1);
  }    
  while(!feof(fpt)){
    if(read_n_tokens(MAX_NUM_TOK,tokens,fpt)<10){   /* Read one line and parse tokens on the line.*/
      if(feof(fpt))break;
      else{
	if(quiet_flag<3)fprintf(stderr,"Error reading ROC file. \n");
	exit(1);
      }
    }
    if(rtab!=NULL){  /* Fill demuxing table */
      sscanf(tokens[1 ],"%d",&isector);
      sscanf(tokens[2 ],"%d",&ilayer);
      sscanf(tokens[3 ],"%d",&islab);
      sscanf(tokens[4 ],"%d",&iplace);
      sscanf(tokens[5 ],"%d",&idummy);
      sscanf(tokens[6 ],"%d",&idummy);
      sscanf(tokens[7 ],"%d",&icrate);
      sscanf(tokens[8 ],"%d",&islot);
      sscanf(tokens[9 ],"%d",&itype);
      sscanf(tokens[10],"%d",&ichan);  
      if(rtab->mod[islot] == NULL){                           /* First encounter of this module. */
	rtab->mod[islot]= malloc(sizeof(ROC_DMUX_MODULE_t));  /* Need to allocate space for it.  */
	rtab->mod[islot]->nummod=islot;     /* Redundant, but safe. */
	rtab->crate     = icrate;           /* Keep track of crate. */
                                            /* NOTE: at this point crates can not be mixed. */

	for(i=0;i<64;i++){                    /* INITIALIZE ! */
	  rtab->mod[islot]->ch[i].sector= 0;  /* Set all channels to undefined. */
	  rtab->mod[islot]->ch[i].layer = 0;  /* Set all channels to undefined. */
	  rtab->mod[islot]->ch[i].slab  = 0;  /* Set all channels to undefined. */
	  rtab->mod[islot]->ch[i].place=  -1; /* Set all channels to undefined. */
	}

	switch(itype){
	case 1872:             /* TDC */
	  idummy = 1;
	  break;
	case 1877:             /* Wire chamber TDC */
	  idummy = 2;
	  break;
	case 1881:
	  idummy = 0;           /* ADC */
	  break;
	default:
	  idummy = 255;
	}
	rtab->mod[islot]->type = (unsigned short)idummy;
      }
      else
	if(rtab->crate != icrate){
	  if(quiet_flag<3)
	    fprintf(stderr,
		    "Sorry, but you can not mix crates in a ROC definition file.\n");
	  exit(1);
	}

      strncpy(rtab->mod[islot]->ch[ichan].bankname,tokens[0],4);
      rtab->mod[islot]->ch[ichan].sector= (unsigned short)isector;
      rtab->mod[islot]->ch[ichan].layer  = (unsigned short)ilayer;
      rtab->mod[islot]->ch[ichan].slab  = (unsigned short)islab;
      rtab->mod[islot]->ch[ichan].place = (unsigned short)iplace;
    }
    if(dtab!=NULL){ /* Muxing table, for going the other direction. */
      if(quiet_flag<3)
	fprintf(stderr,
		"Very sorry, but the translation from bos->raw is not finished. \n");
      exit(1);
    }
  }
}
  
ROC_DMUX_t *alloc_decode_roc(int mode)  /* Mode:  >0 Allocate a demuxing table. */
                                        /* Mode:  <0 Allocate a muxing table. NOT IMPLEMENTED. */
{
  /* This routine allocates memory for the ROC_DMUX_t structure. */ 
     
  ROC_DMUX_t *a;
  int i;
  
  if(mode==0)mode=1;
  if(mode>0){
    a = (ROC_DMUX_t *)malloc(sizeof(ROC_DMUX_t)*mode);
    for(i=0;i<26;i++)
      a->mod[i]=NULL;
  }
  else
    a=NULL;      /* Not yet implemented */
  
  return(a);
}



int decode_raw_data(clasRC_t *RC,int n_call,ROC_DMUX_t *RC_look,
		    int tdc_slot_low,int tdc_slot_high, RAW_DATA_t *R){
/*
 * This routine decodes the RAW data from a TDC or ADC. ( As of now, the 
 * Multi-Hit TDC use in DC are not supported.)
 * This works different for each module type, so there is specific code for 
 * each module type. 
 * It is important to note that because the ADC's have header words (which are
 * quite useless to us) we need to keep count of adc entries, and the RC bank
 * must be called in sequence for the ADCs.
 *
 * Thus: THIS ROUTINE MUST BE CALLED WITH THE ENTIRE BANK IN SEQUENCE.
 *       RUNNING n_call FROM 0 to RC->bank.nrow-1  (for ADC's) !!!
 *
 * Arguments:
 *
 *    *RC            = Raw data pointer as defined in bostypes.h
 *    n_call         = The row entry in RC, must be called in order for ADC's
 *    *RC_look       = Pointer to Structure containing all the crate info. 
 *                     Defined in demux_roc.h
 *    tdc_slot_low   = If RC_look==NULL this determines the lowest slot number 
 *                     with a TDC.
 *    tdc_slot_high  = If RC_look==NULL this determines the higher slot number 
 *                     with a TDC.
 *    *R             = Pointer to output Raw structure.
 *
 * GLOBALS:
 *
 *    quiet_flag   = Set on commandline, Verbosity of routine.
 */

  static int is_channel_flag; /* Static, to save from call to call. */
  int dat;
  int iwc;

  if(n_call==0){ /* initialization code for new RC bank. */
    is_channel_flag=0;
  }
  
  dat=RC->rc[n_call].data;         /* Get the raw data (an ADC or TDC word. */
		
  R->islot= (dat>>27)&0x1F;      /*  However, it looks way confusing... */
  iwc  = (dat)&0x007F;
	    
/* Determine what type of module is in this slot. 
 * This is needed because the modules decode
 * differently. We are mostly interested in ADC's but I'll retain code for TDC also.
 * If the Translation Table is not availabe, we can't determine what type a slot is !
 * In that case, use the simple rule that ADCs are on the outside ( slot<8 or slot>17)
 */		

  if(R->islot<1 || R->islot>25){
      if(quiet_flag<3)fprintf(stderr,
	      "\nDATA ERROR: Data decodes to non existing slot # %d (decode_raw_data)\n",
	      R->islot);
      return(3);          /* Serious Problem, probably should quit here. */
  }

  if(RC_look!=NULL){            /* Find what module it is from the TT */
    if(RC_look->mod[R->islot]!=NULL){
      R->itype = RC_look->mod[R->islot]->type;
    }else{
      if(quiet_flag<3)fprintf(stderr,
	      "\nTT Table ERROR: Slot not in tables: %d (decode_raw_data)\n\n",
	      R->islot);
      return(3);          /* Serious Problem, probably should quit here. */
    }
  }else{                  /* Don't have TT, so assume crate info is ok. */
    if(R->islot>= tdc_slot_low
       && R->islot<= tdc_slot_high)
      {
	R->itype =1;		      
      }else{
	R->itype =0;
      }
  }

  if(R->itype==1){           /* It's a TDC */

    R->ichan= (dat>>16)&0x3F;
    R->idat = (dat)&0x0FFF;  /* Only use 12 bits */
  }
  else                    /* Is ADC */
    {
      
/* The first part of this code identifies the header words, which are ignored. 
 * Each header word contains the number of real words that follow. 
 * We just count down.
 */
      
      R->ichan= (dat>>17)&0x3F;		
      R->idat = (dat)&0x3FFF;	
      if(is_channel_flag==0 ){  /* Is an ADC HEADER */
	if(iwc>0){  /* Next entry will be a channel. */
	  is_channel_flag = iwc-1;
	}
 	R->idat = (int)iwc-1;
	return(1);   /* Skip the histogramming ! */
      }
      else{ /* Not a header, so use the data. */
	
	is_channel_flag--;       /* Count down to next header or TDC. */
	
	if(R->islot<1 || R->islot >25){
	  if(quiet_flag<2)fprintf(stderr,"\nIllegal Slot: %d\n",R->islot);
	  return(2);   /* Minor error. Recoverable. */
	}
	if(R->ichan<0 || R->ichan >=64){   /* Check for errors in data. */
	  if(quiet_flag<2)fprintf(stderr,
				    "\nIllegal Channel %d, for Slot %d \n",R->ichan,R->islot);
	  return(2); /* Minor error, recoverable. */
	}
      }
    }
  return(0);
}

void decode_raw_banks(BOSbank *BCS,                  /* The Big Common boS array. usually &bcs_ */
                      char *bankname,                /* Name of the bank i.e. "SC  " (always 4 char) */
                      int sect,                      /* Which SECTOR ? */
                      clasG_t  *Bankout,             /* A pointer to the output bank. */
                      clasRC_t *Raw,                 /* A pointer to the RAW bank. */
                      ROC_DMUX_t *rc13_look)
{
  /* This routine accepts a RAW bank, and then sorts the data into a BOS bank (SC,CC,EC,EC1) 
   * according to the information in the Demuxing Tables.
   * 
   * Note: The current system always sorts the data with one bank for each sector. Thus you'd
   *       need to call this routine 6 times, once for each sector, creating 6 sequential banks.
   *       (Yes, if you'd like, fix this and make it more efficient. ;> )
   */

  int i,j;

  if(Bankout==NULL){ /* Need to allocate the output bank. */
    if(strncmp(bankname,"SC  ",4)==0)
      Bankout= (clasG_t  *)malloc(sizeof(clasSC_t));
    else if(strncmp(bankname,"EC  ",4)==0)
      Bankout= (clasG_t  *)malloc(sizeof(clasEC_t));
    else if(strncmp(bankname,"EC1 ",4)==0)
      Bankout= (clasG_t  *)malloc(sizeof(clasEC1_t));
    else if(strncmp(bankname,"CC  ",4)==0)
      Bankout= (clasG_t  *)malloc(sizeof(clasCC0_t));
    else{
      if(quiet_flag<3)
	fprintf(stderr,"Don't know bank type %s, can't allocate memory. \n",bankname);
      exit(2);
    }

    if(Bankout==NULL){
      if(quiet_flag<3)fprintf(stderr,"Can't allocate memory for output bank. \n");
      exit(2);
    }
  }
  
  for(i=0;i<Raw->bank.nrow;j++)
    {
      int dat;
      int ibufh,ibufd,ichan,iwc,idat,islot;
      int ilayer,islab,iplace;
      int ind;
      char *chbank;
      int is_channel_flag=0;

      dat=Raw->rc[j].data;        /* This is the device independent method.        */
      islot= (dat>>27)&0x1F;      /* Looks ugly, but is safer than the structures. */
      ibufh= (dat>>7)&0x3F;
      ibufd= (dat>>24)&0x03;
      iwc  = (dat)&0x7F;

      if(rc13_look->mod[islot]==NULL){
	if(quiet_flag<3)
	  fprintf(stderr,"Uninitialized slot encountered in decode_raw_banks: %i \n",islot);
	continue;
      }
	
      if(rc13_look->mod[islot]->type==1 ) /* Is a TDC, This is specific to the RC13 crate layout. */
	  {
	    iwc=0;
	    ichan= (dat>>16)&0x3F;
	    idat = (dat)&0x0FFF;    /* Only use 12 bits */
	  }
	else                    /* Is ADC */
	  {	    
	    ichan= (dat>>17)&0x3F;		
	    idat = (dat)&0x3FFF;	
	    
	    if(is_channel_flag ){ /* Not a header */
	      is_channel_flag--;
	    }
	    else{  /* Is an ADC HEADER */
	      if(iwc>0){  /* Next entry will be a channel. */
		is_channel_flag = iwc-1;
	      }
	      continue;
	    }
	  }

      if(rc13_look->mod[islot]->ch[ichan].sector != sect){ /* not the sector we want */
	continue;
      }

      ilayer  =rc13_look->mod[islot]->ch[ichan].layer;
      islab   =rc13_look->mod[islot]->ch[ichan].slab;
      iplace  =rc13_look->mod[islot]->ch[ichan].place;
      chbank  =rc13_look->mod[islot]->ch[ichan].bankname;
      
      if(strncmp(bankname,"SC  ",4)==0){
	/*	ind = find_bank_row(Bankout,ilayer,islab); */
	if(ind < 0){
	  /*	  Bankout->bank.nrow++;
	  Bankout->sc = realloc; */
      }
      else if(strncmp(bankname,"EC  ",4)==0){
      }
      else if(strncmp(bankname,"EC1 ",4)==0){
      }
      else if(strncmp(bankname,"CC  ",4)==0){
      }
      else{
	if(quiet_flag<3)fprintf(stderr,"Don't know bank type %s, can't sort data. \n",bankname);
	exit(2);
      }
      
      }
    }
}


/************************************************/
/*                                              */
/*      Supporting C Routines                   */
/*                                              */
/* by:  Maurik Holtrop                          */
/*                                              */
/************************************************/
/* 
 *  This routine will read num_tokens from the input file infp.
 *  It will return them in the char array token.
 *  Input rules are as follows:
 *
 *      Everything following an ! is a comment.
 *      Tokens are separated by TOKEN_SEP currently: SPACE,COMMA,\t,\n,(,)
 *      Everything between " or ` characters is considered to be one token.
 *      the "cpp" preprocessor recognizes " but not `, so use ` to make sure
 *      the concatenations of #defines are read as one token.
 *      The quoted tokens will have all whitespace squeezed out.
 */

char *quoted_strtok(char *instr,const char *tokenstring,const char *sptokstring);
char *squeeze_space(char *string);
char *fgets_clean(char *s, int size, FILE *pt);

int read_n_tokens(int num_tokens,char **token,FILE *infp)
{
  char string[MAX_STR_LEN];
  char *tmptok;
  int i;
  int indx,loc,len;
  int continue_count;

  continue_count = 0;
  indx = 0;

  for(i=0;i<num_tokens;i++)
    {
      if(token[i]==NULL)token[i]=malloc(TOKEN_DEPTH);
    }

  while(!feof(infp)&&indx<num_tokens)
    {
      fgets_clean(string,MAX_STR_LEN,infp); /* Get a line after comments are removed. */
      if(feof(infp))
	{
	  return(0);
	}
      if(ferror(infp))
	{
	  if(quiet_flag<3)fprintf(stderr,"\nERROR- occurred while reading parameters.(read-n-tokens)\n\n");
	  exit(1);
	}

      len = kill_left_space(string);   /* Kill all white space on left */
      if(len<=0)continue;              /* line is empty. */
      
      tmptok = quoted_strtok(string,TOKEN_SEP,QUOTE_SEP);
      strncpy(token[continue_count],tmptok,TOKEN_DEPTH-1);
      squeeze_space(token[continue_count]);

      indx = continue_count+1;
      
      while(indx<num_tokens)
	{
	  tmptok =  quoted_strtok(NULL,TOKEN_SEP,QUOTE_SEP);
	  if(tmptok == NULL)          /* Continue on next line */
	    {
	      continue_count=indx;
	      break;
	    }	  
	  strncpy(token[indx],tmptok,TOKEN_DEPTH-1);
	  squeeze_space(token[indx]);
	  indx++;
	}
    }

  return(indx-1);
}

void free_n_tokens(int num_tokens,char **token)
{
  int i;

  for(i=0;i<num_tokens;i++)
    {
      if( token[i]!=NULL )
	{	  
	  free(token[i]);
	  token[i]=NULL;
	}
    }
}

/************************************************/
/*						*/
/*                c_library			*/
/*						*/
/*    Maurik Holtrop				*/
/*						*/
/************************************************/
/*
 * This routine reads a line from the input file pointed to by
 * pt just like fgets, except that it will remove any comments
 * it encouters.
 * Comments are either "C" style, or a # or ! character.
 * For "C" style comments, newline characters are ignored
 * until an end comment is encountered. 
 * For "#" or "!" comments, everything after the comment character
 * is ignored, the pt is placed after the next newline.
 *
 */

char *fgets_clean(char *s, int size, FILE *pt)
{
  char *sx;
  int ch;
  int count=0;

  sx=s;
  while( (ch=fgetc(pt))!=EOF){
    if(ch=='#' || ch=='!'){                        /* Comment char found. */
      while( ((ch=fgetc(pt))!=EOF) && ch!='\n' );  /* Wind to end line. */
    }                                              /* if(ch=='\n') will finish up. */
    if(ch=='/')
      if( (ch=fgetc(pt))!=EOF && ch=='*'){         /* Real comment ? */
	while( (ch=fgetc(pt)) != EOF)              /* YES, Wind to next "*" */
	  if(ch=='*')                              /* End comment ? */
	    if( ((ch=fgetc(pt))!=EOF) && ch=='/')  /* YES ! */
	      continue;                            /* continue with next character. */
      }	                                            
      else{                                         /* No, just a 'loose' "/" */
	*sx='/'; sx++; count++;                     /* Recover the "/" by pushing it onto s */
	if(count>=size-1){                          /* OOPS, string full. */
	  *sx=0;
	  return(s);
	}
      }
                                                      /* Not a Comment. */
    if(ch=='\n'){                                   /* We are done with the line. */
      *sx=ch; sx++; count++;
      *sx=0;
      return(s);
    }
    *sx=ch; sx++; count++;
    if(count>=size-1){                          /* OOPS, string full. */
      *sx=0;
      return(s);
    }    
  }
}


/************************************************/
/*						*/
/*                c_library			*/
/*						*/
/*    Maurik Holtrop				*/
/*						*/
/************************************************/
/*
 * This routine squeezes all spaces from a string. 
 * Spaces are SPACE,\t,\n
 */



char *squeeze_space(char *string)
{
  int len,i,j;

  len = strlen(string);

  for(i=0;i<len;i++)
    {
      if(string[i]==' '||string[i]=='\t'||string[i]=='\n')
	{
	  for(j=i+1;j<=len;j++)string[j-1]=string[j];
	  i--;
	}
    }

  return(string);
}

/************************************************/
/*						*/
/*                c_library			*/
/*						*/
/*    Maurik Holtrop				*/
/*						*/
/************************************************/
/*
 * This is a version of strtok that will deal with tokens in a
 * special manner: Anything quoted is considered a sigle token.
 * This single token is then passed back.
 * One may want to squeeze out the white space from the returned
 * token with squeeze_string.
 *
 * Arguments:
 *
 * 1-st: instr    The input string to be searched for a token. 
 *                if NULL continue searching the previously used
 *                string. (like strtok)
 *
 * 2-nd: tokenstr The tokens that are to be recognized. (like strtok)
 *
 * 3-rd: sptokstr The characters that are considered to be "quotes"
 *                use "\"`" for " and ` as quote characters. 
 *                (unlike strtok)
 *
 */



char *quoted_strtok(char *instr,const char *tokenstring,const char *sptokstring)
{
  static char *save;
  char *p,*q;
  char *nexttok;

  char tokenstr[MAX_STR_LEN],sptokstr[MAX_STR_LEN];

  strcpy(tokenstr,tokenstring);
  strcpy(sptokstr,sptokstring);

  if(instr!=NULL)save = instr;

  while(1)
    {
      if(save[0]=='\0')      /* No token found. */
	{
	  return(NULL);
	}
      if(strchr(sptokstr,save[0])!=NULL) /* first " */
	{
	  nexttok = sptokstr;
	  if(save[1]!=0)save++;
	  else return(NULL);
	  break;                  /* Special char. */
	}
      if(strchr(tokenstr,save[0])==NULL) /* first char not in token str */
	{
	  nexttok = tokenstr;
	  break;
	}
      save++;
    }
  
  p = save;

  while(2)
    {
      if(p[0]=='\0')      /* No second token found. */
	{
	  q = save;
	  save = p;
	  return(q);
	}
      if(strchr(nexttok,p[0])!=NULL) /* First char in token string */
	{
	  p[0]=0;
	  q = save;
	  save = p+1;
	  return(q);
	}
      p++;
    }

}      

/*------------------------------ kill_left_space ------------------------------*/
int kill_left_space(char *string)
{
  int loc,len,i;

  len =(int)strlen(string);           /* lenght of string */
  loc =(int)strspn(string," \t\12\n\f\v"); /* Find first char that is not space or tab,LF,NL etc */

  for(i=0;i<=(len-loc);i++) string[i]=string[i+loc];  /* move all chars up */
  
  return(len-loc);                    /* return new length */
}  
