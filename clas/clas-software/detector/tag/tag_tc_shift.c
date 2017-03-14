//  RCS ID string.  Don't edit next line! CVS takes care of it.
//  $Id: tag_tc_shift.c,v 1.8 2008/03/16 04:33:24 fklein Exp $
//
//_begin_inc
//  include files :
//  ---------------------
// System headers first 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
// ---------------------- 
// other headers 
#include <ntypes.h>
#include <bostypes.h>
#include <call.h>
#include <tagcal_cmn.h>
#include <tag_param.h>
#include <tagruncontrol.h>
#include "tagtcl.h"
#include "tag_event.h"

// local variables
static int first_time = 0;
static int first_in_file = 1;
static int event_first = 0;
static int event_last = 0;
static int goni_status = 0;

FILE *fp_offsets = NULL;

void tc_shift_();
////////////////////////////////////////////



void tc_shift_() {
  int i;
  char *fname = NULL;
  
  int nread;
  
  clasHEAD_t *HEAD = NULL;
  
  if (first_time == 0) {
    first_time++;
    //init everything
    t_ref_.event_first  = 0;
    t_ref_.event_last   = 0;
    t_ref_.eevent_first = 0;
    t_ref_.eevent_last  = 0;
    
    for (i=0; i< 61; i++){
      t_ref_.tl_shift[i] = 0;
      t_ref_.tr_shift[i] = 0;
      t_ref_.tl_read[i]  = 0;
      t_ref_.tr_read[i]  = 0;
    }
    
    if (t_ref_.do_correct) {
      tagtcl_.Tagger_tpeaks_file_name[15] = 0;
      fname = tagtcl_.Tagger_tpeaks_file_name;
      if (fp_offsets = fopen(fname, "r")) {
	fprintf(stderr,"\n Opened Tc-peaks-file= %s\n", fname);
	nread = fscanf(fp_offsets, "%d", &t_ref_.runno_read);
	if ( HEAD = getBank(&bcs_, "HEAD")) {
	  if (HEAD->head[0].nrun != t_ref_.runno_read) {
	    fprintf(stderr, "\n Wrong Tc-peaks-file!\n runno(HEAD)=%d runno(file)=%d\n",
		    HEAD->head[0].nrun, t_ref_.runno_read);      
	    fflush(stderr);
	    exit(0);
	  }
	  // read first interval data
	  
	  // event numbers:
	  nread = fscanf(fp_offsets, "%d%d%d%d", 
			 &t_ref_.event_first,
			 &t_ref_.event_last,
			 &t_ref_.eevent_first,
			 &t_ref_.eevent_last);	  
	  first_in_file = t_ref_.event_first; // save first event number for first interval
	  fprintf(stderr,"\n tshift -I- First interval %d-%d \n", t_ref_.event_first,t_ref_.event_last);
	  // read peaks and
	  // compute offsets as follows:
	  // read peak>0 shift=read-ref
	  // read peak=0 no correction
	  // read peak<0 keep previous correction
	  
	  for (i=0; i< 61; i++) 
	    fscanf(fp_offsets,"%d%", &t_ref_.tl_read[i]);
	  for (i=0; i< 61; i++) 
	    fscanf(fp_offsets,"%d%", &t_ref_.tr_read[i]);
	  for (i=0; i< 61; i++) {
	    if (t_ref_.tl_read[i] > 0) 
	      t_ref_.tl_shift[i] = t_ref_.tl_read[i] - t_ref_.tl_ref[i];
	    else if (t_ref_.tl_read[i] ==  0) 
	      t_ref_.tl_shift[i] = 0;
	    if (t_ref_.tr_read[i] > 0) 
	      t_ref_.tr_shift[i] = t_ref_.tr_read[i] - t_ref_.tr_ref[i];
	    else if (t_ref_.tr_read[i] ==  0) 
	      t_ref_.tr_shift[i] = 0;
	  }
	}
	else {
	  fprintf(stderr, "\n No HEAD bank found! I'll better exit \n");
	  fflush(stderr);
	  exit(0);
	}
      }
      else {
	fprintf(stderr, "\n Unable to open Tc-peaks-file= %s\n",fname);
	fflush(stderr);
	exit(0);
      }
    }
  } // end first_time
  
  if ( t_ref_.do_correct ) {
    HEAD = getBank(&bcs_, "HEAD");
    if ( HEAD->head[0].nevent >=first_in_file) {      
      if ( HEAD->head[0].nevent > t_ref_.event_last) { 
	do 
	  { nread = fscanf(fp_offsets, "%d%d%d%d", 
			   &t_ref_.event_first,
			   &t_ref_.event_last,
			   &t_ref_.eevent_first,
			   &t_ref_.eevent_last);
	  if(nread != EOF) {
	    event_first = t_ref_.event_first; // save them just in case
	    event_last  = t_ref_.event_last;
	    fprintf(stderr, " tshift -I- reading intreval %d-%d. Current event %d \n",
		    t_ref_.event_first,t_ref_.event_last,HEAD->head[0].nevent);
	    for (i=0; i< 61; i++) 
	      fscanf(fp_offsets,"%d%", &t_ref_.tl_read[i]);
	    for (i=0; i< 61; i++) 
	      fscanf(fp_offsets,"%d%", &t_ref_.tr_read[i]);
	    for (i=0; i< 61; i++) {
	      if (t_ref_.tl_read[i] > 0) 
		t_ref_.tl_shift[i] = t_ref_.tl_read[i] - t_ref_.tl_ref[i];
	      else if (t_ref_.tl_read[i] ==  0) 
		t_ref_.tl_shift[i] = 0;
	      if (t_ref_.tr_read[i] > 0) 
		t_ref_.tr_shift[i] = t_ref_.tr_read[i] - t_ref_.tr_ref[i];
	      else if (t_ref_.tr_read[i] ==  0) 
		t_ref_.tr_shift[i] = 0;
	    }
	  }
	  else {
	    fprintf(stderr, "\n Reached EOF of Tc-peaks-file "
		    "and did not find the right interval \n Current event number=%d, "
		    "firts_in_file=%d, last interval: %d-%d\n", 
		    HEAD->head[0].nevent, first_in_file, event_first, event_last);
	    exit(0);
	  } 
	  } while(!((event_first <= HEAD->head[0].nevent)&&(event_last >HEAD->head[0].nevent)));
	
	fprintf(stderr, "\n tshift -I- Found intreval %d-%d for event %d in file\n",
		t_ref_.event_first,t_ref_.event_last,HEAD->head[0].nevent);
	return;
      }
    } 
  }
}
