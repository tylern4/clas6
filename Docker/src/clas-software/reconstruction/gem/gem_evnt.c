//#include "gem.h"

//this file is the same as in SAVEDIRS/gem_updated for the doFits loop.  It uses//circle + linefit instead of helix fit.  the doFits loop was taken from
// SAVEDIRS/gem_broke_nt
#include "TPC_BOS.h"
#include "rtpc.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <time.h>
/* stuff for calling fortran routines and linking cernlib */
#define  f2cFortran

/* #include <cfortran.h> */
/* #include <kernlib.h> */

int gem_evnt_()
{
  int ii,jj,kk;
  float track_theta_deg, track_phi_deg; /* for radian-challenged people */
  int temp;
  int redo;
  int cco;
  int identity;
  int tmin, tmax, dt, newPosition;
  float allSum, protonSum, longSum, fitSum, padSum;
  int tlow, npads_hit, npads_track;
  unsigned int datum,dtime,lead,chan;
  float qtot, qtot_win, qtot_win_pad, qpad_win[NAL_CHAN], qxtra_tot[NUM_XTRA], qlocal,qtime[NAL_SAMP];
  float tmaximum;
  float rat;
  float qbig, qtest, trig_chan, trig_time, qtmp; /* statistics for what caused each trigger */
  #define QSTEP 2000.0
  float hit_pad[NUM_PADS]; /* to store pad hit and total charge on pad */
  int num_hits[NUM_PADS];
  float trig_2,  trig_3; /*  2nd and 3rd biggest  signals in  time window */
  int nbig, data_flag;
  char title[80]; /* histogram titles, etc. */
#define NVAR 25  /* Number of Ntuple elements */
  typedef char chtag_type[8];
  chtag_type chtags[NVAR];
  typedef char id_name_t[11]; id_name_t id_name[30];
  char astring[4];

  //these were added by nate on 11-17-05
  int num_original_chains, npts_tofit;
  float vtl;//visual track length
  float sdist; //distance from start of track to cathode
  float edist; //distance from end of track to some chamber boundary
  float p_corrected;//for Sebastian's mom correction function
  float visible_charge;

  //nkb 12-05-05
  float b_z; //return value of routine which uses real field map to give a more
               //realistic b-field value

  int LREC = 1024;
  int ISTAT;
#define QTOT_WIN_CUT 500 /*  min qtot_win for entry to histograms  */


  int stopOnPrtns = FALSE;
  int tail_cancel=FALSE;
  int cm_filter=FALSE;
  int base_sub=FALSE;
  int tail_clip=FALSE;
  int graphics=FALSE;
  int binAboveThresh[NAL_SAMP];
  int keepReading = TRUE;
  int skipAhead = FALSE;
  int goToTrig = 1;
  int limitNum = FALSE;
  int maxRead = 8000;
  int printThis = 1;
  int next, num_bad_altro_data, bonus_bits;
  int WantedTrigType;
  float xtrq[8]; /* bonus_bits charges in time */
  int goodtr_n;
  int numSatR, numSatL;

  printPads = FALSE;
  num_read = 0;
  qtot_early=0.0;
  qtot_intime=0.0;
  qtot_late=0.0;

  //read_path(); //this initializes the pad location array as well as 
  //reading the .path files to assign cylindrical coordinates 
  //to the data cells - this is done in gem_init_c.c

  /*----------------------------------*/
  for (ii=0; ii<30; ii++) {sprintf(id_name[ii],"");}
  sprintf(id_name[1], "EARLY_STRT");
  sprintf(id_name[2], "LATE_START");
  sprintf(id_name[3], "GOOD_PROTN");
  sprintf(id_name[4], "LOTSoNOISE");
  //sprintf(id_name[5], "CLIP/STOP?");
  sprintf(id_name[26],"UNKNOWN   ");
  sprintf(id_name[27],"NO EVENT  ");
  //sprintf(id_name[6],"LONG STOP ");
  //sprintf(id_name[22],"Q < MIN   ");

  if ( data_flag= read_BOS() != 0 ) keepReading = TRUE;
  else keepReading = FALSE;
  num_read++;
  if ((data_flag > 0)&&(keepReading)) /* i.e. data as opposed to error */
    {
	num_events++;
	goToTrig = num_events+1;
	if (!(num_events % 1000)) {printf("%d TPC trigs analyzed \n",num_events);}
	//if (!(num_events % 100)) {printf("..\n");}
	//printf("calling GEM for the %d th time\n ",num_events);
	if (good_altro_data)
	  {
	    if(!quiet) printf("\n===================================================\n");
	    
	    bonus_bits= get_bonus_bits();// Extract bonusbits from special xtra channels
	    /*   printf("bonus_bits is %8x\n",bonus_bits); */
	    if ( (bonus_bits & 0x11) || (imTrigType & 0x200) ){ n_either_pulser++;}
	    if ( (bonus_bits & 0x11)                         ){ n_bb_pulser++;}
	    if (                        (imTrigType & 0x200) ){ n_tb_pulser++;}
	    if ( (bonus_bits & 0x11) && (imTrigType & 0x200) ){ n_both_pulser++;}
	    
  
  
  if(base_sub) baseline2(1); /* subtract pedestals found in file*/

  /* This call marks unusable hits in the hh_hitlist */
  clean_hits();

 /* step through data and accumulate charge sums:
     qtot, qtot_win, qtot_intime, qpad_win[pad], qxtra_tot */

chain_link();
  sum_hit_charges(&npads_hit, &qtot, &qtot_early, &qtot_late, &qtot_win, 
		  &qtot_intime, qtime, qpad_win, qxtra_tot);

  allSum= totIntQ; /* mostly redundant variables from legacy code */
 
  
 /* tempcode.c extracted from here */
 
 if(!quiet) printf("Number of chains is %d\n", num_chains);
 
 doFits = FALSE;

 if(num_chains > 0) 
   {
     doFits = TRUE;
     if(stopOnPrtns) quiet = FALSE;
   }

 //lets try to fit this data to a helix, whaddaya think?
 ntracks=0;
 identity = 27; //initialize to no event.
 tmin = 1000; tmax = -1000; //initialize so event by event report is meaningful
 dt = 1000; visible_charge = 0;
 num_original_chains = num_chains;

////////////////////////////////////
/// dropAllBanks(&bcs_, "GCPB"); ///
////////////////////////////////////

//printf("dofits = %d \n", doFits);

 if(doFits)
   {
     for(ii=0; ii<num_chains; ii++)
       {
	 //find out how many tracks there are and define the bank accordingly
	 if(num_hits_this_chain[ii]<MIN_LINKS_ON_TRACK)
	   {
	     if(!quiet) printf("Only %d hits on chain %d. Skipping it.\n",num_hits_this_chain[ii],ii);}
	 else	 
	   {ntracks++;
	   }
       }

     //printf("ntracks = %d \n", ntracks);
     //if there are no tracks, return to user_ana
     if(ntracks==0) 
       {
	 ntype[27]++;
/*	 printf("no track, return to calling function\n"); */
	 return;
       }

     clasGCPB_t* GCPB = (clasGCPB_t *)
       makeBank (&bcs_, "GCPB", 0, sizeof (gcpb_t) / 4, ntracks);
	 /////////////done with ntuple initialization///////////////////////
	 ///////////////////////////////////////////////////////////////////
     ntracks=0;
     for(ii=0; ii<num_original_chains; ii++)
       {
	 if(num_hits_this_chain[ii]<MIN_LINKS_ON_TRACK)
	   {
 	     ///*if(!quiet)*/ printf("Only %d hits on chain %d. Skipping it.\n",num_hits_this_chain[ii],ii); 
	   }
	 else //do a fit if the chain is long enough
	   {
	     if(ntracks > EVT_TRKS)
	       {
/*		 printf(">>> Too many tracks, truncating to %d\n",EVT_TRKS); */
	       }
	     else //if ntracks < EVT_TRKS
	       {
		 track_chain[ntracks]= ii;
		 //mark_earliest_hits(track_chain[ntracks]);
		 helix_error = 0;
		 helix_fit(fit_track_to_beamline, ntracks, track_chain[ntracks]);
		 linefit(ntracks, track_chain[ntracks]);
		 npts_tofit = 0;
		 npts_tofit = removeRes(ntracks,track_chain[ntracks]);
		 visible_charge = track_charge(ntracks, &vtl, &sdist, &edist);
		 track_chain[ntracks] = weightData(track_chain[ntracks]);
		 if(track_chain[ntracks] != ii)
		   {
		     npts_tofit = num_hits_this_chain[track_chain[ntracks]];
		   }
		 
		 
		 if(npts_tofit > 2)
		   {
		     helix_fit(fit_track_to_beamline, ntracks, track_chain[ntracks]);
		     linefit(ntracks, track_chain[ntracks]);
		     if(dca[ntracks] < DCA_CUT)
		     {
		       if(!quiet) printf("****refitting with beamline - old dca = %f*************\n", dca[ntracks]);
		       helix_fit(1, ntracks, track_chain[ntracks]);
		     }
		     track_phi= phi0[ntracks];
		     track_phi_deg= track_phi*180.0/PI;
		     track_theta = PI/2.0 - atan(dzds[ntracks]); /* atan returns -PI/2 to PI/2 */
		     track_theta_deg = track_theta*180.0/PI;
		     track_vertex = z_close[ntracks];
		     //p_perp_corrected = mom_fit_by_r((double)r_0[ntracks],(double)track_vertex/10.,(double)track_theta_deg);
		     /*       con   Tesla  mm      1st order uniform field approx. no E-loss */
		     b_z = bofz((track_vertex + z_exit)/2.0); //gives b_z in kiloguass
		     b_z /= 10.0; //converts to Tesla
		     if(b_z < 0.0) p_perp = (0.3)*(3.876)*fabs(r_0[ntracks]); 
		     else p_perp= (0.3)*(float)b_z*fabs(r_0[ntracks]); 
		     p_x = p_perp * cos(track_phi);
		     p_y = p_perp * sin(track_phi);
		     p_para = p_perp/tan(track_theta);
		     p_tot = sqrt(p_perp*p_perp + p_para*p_para);                      
		     //p_corrected = p_tot + 132.41*exp(-0.0376*p_tot)/sin(track_theta);
		     //p_corrected is the result of Sebastian's mom correction function
		     if(!quiet)
		     //if(-z_close[ntracks] < 9.86 && -z_close[ntracks] > 9.84)
		     {
			 printf("==================================================================\n");
			 printf("Trig#-%d Fit Results for Track %d (Chain %d) \n",num_events,ntracks, track_chain[ntracks]);
			 printf("    x_0       y_0     radius      dca     z@dca      theta      phi     chi2/dof  bz\n");
			 printf(" %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f\n",
				x_0[ntracks],y_0[ntracks],r_0[ntracks],dca[ntracks],z_close[ntracks],
				track_theta_deg, track_phi_deg, chi2[ntracks], b_z);
			 printf("  p_perp      p_x       p_y      p_para    p_tot   \n");
			 printf(" %9.3f %9.3f %9.3f %9.3f %9.3f MeV/c \n", 
				p_perp, p_x, p_y, p_para, p_tot);		    
		     }
/*		     printf("%9.2f %9.2f %d\n", track_vertex,track_theta);  */
		     
		     identity = event_id(vtl, visible_charge, edist, sdist,
					 track_chain[ntracks]);
		     dt = tmax - tmin +1;

		     npads_track = countPads(ntracks,track_chain[ntracks]);
		     //if ( (dt > 3*n_pathpts) ) {identity = 4;} //too noisy.n_pathpts is time interval for drift from r to R.
		     
		     ntype[identity]++;
		     //printf("filling ntuple for track %d\n",ntracks);
		     //GCPB->gcpb[ntracks].pid    = identity;
		     GCPB->gcpb[ntracks].pid                       = 2212;
		     //printf("pid %d\n",identity);
		     GCPB->gcpb[ntracks].vert.x                    = x_close[ntracks];
		     //printf("x_close %f\n",x_close[ntracks]);
		     GCPB->gcpb[ntracks].vert.y                    = y_close[ntracks];
		     //printf("y_close %f\n",y_close[ntracks]);
		     GCPB->gcpb[ntracks].vert.z                    = z_close[ntracks];
		     //printf("z_close %f\n",z_close[ntracks]);
		     GCPB->gcpb[ntracks].dedx                      = visible_charge/vtl;
		     //printf("dQdx %f\n",visible_charge/vtl);
		     GCPB->gcpb[ntracks].p.x                       = p_x;
		     //printf("p_x %f\n",p_x);
		     GCPB->gcpb[ntracks].p.y                       = p_y;
		     //printf("p_y %f\n",p_y);
		     GCPB->gcpb[ntracks].p.z                       = p_para;
		     //printf("p_para %f\n",p_para);
		     GCPB->gcpb[ntracks].p_tot                     = dzds[ntracks];
		     //printf("p_tot %f\n",p_tot);
		     GCPB->gcpb[ntracks].x2                      = chi2[ntracks]; //for the helix fit
		     //GCPB->gcpb[ntracks].x2                        = calcChi(ntracks,track_chain[ntracks]); //for the straight line fit
		     //printf("chi2 %f\n",chi2[ntracks]);
		     GCPB->gcpb[ntracks].theta                     = track_theta;
		     //printf("track_theta %f\n",track_theta);
		     GCPB->gcpb[ntracks].q                         = visible_charge;
		     //printf("trackSum %f\n",visible_charge);
		     GCPB->gcpb[ntracks].dca                       = dca[ntracks];
		     //printf("dca %f\n",dca[ntracks]);
		     GCPB->gcpb[ntracks].index                     = 1; 
		     //printf("index %d\n",helix_error);
		     GCPB->gcpb[ntracks].phi                       = track_phi; 
		     //printf("track_phi %f\n",track_phi);
		     GCPB->gcpb[ntracks].vtl                       = vtl; 
		     //printf("vtl %f\n",vtl);
		     GCPB->gcpb[ntracks].sdist                     = sdist; 
		     //printf("sdist %f\n",sdist);
		     GCPB->gcpb[ntracks].edist                     = edist; 
		     //printf("edist %f\n",edist);
		     GCPB->gcpb[ntracks].npts                      = npts_tofit; 
		     //printf("npts %d\n",npts_tofit);
		     GCPB->gcpb[ntracks].r_0                       = r_0[ntracks]; 
		     //printf("r_0 %f\n",r_0[ntracks]);
		     /*GCPB->gcpb[ntracks].fiterr                    = helix_error; 
		     //printf("fiterr %d\n",helix_error);
		     GCPB->gcpb[ntracks].tothits                   = hh_num_hits; 
		     //printf("tothits %d\n",hh_num_hits);
		     GCPB->gcpb[ntracks].npd_track                 = npads_track;
		     //printf("npd_track %d\n",npads_track);
		     GCPB->gcpb[ntracks].npd_event                 = npads_hit;
		     //printf("npd_hit %d\n",npads_hit);
		     GCPB->gcpb[ntracks].bonus_bits                = bonus_bits;
		     //printf("bonus_bits %d\n",bonus_bits);
		     GCPB->gcpb[ntracks].q_tot                     = qtot;
		     //printf("qtot %f\n",qtot);*/
		   }//ends if there are still enough points to fit
		 else 
		   {
		     ntracks--;
		     //printf("not enough points after removal\n");
		   }
	       }//ends check if ntracks > EVT_TRACKS
	     ntracks++;
	     
	     //printf("Mark-1\n");	   
	   }//end of if(num_hits_this_chain[ii]>MIN_LINKS_ON_TRACK)
       }//end of loop over number of chains found
   }//end if(doFits)
   
   	if(ntracks==0) dropAllBanks(&bcs_, "GCPB");
 
	  } /* end good-altro-data block */
	      else
		{
		  num_bad_altro_data++;
		}
	

      }// end of read one event */

return;
 
}


