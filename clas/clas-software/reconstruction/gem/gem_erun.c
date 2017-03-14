/*---------------------gem_evnt.c------------------------*/
/* - one stop shopping for all GEM-related reconstruction
   - gets called every event (ideally before seb/pid type routines
   are
   Author: G. Niculescu, 2005
/*----------------------------------------------------------*/
#include "rtpc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

int gem_erun_(){
  
  float rat;
  printf("After reading %d events.\n",num_events);
  printf("**** %d events skipped because of not good altro data\n",num_bad_altro_data);
  printf(" trigger/bonus bits summary:\n");
  printf("     pulser events flagged by bonus_bits %8d\n",n_bb_pulser);
  printf("     pulser events flagged by trig_bits  %8d\n",n_tb_pulser);
  printf("     pulser events flagged by either     %8d\n",n_either_pulser);
  printf("     pulser events flagged by both       %8d\n",n_both_pulser);
  qtot_early/= (float)num_events*(TPC_TMIN);
  qtot_intime/= (float)num_events*(1+TPC_TMAX-TPC_TMIN);
  qtot_late/= (float)num_events*(nal_samp-TPC_TMAX);
  printf(" Q_EARLY: %f per event*time_bin\n",qtot_early);
  printf(" Q_INTIM: %f per event*time_bin\n",qtot_intime);
  printf(" Q_LATE : %f per event*time_bin\n",qtot_late);
  rat= 100.0*ntype[4]/num_events;
  printf(" NOISE/DISCHG:%5.0d %4.1f\%\n",ntype[4],rat);
  rat= 100.0*ntype[5]/num_events;
  printf(" GOOD_PROTON: %5.0d %4.1f\%\n",ntype[5],rat);

  rat= 100.0*ntype[1]/num_events;
  printf(" BAD_START:  %5.0d %4.1f\%\n",ntype[1],rat);
  printf(" --started far outside (percent of BAD_STARTs):%5.0d %4.1f\%\n",
	 strt_bfore_bound, 100.0*strt_bfore_bound/ntype[1]);
  printf(" --started far inside (percent of BAD_STARTs):%5.0d %4.1f\%\n",
	 strt_aftr_bound, 100.0*strt_aftr_bound/ntype[1]);

  rat= 100.0*ntype[2]/num_events;
  printf(" BAD_END:  %5.0d %4.1f\%\n",ntype[2],rat);
  printf(" --ended far inside (percent of BAD_ENDs):%5.0d %4.1f\%\n",
	 end_bfore_bound, 100.0*end_bfore_bound/ntype[2]);
  printf(" --ended far outside (percent of BAD_ENDs):%5.0d %4.1f\%\n",
	 end_aftr_bound, 100.0*end_aftr_bound/ntype[2]);

  rat= 100.0*ntype[3]/num_events;
  printf(" BAD_END&STRT:  %5.0d %4.1f\%\n",ntype[3],rat);
  printf(" --started and ended far inside (percent of BAD_END&STRT):%5.0d %4.1f\%\n",
	 siei, 100.0*siei/ntype[3]);
  printf(" --started and ended far outside:%5.0d %4.1f\%\n",
	 soeo, 100.0*soeo/ntype[3]);
  printf(" --started far inside and ended far outside:%5.0d %4.1f\%\n",
	 sieo, 100.0*sieo/ntype[3]);
  printf(" --started far outside and ended far inside:%5.0d %4.1f\%\n",
	 soei, 100.0*soei/ntype[3]);
  
  rat= 100.0*ntype[27]/num_events;
  printf(" NO EVENT:      %5.0d %4.1f\%\n",ntype[27],rat);
  rat= 100.0*ntype[26]/num_events;
  printf(" UNKNOWN:     %5.0d %4.1f\%\n",ntype[26],rat);
  
  printf("Total number of left candidates -> %d\n",leftCand); //nkb
  printf("Total number of right candidates -> %d\n",rightCand); //nkb
 return 0;
}
