
/*
 * Read in calibration constants used by tagM
 * 
 */

#include <tag_cpp_wrapper.h> /* CLAS include file needed for coping from TCL/COMMON described in note below */

#include <tagM.h>

/* Declaration of Globals used to hold calibration constants				  */
float EID_TIME_OFFSETS[384];		/* align E-counters in ns					  */
float TID_TIME_OFFSETS[61];		/* align T-counters in ns (multi-hit)	  */
float E2T_TIME;						/* time difference between E and T cntrs */
float EID2NS,TID2NS;					/* Convert E-counter and T-counter TDC	  */
											/* counts to nanoseconds respectively	  */
float EE_TIME_WINDOW;				/* half of timing window size for adjacent E-cntrs in ns	*/
float TT_TIME_WINDOW;				/* half of timing window size for adjacent T-cntrs in ns	*/
float ET_TIME_WINDOW;				/* half of timing window size for E and T cntrs in ns		*/

tagE_boundaries_t	tagE_boundaries[767];
tagETgeom_t			tagETgeom[384];
tagT_boundaries_t	tagT_boundaries[61];
float TAGM_EBEAM_ENERGY; /* Electron beam energy (GeV) */


#define TAGM_CALIB_BUFF_SIZE (1024*1024)


int tagM_brun(int runnumber)
{
	int i;
	char *parms_path, map_file[256], fname[256];
	int err;
	int map_run;
	FILE *file;
	char *buff,*ptr,*line;
	float bestE, E;
	clasRUNC_t *RUNC=getBank(&wcs_, "RUNC");


	fprintf(stdout,"TAGM: Initializing with run number %d\n",runnumber);

	/*=========================== Copy from TCL mapped COMMON block =========================*/
	/* This is less than optimal, but neccessitated by the need for backwards compatibility
	 * with the CLAS tag package. The CLAS tag package already has values for the E-E, T-T,
	 * and E-T coincidence window size in a COMMON block and linked to TCL variables. We want
	 * to recycle those here. On the other hand, I'm hoping to use the tagM package for analysis
	 * of PrimEx data as well. This benefits from keeping the tagM package as independant from
	 * CLAS software as much as possible (probably a futile gesture, I know). Anyway, here we
	 * initialize the values with the same ones in tagtcl_set_def.F, then immediately overwrite
	 * them with those from the COMMON block. I don't believe anyone actually uses the TCL
	 * variables, so the COMMON block values will almost surely have the same values as are
	 * listed here. I define them first here to make it a little easier to just cut out the
	 * COMMON block references whenever this gets converted into PrimEx code.
	 * 
	 * One other note: Since these values are set only when tagM_brun is called, any changes to 
	 * the tcl variables between calls will not be used until this routine is called again.
	 * This is different from the tag package which would immediately implement the change.
	 */
	TT_TIME_WINDOW = 10.0/2.0;
	EE_TIME_WINDOW = 20.0/2.0;
	ET_TIME_WINDOW = 20.0/2.0;

	TT_TIME_WINDOW = tagparam_.ADJ_T_COINC/2.0;
	EE_TIME_WINDOW = tagparam_.ADJ_E_COINC/2.0;
	ET_TIME_WINDOW = tagparam_.ET_window/2.0;
	

	/*================================= Read from Map/CalDB =================================*/

	/* Get full pathname to Map file (Note: this is not entirely neccessary since we now
	 * have the database. It's easy enough to maintain backward compatibility with the
	 * map though.
	 */ 
	parms_path=(char*)getenv("CLAS_PARMS");
	sprintf(map_file,"%s/Maps/TAG_CALIB.map", parms_path ? parms_path:"/group/clas");
	
	/* Conversion of TDC channels to nanoseconds.
	 * As of now (11/12/01) the map seems only to contain a value of 500.0 for this
	 * parameter. I guess this means the nominal 1/2 ns per bin is always used for
	 * the LRS1877's and the tag package likes to work in picoseconds :) We'll go ahead
	 * and read this from the map just to be consistent and in case it ever changes.
	 */
	err = map_get_float(map_file,"tag_e","slope",1,&EID2NS,runnumber,&map_run);
	EID2NS /=1000.0; /* map value converts to pico seconds. we want nanoseconds. */
	TID2NS = EID2NS; /* T-counters are also 1877's */

	/* TDC offsets to align E-counter TDCs with each other.
	 * The map holds floating point values in nanoseconds for the E-counter offsets.
	 */
	err = map_get_float(map_file,"tag_e","dt",384,EID_TIME_OFFSETS,runnumber,&map_run);

	/* TDC offsets to align T-counter TDCs with each other.
	 * The map holds floating point values in nanoseconds for the T-counter offsets.
	 * (THIS STILL NEEDS TO BE ADDED TO THE MAP/CALDB !!!!)
	 */
	for(i=0;i<61;i++)TID_TIME_OFFSETS[i]=0.0;
	err = map_get_float(map_file,"tag_t","dt_multihit",61,TID_TIME_OFFSETS,runnumber,&map_run);

	/* Time difference between E-counter and T-counters in nanoseconds.
	 * Ttcounter = Tecounter + E2T_TIME
	 * (THIS STILL NEEDS TO BE ADDED TO THE MAP/CALDB !!!!)
	 */
	E2T_TIME = -677.0; /* eyeballed from G6c data until we get a real constant in the map */
	err = map_get_float(map_file,"tag_e","e2t",1,&E2T_TIME,runnumber,&map_run);

	/* Get beam energy from RUNC bank so we know which table to read in */
	if(RUNC){
		TAGM_EBEAM_ENERGY = RUNC->runc.beam.energy.val.f[0]; /* already in GeV */
	}else{
		fprintf(stderr,"%s:%d RUNC bank not present. I REALLY, REALLY, need it to do this!\n",__FILE__,__LINE__);
		exit(-1);
	}



	/*================================= Read from files in parms area =================================*/
	
	/* allocate a large block temporarily so each file can be read in its entirety */
	buff = malloc(TAGM_CALIB_BUFF_SIZE); /* largest file is 411,258 bytes so 1MB should be plenty of overkill */
	if(!buff){
		fprintf(stderr,"%s:%d Unable to allocate memory!\n",__FILE__,__LINE__);
		exit(-1);
	}

	/*---- tagE_boundaries ---*/
	sprintf(fname,"%s/tagE-boundaries_ALL.dat",parms_path);
	file=fopen(fname,"r");
	if(!file){
		fprintf(stderr,"%s:%d Unable to find file \"%s\".\n",__FILE__,__LINE__,fname);
	} else {
		fprintf(stdout,"TAGM: Reading file \"%s\"",fname);
		bestE=-1000.0;
		memset(buff,0,TAGM_CALIB_BUFF_SIZE);
		fread(buff,1,TAGM_CALIB_BUFF_SIZE,file);
		fclose(file);
		ptr=buff;
		while(ptr=strstr(ptr,"\n")){*ptr=0;ptr++;} /* replace CRs with NULLs */
		line=buff;
		do{
			if(ptr=strstr(line,"E0")){
				if(!sscanf(ptr,"E0 = %f",&E))continue;
				if(fabs(E-TAGM_EBEAM_ENERGY)<=fabs(bestE-TAGM_EBEAM_ENERGY)){
					tagE_boundaries_t b;
					
					bestE=E;
					b.Echan=0;
					do{
						if(!strlen(line=&line[strlen(line)+1]))break;
						if(line[0]=='#')continue;
						sscanf(line," %d %f %f %f %f",&b.Echan,&b.kmin,&b.kmax,&b.kavg,&b.ksdev);
						if(b.Echan>=1 && b.Echan<=767)tagE_boundaries[b.Echan-1]=b;
					}while(b.Echan<767);
				}
			}
		}while(strlen(line=&line[strlen(line)+1]));
		fprintf(stdout,":tableE=%4.2f GeV\n",bestE);
	}


	/*---- tagT_boundaries ---*/
	sprintf(fname,"%s/tagT-boundaries_ALL.dat",parms_path);
	file=fopen(fname,"r");
	if(!file){
		fprintf(stderr,"%s:%d Unable to find file \"%s\".\n",__FILE__,__LINE__,fname);
	} else {
		fprintf(stdout,"TAGM: Reading file \"%s\"",fname);
		bestE=-1000.0;
		memset(buff,0,TAGM_CALIB_BUFF_SIZE);
		fread(buff,1,TAGM_CALIB_BUFF_SIZE,file);
		fclose(file);
		ptr=buff;
		while(ptr=strstr(ptr,"\n")){*ptr=0;ptr++;} /* replace CRs with NULLs */
		line=buff;
		do{
			if(ptr=strstr(line,"E0")){
				if(!sscanf(ptr,"E0 = %f",&E))continue;
				if(fabs(E-TAGM_EBEAM_ENERGY)<=fabs(bestE-TAGM_EBEAM_ENERGY)){
					tagT_boundaries_t b;
					
					bestE=E;
					b.Tctr=0;
					do{
						if(!strlen(line=&line[strlen(line)+1]))break;
						if(line[0]=='#')continue;
						sscanf(line," %d %f %f %f",&b.Tctr,&b.kmin,&b.kminfirst,&b.kmax);
						if(b.Tctr>=1 && b.Tctr<=61)tagT_boundaries[b.Tctr-1]=b;
					}while(b.Tctr<61);
				}
			}
		}while(strlen(line=&line[strlen(line)+1]));
		fprintf(stdout,":tableE=%4.2f GeV\n",bestE);
	}


	/*---- tagETcoinc ---*/
	sprintf(fname,"%s/tagETcoinc_ALL.dat",parms_path);
	file=fopen(fname,"r");
	if(!file){
		fprintf(stderr,"%s:%d Unable to find file \"%s\".\n",__FILE__,__LINE__,fname);
	} else {
		fprintf(stdout,"TAGM: Reading file \"%s\"",fname);
		bestE=-1000.0;
		memset(buff,0,TAGM_CALIB_BUFF_SIZE);
		fread(buff,1,TAGM_CALIB_BUFF_SIZE,file);
		fclose(file);
		ptr=buff;
		while(ptr=strstr(ptr,"\n")){*ptr=0;ptr++;} /* replace CRs with NULLs */
		line=buff;
		do{
			if(ptr=strstr(line,"E0")){
				if(!sscanf(ptr,"E0 = %f",&E))continue;
				if(fabs(E-TAGM_EBEAM_ENERGY)<=fabs(bestE-TAGM_EBEAM_ENERGY)){
					tagETgeom_t b;
					
					bestE=E;
					b.Ectr=0;
					do{
						if(!strlen(line=&line[strlen(line)+1]))break;
						if(line[0]=='#')continue;
						sscanf(line," %d %d %d %d %d %d %d",&b.Ectr,&b.Tmin,&b.Tmax,&b.T1min,&b.T1max,&b.TCHANmin,&b.TCHANmax);
						if(b.Ectr>=1 && b.Ectr<=384)tagETgeom[b.Ectr-1]=b;
					}while(b.Ectr<384);
				}
			}
		}while(strlen(line=&line[strlen(line)+1]));
		fprintf(stdout,":tableE=%4.2f GeV\n",bestE);
	}

	/* free up the memory we don't need any more */
	free(buff);


	fprintf(stdout,"TAGM: Initialization complete.\n");

}


/* 
 * Here is the note at the top of the tagcal_read_file.F from the CLAS software
*/

/*
c
c_begin_doc
c  RCS ID string
c  $Id: tagM_brun.c,v 1.4 2002/05/22 18:54:55 liji Exp $X%
c
c  Documentation for subroutine tag_read_bounds
c
c  Purpose: 
c  --------
c	Attempts to read boundaries energies from files in CLAS_PARMS area
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  where   (int): ( 1 = local, otherwise CLAS_PARMS)
c  ----------------   energy (real): beam energy in Gev
c
c  Output Parameters:  ok (boolean) things worked fine
c  -----------------
c
c  Called from: tagcal_read_file AND tagcal_read_local_file
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes: Modification of may 1999
c  ------
c  Dan Sober Notes:
c
c
c 1.  INTRODUCTION
c
c The energy calibration of the tagger changes slowly as a function of E0,
c because the magnetic field at the full-energy orbit decreases relative to the
c field at the center of the pole as the magnet approaches saturation.
c The effect is small (always less than 3 fine channels of ~.001 E0) at energies 
c below 4.1 GeV, but increases rapidly at higher energies, as shown in Table 1.
c The effect is largest for low photon energies, where the energy of the
c detected  electron is a large fraction of E0.
c 
c --------------------------------------------------------------------------------
c   Table 1.  Values of k/E0 vs. fine channel for 5 different energies
c             (channel upper limit with no angular smearing).
c             
c    E0 =    0.90 GeV     2.39 GeV        4.14 GeV        5.56 GeV        6.12 GeV
c E channel  
c  100       .85978       .86042          .86058          .86035          .85966
c  200       .76309       .76357          .76361          .76204          .76047
c  300       .66681       .66682          .66664          .66356          .66121
c  400       .57056       .56990          .56941          .56484          .56177
c  500       .47424       .47285          .47209          .46616          ,46244
c  600       .37701       .37523          .37427          .36722          .36312
c  700       .27888       .27710          .27608          .26841          .26404
c --------------------------------------------------------------------------------
c 
c 
c The calculations used in creating these files consist of the following steps:
c (1) For each of the 10 field maps, calculate a file of trajectories by 
c     ray-tracing using the code SNAKE (actually my modification of an old
c     version of SNAKE.)  The present version of the trajectory file consists
c     of 235 rays (47 energies times 5 angles per energy).  I have tested the
c     energy steps and confirmed that interpolation between these energies
c     leads to energy errors less than 5E-5, or 0.05 fine channel.
c (2) Using the 235-ray files, run my tagger Monte Carlo code in two separate
c     modes:
c     (a) uniform energy steps (< 1.E-6 E0), no bremsstrahlung angle, no
c         multiple scattering -- to determine energy boundaries of counters
c         and channels.
c     (b) Monte Carlo energy distribution (as 1/k), bremsstrahlung and
c         multiple-scattering angular distributions enabled -- to determine
c         correlations between counters.
c 
c 
c 
c The data format of the new files, and my suggestions for reading them,
c follow below.
c 
c tagT-boundaries.nnnn:
c 
c    #     Tctr      kmin kminfirst      kmax   ! Last of 6 comment lines
c             1   0.94476   0.95347   0.95347   ! format(i10,3f10.5)
c             2   0.93500   0.94476   0.94570
c           ...
c           
c           (kminfirst is the minimum k for which this T counter is the first
c           one hit.)
c 
c       do i=1,61
c  1000  read(Ilun,'(a)') line                ! read 80-character line
c        if(index(line,'#').ne.0) go to 1000  ! skip lines with '#'       
c        read (line,'(i10,3f10.5)')) itest,Tboundary(2*i+1),dummy_real,
c      1  Tboundary(2*i-2)
c        if(itest.ne.i) go to 5000            ! test for data corruption
c       end do
c 
c 
c tagE-boundaries.nnnn:
c 
c    # Echan    kmin      kmax      kavg     ksdev   ! Last of 6 comment lines
c        1   0.95169   0.95322   0.95246   0.04434   ! format(i5,4f10.5)
c        2   0.95097   0.95169   0.95133   0.02080
c        ...
c 
c       do i=1,767         
c 1000   read(iunit,'(a)') line               ! read 80-character line
c         if(index(line,'#').ne.0) go to 1000  ! skip lines with '#'
c         read(line,'(i5,4f10.5)') itest,Eboundary(i+1),dummy,Eaverage(i),
c      1   Edeviation(i)
c         if(itest.ne.i) go to 5000            ! test for data corruption
c       end do
c 
c 
c tagETcoinc.nnnn:
c    # Ectr Tmin Tmax T1min T1max TCHANmin TCHANmax  ! Last of 6 comment lines
c        1    1    1    1    1    1    1             ! format(7i5)
c        2    1    1    1    1    1    1
c      ...
c 
c       (T1min and T1max are the first and last T's for which that T is the
c       first one in coincidence with the E.  TCHANmin and TCHANmax refer to
c       the T-channels 1-121.)
c 
c       do i=1,384
c  1000  read(Ilun,'(a)') line                  ! read 80-character line
c        if(index(line,'#').ne.0) go to 1000    ! skip lines with '#'
c        read(line,'(7i5)') itest,tmin(i),tmax(i)
c        if(itest.ne.i) go to 5000              ! test for data corruption
c        tbinmin(tag_bin(i,1)) = tag_bin(tmin(i),1)
c        if(i.lt.384) tbinmin(tag_bin(i,2)) = tag_bin(tmin(i),1)
c       end do
c 
c 

c  Energy boundaries of T counters         D. Sober          16 June 1997
c
c  Description of T boundaries read file:
c
c  The energies in the table are always expressed as k/E0 =
c  (photon energy)/(incident electron energy).
c
c  There is  ~20% overlap between the T-counters, so the important
c  energy boundaries are those defining the energies for which a given
c  counter is the FIRST one hit, i.e. k_min and k_max1 in the table.
c  k_max is the maximum value of k/E0 for which the electron hits the
c  counter, but the electron corresponding to k=k_max will always (except
c  for T1) hit the previous counter as well.
c
c    k/E0=           electron path
c   .9227 ------------------------------+----->
c                                       |
c   .9349 -----------------------+------|--->
c                                |     .9356
c   .9446 ----------------+------|-->
c                         |     .9455
c   .9534 ----------------+-->
c                         T1     T2     T3
c
c  ====================================================================
c  T-counter energy boundaries for first T-counter hit
c
c  Calculated without multiple scattering or angular distribution
c  E0 =  2.9800      Rays from file = SN10.OUT     5-May-97  13:07:05
c
c     T_ctr     k_min     k_max1    k_max   (k_max1 = max. k for 1st T)
c         1     .9446     .9534     .9534
c
c   Description of E boundaries read file:
c
c
c E counter channel information            D. Sober          16 June 1997
c
c Monte Carlo output at E0 = 2.98 GeV with brems. angular distribution and
c multiple scattering disabled (i.e. giving nominal channel boundaries),
c and with photon energies generated uniformly in steps of .00001 E0.
c
c E-counter channels:
c      channel 1 = E1 .and..not. E2
c      channel 2 = E1*E2
c      channel 3 = E2 .and..not. E1 .and..not. E3
c      channel 4 = E2*E3
c
c For integer n = 1 to 384:  Channel 2n = coincidence E_n ^ E_n+1
c                            Channel 2n-1 = counter E_n anticoincidencec
c
c Table columns:
c Chan       E-counter channel number 1-767, defined as above
c #          number of events in channel
c >2_E       no. of events in channel for which more than 2 E counters hit
c No_T       no. of events in channel for which no T counters hit
c 2_T        no. of events in channel for which 2 or more T counters hit
c #Dbl       no. of events in "Double Channel" 2n = sum of channel 2n + 2n-1
c            (Note: the Double Channel is probably the cleanest way to
c            view data, since it does not alternate in width like the
c            odd-even single channels)
c k_avg      mean value of k/E0
c k_sd%      standard deviation of k/E0 in percent of E0
c k_min      minimum value of k/E0 in channel
c k_max      maximum value of k/E0 in channel
c
c ============================================================================
c E0 =  2.9800      Rays from file = SN10.OUT               5-May-97  13:07:05
c Energy from  .19000 to  .96000, step  .00001, number    .7700E+05
c Angle  from   .0000 to   .0000, step  .00000
c
c Chan     #  >2_E  No_T   2_T    #Dbl    k_avg   k_sd%   k_min   k_max
c    1   156     0     0     0           .95234  .04503  .95157  .95312
c
c
c
c RESULTING ARRAY definitions (stored in tagcal.CMN) :
c  !!!!REMEMBER lower i ID = higher k/E (k = photon energy, E = beam energy)
c    i.e: the max k/E value of bin i is the minimum k/E value for bin i-1 !!
c
c  T-bondary(i) is the max k/E for bin i
c    this is why there is 122 channels because to have the minimum value of k/E of bin 121 we read the
c    maximum value of a virtual bin 122.
c
c  E-boundary(i) is the max  k/E for bin i
c      min value for bin 767 is given by max value of virtual bin 768.
cc
c  Author:   Dan Sober & Eric Anciant      Created:  May 1999
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
*/


