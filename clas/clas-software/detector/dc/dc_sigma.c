



#include <dc.h>

float DC_SIGMA_MAP_R1[50];
float DC_SIGMA_MAP_R2[50];
float DC_SIGMA_MAP_R3[50];

/*==============================================================*
 * Initialize the DC sigma routines.
 * This just reads in the values from the map and uses them
 * to fill the sigma table
/*==============================================================*/
void dc_sigma_init(int runno)
{
   char mapfile[256]="DC_DOCA.map";
   char *ptr;
   int firsttime;
   int sup, k;
   float rdoca;
   int err;

	/* Read in Map parms */
   if(ptr=getenv("CLAS_PARMS"))sprintf(mapfile,"%s/Maps/DC_DOCA.map",ptr);
   
	err=map_get_float(mapfile,"RMS","Region1",50,DC_SIGMA_MAP_R1,runno,&firsttime);
	err=map_get_float(mapfile,"RMS","Region2",50,DC_SIGMA_MAP_R2,runno,&firsttime);
	err=map_get_float(mapfile,"RMS","Region3",50,DC_SIGMA_MAP_R3,runno,&firsttime);
	if(err!=0){
		fprintf(stderr,"DC: Error reading RMS values from the map. Make sure your parms\n");
		fprintf(stderr,"DC: area is up-to-date. (contact davidl@jlab.org for more help)\n");
		exit(-1);
	}
	fprintf(stderr,"DC: RMS residual values read (%d)\n",firsttime);

	/*--- Fill Table ---*/
	/* R1 */
	for(k=0;k<n_rdoca_bins;k++)
		DC_SIGMA_TABLE[k][0]
		=DC_SIGMA_TABLE[k][1]=DC_SIGMA_MAP_R1[k];
	/* R2 */
	for(k=0;k<n_rdoca_bins;k++)
		DC_SIGMA_TABLE[k][2]
		=DC_SIGMA_TABLE[k][3]=DC_SIGMA_MAP_R2[k];
	/* R3 */
	for(k=0;k<n_rdoca_bins;k++)
		DC_SIGMA_TABLE[k][4]
		=DC_SIGMA_TABLE[k][5]=DC_SIGMA_MAP_R3[k];

}


/*=========================================================================*
 * After writing the routines below, I don't think are really
 * neccessary since I don't know if anything outside of the
 * DC package ever calls them. They basically made for a complicated
 * scheme of copying the map parameters into the COMMON block.
 * I will comment them out for now until I'm sure they aren't needed.
 /*=======================================================================*/

#if 0

/*====================================================*
 * For backward compatibility.
 * "rdoca" is normalized so it is bewtween 0 and 1
/*====================================================*/
float dc_sigma(int sup,float rdoca)
{
	float dc_sigma;
	int rdoca_bin;
	
	rdoca_bin = (int)floor(rdoca*50.0);
   if (rdoca_bin.GT.NPOINTS) rdoca_bin=NPOINTS
      
	switch(sup){
		case 1:
		case 2:
			dc_sigma = DC_SIGMA_MAP_R1[rdoca_bin];
			break;
		case 3:
		case 4:
			dc_sigma = DC_SIGMA_MAP_R2[rdoca_bin];
			break;
		case 5:
		case 6:
			dc_sigma = DC_SIGMA_MAP_R3[rdoca_bin];
			break;

	}

	return dc_sigma;
}

/*==================================*
 * Fortran wrapper for above
/*==================================*/
float dc_sigma_(int *sup,float *rdoca)
{
	return dc_sigma(*sup,*rdoca);
}

#endif



