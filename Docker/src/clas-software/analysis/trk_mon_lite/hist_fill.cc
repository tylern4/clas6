

#include "trk_mon_lite.h"

// int TOTAL_NTUPLE_ROWS=0;
// int MAX_NTUPLE_ROWS=485000;
int TOTAL_NTUPLE_ROWS[7]={0,0,0,0,0,0,0};
int MAX_NTUPLE_ROWS=100000;
float LOCANGLE_CUT_HIGH[4],LOCANGLE_CUT_LOW[4];
int N_LOCANGLE_EVENTS[3]={0,0,0};

int EnoughEvents(void);


/*  from bosddl.h */
/* ------------------------ tbla -----------------*/
/*typedef struct {
/*        int trk_pln;    /*  (track_number) *100 + Trk_plane_number */
/*        vector3_t pos;    /* coord [cm]  for track in this plane */
/*        vector3_t dir;    /*  direction cosine (cx) at coord.{x,y,z} */
/*        float tlen;    /*  track length [cm] from origin to this plane */
/*        int dc1;    /*  Pointer to DC1 bank */
/*        int stat;    /*  Status of the hit */
/*        int wire;    /*  Wire number  */
/*        float dtime;    /*  drift time  [ns] */
/*        float alpha;    /*  track angle (relative to R of SL) [deg] */
/*        float wlen;    /*  Wire length (hit pos. to preamp)  [cm] */
/*        float sgdoca;    /*  sigma DOCA  [cm] */
/*        float fitdoca;    /*  Fitted DOCA [cm] */
/*        float calcdoca;    /*  calculated DOCA (via dtime)  [cm] */
/*} tbla_t;
*/

/********************************************************************/
/* OK, here we need info from at least three banks:
/*
/* TBID -> beta
/* TBLA -> dtime, fitdoca, calcdoca, alpha, wire, B-field
/* TBTR -> phi,P
/*
/* The TBID bank has an overall track number, as well as the sector
/* the track was in.
/*
/* The TBLA bank has the overall track number, but is indexed by
/* the sector (getGroup(&bcs,"TBLA",sector)).
/*
/* The TBTR bank only has the track number within the sector and 
/* the sector, but is indexed by "track" in TBID if "track">0.
/* 
/*******************************************************************/
int hist_fill(void)
{
   float nt_array[32];
   int sup,layer;
   float *B;
   int i,j,NID;
   clasTBID_t *TBID = (clasTBID_t *)getGroup(&bcs_, "TBID",1);
   clasTBTR_t *TBTR = (clasTBTR_t *)getBank(&bcs_, "TBTR");
   clasTBER_t *TBER = (clasTBER_t *)getBank(&bcs_, "TBER");
   clasTBLA_t *TBLA = NULL, *TBLAS[7]={NULL,NULL,NULL,NULL,NULL,NULL,NULL};
   tbid_t *tbid;
   tbtr_t *tbtr;
   tbla_t *tbla;
   float phi,P,px,py,pz;
   vector3_t *pvec;
   float q,chisq;
   int hit; 
   float c_time;
   float time,beta;
   int sec, imax;
   int sec_t,sup_t;
   float time_t,timec_t,beta_t;

   // Stop now if we have enough statistics
	if(EnoughEvents())return 1;

	if(!TBID)TBID=(clasTBID_t *)getBank(&bcs_, "TBID");
  	if(TBID==NULL || TBTR==NULL)return 0;
	for(sec=1;sec<=6;sec++)
	  TBLAS[sec] = (clasTBLA_t *)getGroup(&bcs_, "TBLA",sec);


	if(!LOCAL_ANGLE_HISTOS_ONLY)hcdir("//esr"," ");
	hcdir("//PAWC"," ");

	tbtr=TBTR->tbtr;
	for (int itr=0; itr<TBTR->bank.nrow; itr++,tbtr++){
	  sec=tbtr->itr_sec/100; 
	  int itsec=tbtr->itr_sec%100;
	/*--- Loop over TBLA banks ---*/
	  if(!TBLAS[sec]) continue;
	  if( (imax=TBLAS[sec]->bank.nrow) < itsec*34 || imax<34) {
	    printf("TBLA missing in sec %d for track %d (%d)\n",sec,itsec,itr);
	    continue;
	  }
	  tbla = &TBLAS[sec]->tbla[imax-34];
	  for(i=imax-34;i<imax;i++,tbla++){
	    if(tbla->stat!=0 || tbla->dc1 <= -1)continue;
	    int sup= ((tbla->trk_pln)%100 - 4)/6 + 1;
	    if(!LOCAL_ANGLE_HISTOS_ONLY){
	      int offset =  10000*sec + 1000*sup;
	      hf2(2+ offset, tbla->dtime , tbla->fitdoca,1);
	    }
	    int reg = ((sup+1)/2);
	    float localangle = tbla->alpha*180.0/3.14159;
	    hf1(4000 + reg, localangle, 1);
	    N_LOCANGLE_EVENTS[reg-1]++;
	    
	    // fill residual histogram
	    float resi = fabs(tbla->fitdoca)-fabs(tbla->calcdoca);
	    int RPID=500+100+(sec*10)+sup;
	    hf1(RPID, resi, 1);
			// fill "cut" localangle histo
	    if( PROTON_CUT && tbtr->q < 0 ) continue;
	    if( tbtr->vert.z < VERTEX_CUT_LOW || tbtr->vert.z > VERTEX_CUT_HIGH ) continue;
	    if(localangle<LOCANGLE_CUT_LOW[reg] || localangle>LOCANGLE_CUT_HIGH[reg]) continue;
	    hf1(5000 + reg, localangle, 1);
	    
	  }
	}
	
	// return here is we're only producing local angle histograms
	if(LOCAL_ANGLE_HISTOS_ONLY)return 0;
	
	/*--- Loop over TBER banks ---*/
	if(TBER){
		tber_t *tber=TBER->tber;
		for(int i=0;i<TBER->bank.nrow;i++, tber++){
			// get sector
			int sec=tber->layinfo2>>24;
			
			// count hits per superlayer and add to histograms
			int nslhits[7];
			for(int j=1;j<=6;j++)nslhits[j]=0;
			for(int j=0;j<30;j++){
				int sl=1+(j/6);
				nslhits[sl] += (tber->layinfo1>>j)&0x1;
			}
			for(int j=0;j<6;j++){
				int sl=6;
				nslhits[sl] += (tber->layinfo2>>j)&0x1;
			}
			for(int sup=1;sup<=6;sup++){
				int offset =  10000*sec + 1000*sup;
				hf1(7+ offset, nslhits[sup], 1);
      	}
			
			// Chisq histogram
			hf1(sec+2000, tber->chi2, 1);
		}
	}

   /*--- Loop over TBID banks ---*/
   for(i=0;i<TBID->bank.nrow;i++){
      tbid=&TBID->tbid[i];

      if(tbid->track<1)continue;

      tbtr=&TBTR->tbtr[tbid->track - 1];
      if( tbtr->vert.z < VERTEX_CUT_LOW || tbtr->vert.z > VERTEX_CUT_HIGH ) continue;
      
      /*-- loop over tracks in sector to find right set of TBLA banks --*/
      TBLA=(clasTBLA_t *)getGroup(&bcs_, "TBLA",tbid->sec);
      if(TBLA==NULL)continue;
      tbla=NULL;
      for(j=0;j<TBLA->bank.nrow;j+=34){
         if((TBLA->tbla[j].trk_pln/100) == tbid->track){
            tbla=&TBLA->tbla[j];
            break;
         }
      }
      if(!tbla){
         /*fprintf(stderr,"Unable to find TBT track in TBLA banks!\n");*/
         return 0;
      }
      
      /*----------------------------------------------------------------*/
      /* at this point the tbid,tbtr, and tbla pointers should be valid */
      /*----------------------------------------------------------------*/

      pvec=&tbtr->p;
      px=pvec->x;  py=pvec->y;  pz=pvec->z;

      P=sqrt((px*px) + (py*py) + (pz*pz));
      
      q=tbtr->q;
      chisq=tbtr->chi2;

      /*NEW METHOD. 12/15/00 -JL */
      phi = atan2(py,px)*(180.0/3.1415);
      if(phi<0.0){
			phi+=360.0;
      }

      hfill(1890,P,tbid->beta,1.);

      if( !SINGLE_SECTOR || tbid->sec==SINGLE_SECTOR ){
      /*--- loop over tbla rows ---*/
      
      for(j=0;j<34;j++){

         if(tbla->dtime<-50.0 || tbla->dtime>3000.0){tbla++;continue;}

         layer=(tbla->trk_pln)%100 - 3;
         sup=((layer-1)/6) + 1;
         if(sup>6 || sup<1){tbla++;continue;}
	 if(tbid->beta <=0.2 || tbid->beta > 1.2) { tbla++;continue;}
         B=(float*)&tbla->dir; /* changed to hold B-field some time ago by Franz */

			nt_array[0] =tbid->sec;
			nt_array[1] =layer;
			nt_array[2] =tbla->wire;
			nt_array[3] =tbla->dtime;
			nt_array[4] =tbla->fitdoca;
			nt_array[5] =fabs(tbla->fitdoca)-fabs(tbla->calcdoca);
			nt_array[6] =sqrt((B[0]*B[0]) + (B[1]*B[1]) + (B[2]*B[2]));
			nt_array[7] =B[0];
			nt_array[8] =B[1];
			nt_array[9] =tbid->beta;
			nt_array[10]=phi;
			nt_array[11]=tbla->alpha*180.0/3.14159;
			nt_array[12]=P;
			nt_array[13]=q;
			nt_array[14]=chisq;
			nt_array[15]=tbla->stat;
			nt_array[16]=tbla->dc1;
			nt_array[17]=tbla->calcdoca;
	 
			c_time=0.0;
			time = tbla->dtime;
			sec = tbid->sec;
			beta = tbid->beta;
	
			c_time = dc_time_correction_(&time,&layer,&sec,&beta);
                        nt_array[18]=c_time;// PLC I think this was meant 
                        // to be.  Otherwise the ntuple shows a const ~2 
                        // for ctime.

			hfill(1891,P,tbid->beta,1.);
          /* Check command-line cuts and write out event if passed */
			if(TOTAL_NTUPLE_ROWS[sup]<MAX_NTUPLE_ROWS) {
         if(tm_pass_ntuple_cut(nt_array)){
            hcdir("//esr/TBT/proton"," ");
            hcdir("//PAWC/TBT/proton"," ");
            NID=1000+sup;
            hfn(NID,nt_array);
            hcdir("//esr"," ");
            hcdir("//PAWC"," ");
            TOTAL_NTUPLE_ROWS[sup]++;
         }
			}
			tbla++;
         
      }/* end of loop over TBLA bank rows (34 of them) */
      } /* end SINGLE_SECTOR */
   } /* end of loop over TBID->bank.nrow */

   return 0;
}

//----------------
// EnoughEvents
//----------------
int EnoughEvents(void)
{
	// There are a few possibilities here:
	//
	// 1. We're auto-finding local angles and are only filling the
	// local angle histos in memory.
	//
	// 2. We're only filling local angle histos, but NOT as first pass
	// for auto-finding.
	//
	// 3. We're filling Ntuples

	if(LOCAL_ANGLE_HISTOS_ONLY){
		if(!AUTOFIND_LOCAL_ANGLE_CUTS)return 0;
		
		// All 3 regions must have at least 50k events in histo to stop early
		for(int reg=1;reg<=3;reg++)if(N_LOCANGLE_EVENTS[reg-1]<50000)return 0;
		
		cout<<endl<<"Local angle histograms filled."<<endl;
		return 1;
	}

   // Make sure we don't overfill the ntuples
	int sum=0;
	for(int sup=1; sup<=6; sup++) sum +=TOTAL_NTUPLE_ROWS[sup];
   if(sum >= MAX_NTUPLE_ROWS*6 ){
     //   if(TOTAL_NTUPLE_ROWS>=MAX_NTUPLE_ROWS){
      cout<<" Maximum number of Ntuple rows per superlayer filled ("<<MAX_NTUPLE_ROWS<<")."<<endl<<endl;
      return 1;
   }
	
	return 0;
}
