

#include "dc3.h"




int QualityCheck(ClientData clientData,Tcl_Interp *interp,int argc, const char *argv[])
{
   int sec,sup;
   char cmd[256];
   float sigma_n[7][7],sigma_w[7][7],mean[7][7],amp_ratio[7][7];
   int RID,RPID;
   float hpt[7],chisq[7];
   static int working=0;
   float tot_width,s1,s2,R;
   
   if(working)return TCL_OK;
   working=1;

   Tcl_Evaluate("QualitySummary ; update ; update");
   cout<<interp->result<<"\n";
   
   // Hits per TBT
   FindHitsPerTBT(hpt);
   for(sec=1;sec<=6;sec++){
      sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 8.4 %d 1 %1.2f #000000"
         ,sec,hpt[sec]);
      Tcl_Evaluate(cmd);
   }
   sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 8.4 %d 1 %1.2f #000000"
      ,7,hpt[0]);
   Tcl_Evaluate(cmd);

   // Avg tracking chisq
   FindAvgTrackingChisq(chisq);
   for(sec=1;sec<=6;sec++){
      sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 9.0 %d 1 %1.2f #000000"
         ,sec,chisq[sec]);
      Tcl_Evaluate(cmd);
   }
   sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 9.0 %d 1 %1.2f #000000"
      ,7,chisq[0]);
   Tcl_Evaluate(cmd);

   // Initialize arrays
   for(sup=0;sup<=6;sup++){
      sigma_n[0][sup]=0.0;
      sigma_n[sup][0]=0.0;
      sigma_w[0][sup]=0.0;
      sigma_w[sup][0]=0.0;
      mean[0][sup]=0.0;
      mean[sup][0]=0.0;
      amp_ratio[0][sup]=0.0;
      amp_ratio[sup][0]=0.0;
   }
   
   // narrow/wide residuals
   for(sup=1;sup<=6;sup++){
      for(sec=1;sec<=6;sec++){
         
         // Make sure histogram is filled
         sprintf(cmd,"set sup %d ; set sec %d ; update ; update",sup,sec);
         Tcl_Evaluate(cmd);
      
         RID=100+sup+(sec*10);
         RPID=RID+500;

         // calculate sigmas
         FindResidualWidths(RPID,&sigma_n[sec][sup],&sigma_w[sec][sup]
            						  ,&mean[sec][sup],&amp_ratio[sec][sup]);
         
         sigma_n[sec][ 0 ]+=sigma_n[sec][sup]/6.0;
         sigma_n[ 0 ][sup]+=sigma_n[sec][sup]/6.0;
         sigma_n[ 0 ][ 0 ]+=sigma_n[sec][sup]/36.0;
         sigma_w[sec][ 0 ]+=sigma_w[sec][sup]/6.0;
         sigma_w[ 0 ][sup]+=sigma_w[sec][sup]/6.0;
         sigma_w[ 0 ][ 0 ]+=sigma_w[sec][sup]/36.0;
         mean[sec][ 0 ]+=mean[sec][sup]/6.0;
         mean[ 0 ][sup]+=mean[sec][sup]/6.0;
         mean[ 0 ][ 0 ]+=mean[sec][sup]/36.0;
         amp_ratio[sec][ 0 ]+=amp_ratio[sec][sup]/6.0;
         amp_ratio[ 0 ][sup]+=amp_ratio[sec][sup]/6.0;
         amp_ratio[ 0 ][ 0 ]+=amp_ratio[sec][sup]/36.0;

         tot_width = TotalWidth(amp_ratio[sec][sup], sigma_n[sec][sup], sigma_w[sec][sup]);
      
         sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 1.2 %d %d %d %s"
            ,sup,sec,(int)sigma_n[sec][sup]
            ,sigma_n[sec][sup]>310.0 ? "#FF0000":"#000000");
         Tcl_Evaluate(cmd);
         sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 3.0 %d %d %d %s"
            ,sup,sec,(int)sigma_w[sec][sup]
            ,sigma_w[sec][sup]>900.0 ? "#FF0000":"#000000");
         Tcl_Evaluate(cmd);
         sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 4.8 %d %d %d %s"
            ,sup,sec,(int)mean[sec][sup]
            ,fabs(mean[sec][sup])>50.0 ? "#FF0000":"#000000");
         Tcl_Evaluate(cmd);
         sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 6.6 %d %d %d %s"
            ,sup,sec,(int)tot_width
            ,tot_width>550.0 ? "#FF0000":"#000000");
         Tcl_Evaluate(cmd);
         Tcl_Evaluate("update ; update");
      }
   }

   // averages
   for(sup=1;sup<=6;sup++){
      sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 1.2 %d %d %d #000000",sup,7,(int)sigma_n[0][sup]);
      Tcl_Evaluate(cmd);
      sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 1.2 %d %d %d #000000",7,sup,(int)sigma_n[sup][0]);
      Tcl_Evaluate(cmd);

      sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 3.0 %d %d %d #000000",sup,7,(int)sigma_w[0][sup]);
      Tcl_Evaluate(cmd);
      sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 3.0 %d %d %d #000000",7,sup,(int)sigma_w[sup][0]);
      Tcl_Evaluate(cmd);

      sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 4.8 %d %d %d #000000",sup,7,(int)mean[0][sup]);
      Tcl_Evaluate(cmd);
      sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 4.8 %d %d %d #000000",7,sup,(int)mean[sup][0]);
      Tcl_Evaluate(cmd);

		tot_width = TotalWidth(amp_ratio[0][sup], sigma_n[0][sup], sigma_w[0][sup]);
      sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 6.6 %d %d %d #000000",sup,7,(int)tot_width);
      Tcl_Evaluate(cmd);
      
		//s1=sigma_n[sup][0];
		//s2=sigma_w[sup][0];
		//R=amp_ratio[sup][0];
		//tot_width = pow((R*s1)+s2,2.0)/((R*R*s1)+s2);
 		tot_width = TotalWidth(amp_ratio[sup][0], sigma_n[sup][0], sigma_w[sup][0]);
      sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 6.6 %d %d %d #000000",7,sup,(int)tot_width);
      Tcl_Evaluate(cmd);
   }
   sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 1.2 %d %d %d #000000",7,7,(int)sigma_n[0][0]);
   Tcl_Evaluate(cmd);
   sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 3.0 %d %d %d #000000",7,7,(int)sigma_w[0][0]);
   Tcl_Evaluate(cmd);
   sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 4.8 %d %d %d #000000",7,7,(int)mean[0][0]);
   Tcl_Evaluate(cmd);
   
	tot_width = TotalWidth(amp_ratio[0][0], sigma_n[0][0], sigma_w[0][0]);
   sprintf(cmd,"DrawTableElement .qualsum.c.c 1.0 6.6 %d %d %d #000000",7,7,(int)tot_width);
   Tcl_Evaluate(cmd);
   
      
   
   working=0;

   return TCL_OK;
}

void FindResidualWidths(int HID
   ,float *sigma_narrow
   ,float *sigma_wide)
{
   float m,ar;
   
   FindResidualWidths(HID,sigma_narrow,sigma_wide,&m,&ar);
}
void FindResidualWidths(int HID
   ,float *sigma_narrow
   ,float *sigma_wide
   ,float *mean
   ,float *amplitude_ratio)
{
   float STEP[6],PMIN[6],PMAX[6],SIGPAR[6],CHISQ;
   float PAR[6]={500.0, 0.0, 0.0300, 50.0, 0.0, 0.100};
   float tmp;
   static int working=0;
   int i;
   char cmd[256];
   float total_width;
   
   while(working)USLEEP(10000);
   working=1;

   hfithn(HID,"g+g","Q",6,PAR,STEP,PMIN,PMAX,SIGPAR,&CHISQ);
   
   PAR[2]=fabs(PAR[2]);
   PAR[5]=fabs(PAR[5]);
   
   // Make sure narrow and wide didn't switch
   if(PAR[2]>PAR[5]){
      for(i=0;i<3;i++){
         tmp=PAR[i];
         PAR[i]=PAR[i+3];
         PAR[i+3]=tmp;
      }
   }

   *sigma_narrow    = 1.0E4*PAR[2];
   *sigma_wide      = 1.0E4*PAR[5];
   *amplitude_ratio = PAR[0]/PAR[3];
   *mean            = ((PAR[0]*PAR[1]*PAR[2]) + (PAR[3]*PAR[4]*PAR[5]))
      					/((PAR[0]*PAR[2]) + (PAR[3]*PAR[5]));
   *mean*=1.0E4;

	total_width = TotalWidth(*amplitude_ratio, *sigma_narrow, *sigma_wide);
		
   /* update values in GUI */
   Tcl_Evaluate("global quality_path");
   sprintf(cmd,"$quality_path.right.stats.tw configure -text \"Total width    : %3d microns\"",(int)total_width);
	Tcl_Evaluate(cmd);
   sprintf(cmd,"$quality_path.right.stats.ns configure -text \"Narrow sigma   : %3d microns\"",(int)*sigma_narrow);   
	Tcl_Evaluate(cmd);
   sprintf(cmd,"$quality_path.right.stats.ws configure -text \"Wide sigma     : %3d microns\"",(int)*sigma_wide);   
	Tcl_Evaluate(cmd);
   sprintf(cmd,"$quality_path.right.stats.mn configure -text \"Mean           : %3d microns\"",(int)*mean);   
	Tcl_Evaluate(cmd);
   sprintf(cmd,"$quality_path.right.stats.ar configure -text \"Amplitude Ratio: %2.2f\"",*amplitude_ratio);   
	Tcl_Evaluate(cmd);

   working=0;
}

void FindHitsPerTBT(float *hpt)
{
   int sec,sup,HID;
   float hist[7],wsum,sum;
   int i;
   
   hcdir("//LUN1"," ");
   
   hpt[0]=0.0;
   for(sec=1;sec<=6;sec++){
      hpt[sec]=0.0;
      // average over superlayers
      for(sup=1;sup<=6;sup++){
         HID=(sec*10000)+(sup*1000)+7;
         hrin(HID,1,0);
         hunpak(HID,hist,"HIST",0);
         hdelet(HID);
         wsum=sum=0.0;
         for(i=0;i<=6;i++){
            sum+=hist[i];
            wsum+=hist[i]*(float)i;
         }
         hpt[sec]+=wsum/sum;
      }
      // overall average
      hpt[0]+=hpt[sec]/6.0;
   }
   
   hcdir("//LUN1/tbt/proton"," ");
}

void FindAvgTrackingChisq(float *chisq)
{
   int sec,sup,HID;
   float hist[7],wsum,sum;
   int i;
   
   hcdir("//LUN1"," ");
   
   chisq[0]=0.0;
   for(sec=1;sec<=6;sec++){
      HID=2000+sec;
      hrin(HID,1,0);
      chisq[sec]=hstati(HID,1,"HIST",0);
      hdelet(HID);
      chisq[0]+=chisq[sec]/6.0;
   }
   
   hcdir("//LUN1/tbt/proton"," ");
}

float TotalWidth(float R,float sigma_n, float sigma_w)
{
	return ((R*sigma_n*sigma_n)+(sigma_w*sigma_w))/((R*sigma_n)+sigma_w);
}



