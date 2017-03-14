
#include <mysql.h>

#include "trk_mon_lite.h"

int UPDATE_QUALITY_DATABASE=0;


void FindHitsPerTBT(float *hpt);
void FindAvgTrackingChisq(float *chisq, int *ntracks);
float TotalWidth(float R,float sigma_n, float sigma_w);
void FindResidualWidths(int HID
   ,float *sigma_narrow
   ,float *sigma_wide
   ,float *mean
   ,float *amplitude_ratio);

// Get number of events in histo (I'm too lazy to put this in clas_cern right now)
extern "C" void hnoent_(int*,int*);
void hnoent(int HID, int *N){hnoent_(&HID,N);}

//---------------------
// UpdateQualityDB
//---------------------
int UpdateQualityDB(void)
{
	cout<<"Recording calibration quality in dc_settings database..."<<endl;

	// What we want here is to attempt to connect to the clasdb server.
	MYSQL *mysql_dcdefault = NULL;
	char *db_server = "clasdb.jlab.org";
	//db_server = "localhost"; // for testing locally
	if(!mysql_dcdefault){
		mysql_dcdefault = (MYSQL*)malloc(sizeof(MYSQL));
		mysql_init(mysql_dcdefault);
		MYSQL *tmp = mysql_real_connect(mysql_dcdefault, db_server, "clasuser", NULL, "dc_settings", 0, NULL, 0);
		if(!tmp){
			free(mysql_dcdefault);
			mysql_dcdefault = NULL;
		}
	}
	if(!mysql_dcdefault){
		cerr<<"Unable to connect to \""<<db_server<<"\"!"<<endl;
		return -1;
	}
	
	// This stuff is used in the TrackingStats table below. We want to
	// use ntracks here though to decide whether to make any entries
	// at all
	float hpt[7], chisq[7];
	int ntracks[7];
	FindHitsPerTBT(hpt);
	FindAvgTrackingChisq(chisq, ntracks);
	if(ntracks[0]<30000){
		cout<<"Too few tracks to make useful entry regarding calibration"<<endl;
		cout<<"quality in database (number of tracks="<<ntracks[0];
		cout<<"  needed="<<30000<<")"<<endl;
		return -1;
	}
	
	// ---- Fill ConstantsID table and get cid -----
	char query[1024];
	char fields[1024]="";
	char values[1024]="";
	char *ptr;
	char *user = getenv("USER");
	
	strcat(fields,"runnumber,");
	sprintf(values,"%d,", RUNNUMBER);
	if(ptr=getenv("CLAS_CALDB_HOST")){
		strcat(fields, "CLAS_CALDB_HOST,");
		strcat(values,"\"");strcat(values,ptr);strcat(values,"\",");
	}
	if(ptr=getenv("CLAS_CALDB_TIMESTAMP")){
		strcat(fields, "CLAS_CALDB_TIMESTAMP,");
		strcat(values,"\"");strcat(values,ptr);strcat(values,"\",");
	}
	if(ptr=getenv("CLAS_CALDB_RUNINDEX")){
		strcat(fields, "CLAS_CALDB_RUNINDEX,");
		strcat(values,"\"");strcat(values,ptr);strcat(values,"\",");
	}
	if(user){
		strcat(fields, "author,");
		strcat(values,"\"");strcat(values,user);strcat(values,"\",");
	}
	strcat(fields, "time");
	strcat(values, "NOW()");
	
	// Make insert
	int cid = -1;
	sprintf(query, "INSERT INTO ConstantsID (%s) VALUES(%s)", fields, values);
	if(mysql_real_query(mysql_dcdefault, query, strlen(query))) { //query failed
			cerr<<endl<<query<<endl;
			cerr<<mysql_error(mysql_dcdefault)<<endl;
	}else{
		cid = mysql_insert_id(mysql_dcdefault);
	}
	
	// ----- TrackingStats ----
	for(int sector=1;sector<=6;sector++){
		strcpy(fields, "cid,sector,HitsPerTBT,ChisqPerDOF,");
		sprintf(values,"%d,%d,%f,%f,", cid, sector, hpt[sector], chisq[sector]);
		if(user){
			strcat(fields, "author,");
			strcat(values,"\"");strcat(values,user);strcat(values,"\",");
		}
		
		strcat(fields,"ntracks,");
		sprintf(&values[strlen(values)], "%d,", ntracks[sector]);

		strcat(fields, "time");
		strcat(values, "NOW()");
		sprintf(query, "INSERT INTO TrackingStats (%s) VALUES(%s)", fields, values);
		if(mysql_real_query(mysql_dcdefault, query, strlen(query))) { //query failed
			cerr<<endl<<query<<endl;
			cerr<<mysql_error(mysql_dcdefault)<<endl;
		}
	}
	
	// ---- Residuals ----
	for(int sup=1;sup<=6;sup++){
		for(int sec=1;sec<=6;sec++){
         
         int RPID= 500+100+sup+(sec*10);

         // calculate sigmas
			float sigma_n, sigma_w, mean, amp_ratio;
         FindResidualWidths(RPID, &sigma_n, &sigma_w, &mean, &amp_ratio);
			float sigma_t = TotalWidth( amp_ratio, sigma_n, sigma_w);
			int nentries=0;
			hnoent(RPID,&nentries);
			sprintf(query, "INSERT INTO Residuals VALUES(%d,%d,%d,%f,%f,%f,%f,%d,\"%s\",NOW())"
				,cid,sec,sup,sigma_n,sigma_w,sigma_t,mean,nentries,user ? user:"NULL");
			if(mysql_real_query(mysql_dcdefault, query, strlen(query))) { //query failed
				cerr<<endl<<query<<endl;
				cerr<<mysql_error(mysql_dcdefault)<<endl;
			}
		}
	}
	
	// ---- LocalAngleCuts ----
	sprintf(query,"INSERT INTO LocalAngleCuts VALUES(%d,%f,%f,%f,%f,%f,%f,\"%s\",NOW())"
		,cid
		,LOCANGLE_CUT_HIGH[1]
		,LOCANGLE_CUT_LOW[1]
		,LOCANGLE_CUT_HIGH[2]
		,LOCANGLE_CUT_LOW[2]
		,LOCANGLE_CUT_HIGH[3]
		,LOCANGLE_CUT_LOW[3]
		,user ? user:"NULL"
		);
	if(mysql_real_query(mysql_dcdefault, query, strlen(query))) { //query failed
		cerr<<endl<<query<<endl;
		cerr<<mysql_error(mysql_dcdefault)<<endl;
	}
	
	// Close the DB connection
	mysql_close(mysql_dcdefault);
	free(mysql_dcdefault);
	mysql_dcdefault=NULL;
	
	cout<<"Finished updating calibration quality in database"<<endl;

	return 0;
}

//----------------------
// FindHitsPerTBT
//----------------------
void FindHitsPerTBT(float *hpt)
{
   int sec,sup,HID;
   float hist[7],wsum,sum;
   int i;
   
   hcdir("//PAWC"," ");
   
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
   
}

//----------------------
// FindAvgTrackingChisq
//----------------------
void FindAvgTrackingChisq(float *chisq, int *ntracks)
{
   int sec,sup,HID;
   float hist[7],wsum,sum;
   int i;
   
   hcdir("//PAWC"," ");
   
   chisq[0]=0.0;
	ntracks[0] = 0;
   for(sec=1;sec<=6;sec++){
      HID=2000+sec;
      hrin(HID,1,0);
      chisq[sec]=hstati(HID,1,"HIST",0);
		hnoent(HID,&ntracks[sec]);
      hdelet(HID);
		ntracks[0] += ntracks[sec];
      chisq[0]+=chisq[sec]/6.0;
   }
   
}

//----------------------
// FindResidualWidths
//----------------------
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
		
}

//----------------------
// TotalWidth
//----------------------
float TotalWidth(float R,float sigma_n, float sigma_w)
{
	return ((R*sigma_n*sigma_n)+(sigma_w*sigma_w))/((R*sigma_n)+sigma_w);
}

