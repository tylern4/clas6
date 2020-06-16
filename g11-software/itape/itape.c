/*
 * itape.c 
 * Purpose: convert bos banks to itape format
 */
  
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <time.h>
#include <clas_cern.h>
#include <particleType.h>
#include <kinematics.h>
#include <pdgutil.h>
#include <pid.h>
#include <scalers.h>
#include <utility.h>
#include <printBOS.h>
#include <ec.h>
#include <PartUtil.h>
#include <itape.h>
/* void *data_addGroup(void *buffer,int bufsize,int type,int length); */



int useDispatcher = 0;
int RawMode = 0;
int fileLun = 0;
itape_header_t *buffer = NULL;
int verbose = 0;

#define BASE 100
#define BUFSIZE 300000

int SetVerbose(int v)
{
  verbose = v;
}

int GroupSize(int base)
{
  int ret;
  base /= BASE;
  switch (base) {
  case 800:
    ret = sizeof(part_t)/sizeof(int);
    break;
  case 100:
    ret = sizeof(dc0_t)/sizeof(int);
    break;
  default:
    ret = 1;
  }
  return(ret);
}



int GroupNum(char *name,int sec)
{
  int ibase;
  if (strcmp(name,"BMPR") == 0) {
    ibase = 10;
  }
  else if  (strcmp(name,"BREP") == 0) {
    ibase = 20;
  }
  else if  (strcmp(name,"CALL") == 0) {
    ibase = 30;
  }
  else if  (strcmp(name,"CC0 ") == 0) {
    ibase = 40;
  }
  else if  (strcmp(name,"CC01 ") == 0) {
    ibase = 50;
  }
  else if  (strcmp(name,"CC1 ") == 0) {
    ibase = 60;
  }
  else if  (strcmp(name,"CCDI") == 0) {
    ibase = 70;
  }
  else if  (strcmp(name,"CCH ") == 0) {
    ibase = 80;
  }
  else if  (strcmp(name,"CCH1") == 0) {
    ibase = 90;
  }
  else if  (strcmp(name,"CCH2") == 0) {
    ibase = 100;
  }
  else if  (strcmp(name,"CCMT") == 0) {
    ibase = 110;
  }
  else if  (strcmp(name,"CCPB") == 0) {
    ibase = 120;
  }
  else if  (strcmp(name,"CCPE") == 0) {
    ibase = 130;
  }
  else if  (strcmp(name,"CCRC") == 0) {
    ibase = 140;
  }
  else if  (strcmp(name,"CCS ") == 0) {
    ibase = 150;
  }
  else if  (strcmp(name,"CHI2") == 0) {
    ibase = 160;
  }
  else if  (strcmp(name,"CPED") == 0) {
    ibase = 170;
  }
  else if  (strcmp(name,"ECPC") == 0) {
    ibase = 180;
  }
  else if  (strcmp(name,"ECPE") == 0) {
    ibase = 190;
  }
  else if  (strcmp(name,"CL01") == 0) {
    ibase = 200;
  }
  else if  (strcmp(name,"DC0 ") == 0) {
    ibase = 210;
  }
  else if  (strcmp(name,"DC1 ") == 0) {
    ibase = 220;
  }
  else if  (strcmp(name,"DCDW") == 0) {
    ibase = 230;
  }
  else if  (strcmp(name,"DCGM") == 0) {
    ibase = 240;
  }
  else if  (strcmp(name,"DCGW") == 0) {
    ibase = 250;
  }
  else if  (strcmp(name,"DCH ") == 0) {
    ibase = 260;
  }
  else if  (strcmp(name,"DCPB") == 0) {
    ibase = 270;
  }
  else if  (strcmp(name,"DCV1") == 0) {
    ibase = 280;
  }
  else if  (strcmp(name,"DCV2") == 0) {
    ibase = 290;
  }
  else if  (strcmp(name,"DCV3") == 0) {
    ibase = 300;
  }
  else if  (strcmp(name,"DDLY") == 0) {
    ibase = 310;
  }
  else if  (strcmp(name,"DGEO") == 0) {
    ibase = 320;
  }
  else if  (strcmp(name,"DHCL") == 0) {
    ibase = 330;
  }
  else if  (strcmp(name,"DOCA") == 0) {
    ibase = 340;
  }
  else if  (strcmp(name,"DPSP") == 0) {
    ibase = 350;
  }
  else if  (strcmp(name,"DSPC") == 0) {
    ibase = 360;
  }
  else if  (strcmp(name,"DSPS") == 0) {
    ibase = 370;
  }
  else if  (strcmp(name,"DSTC") == 0) {
    ibase = 380;
  }
  else if  (strcmp(name,"DTRK") == 0) {
    ibase = 390;
  }
  else if  (strcmp(name,"EC  ") == 0) {
    ibase = 400;
  }
  else if  (strcmp(name,"EC01") == 0) {
    ibase = 410;
  }
  else if  (strcmp(name,"EC1 ") == 0) {
    ibase = 420;
  }
  else if  (strcmp(name,"EC1P") == 0) {
    ibase = 430;
  }
  else if  (strcmp(name,"EC1R") == 0) {
    ibase = 440;
  }
  else if  (strcmp(name,"ECCA") == 0) {
    ibase = 450;
  }
  else if  (strcmp(name,"ECCL") == 0) {
    ibase = 460;
  }
  else if  (strcmp(name,"ECCT") == 0) {
    ibase = 470;
  }
  else if  (strcmp(name,"ECDI") == 0) {
    ibase = 480;
  }
  else if  (strcmp(name,"ECG ") == 0) {
    ibase = 490;
  }
  else if  (strcmp(name,"ECH ") == 0) {
    ibase = 500;
  }
  else if  (strcmp(name,"ECHB") == 0) {
    ibase = 510;
  }
  else if  (strcmp(name,"ECMT") == 0) {
    ibase = 520;
  }
  else if  (strcmp(name,"ECP ") == 0) {
    ibase = 530;
  }
  else if  (strcmp(name,"ECP1") == 0) {
    ibase = 540;
  }
  else if  (strcmp(name,"ECPB") == 0) {
    ibase = 550;
  }
  else if  (strcmp(name,"ECPI") == 0) {
    ibase = 560;
  }
  else if  (strcmp(name,"ECPO") == 0) {
    ibase = 570;
  }
  else if  (strcmp(name,"ECRB") == 0) {
    ibase = 580;
  }
  else if  (strcmp(name,"ECS ") == 0) {
    ibase = 590;
  }
  else if  (strcmp(name,"EID0") == 0) {
    ibase = 600;
  }
  else if  (strcmp(name,"EPIC") == 0) {
    ibase = 610;
  }
  else if  (strcmp(name,"EVNT") == 0) {
    ibase = 620;
  }
  else if  (strcmp(name,"FBPM") == 0) {
    ibase = 630;
  }
  else if  (strcmp(name,"GPAR") == 0) {
    ibase = 640;
  }
  else if  (strcmp(name,"G1SL") == 0) {
    ibase = 650;
  }
  else if  (strcmp(name,"G2SL") == 0) {
    ibase = 660;
  }
  else if  (strcmp(name,"G3SL") == 0) {
    ibase = 670;
  }
  else if  (strcmp(name,"G4SL") == 0) {
    ibase = 680;
  }
  else if  (strcmp(name,"HBER") == 0) {
    ibase = 690;
  }
  else if  (strcmp(name,"HBID") == 0) {
    ibase = 700;
  }
  else if  (strcmp(name,"HBLA") == 0) {
    ibase = 710;
  }
  else if  (strcmp(name,"HBTB") == 0) {
    ibase = 720;
  }
  else if  (strcmp(name,"HBTR") == 0) {
    ibase = 730;
  }
  else if  (strcmp(name,"HCAL") == 0) {
    ibase = 740;
  }
  else if  (strcmp(name,"HDPL") == 0) {
    ibase = 750;
  }
  else if  (strcmp(name,"HEAD") == 0) {
    ibase = 760;
  }
  else if  (strcmp(name,"HEVT") == 0) {
    ibase = 770;
  }
  else if  (strcmp(name,"HLS ") == 0) {
    ibase = 780;
  }
  else if  (strcmp(name,"LCDI") == 0) {
    ibase = 790;
  }
  else if  (strcmp(name,"LCPB") == 0) {
    ibase = 800;
  }
  else if  (strcmp(name,"KFIT") == 0) {
    ibase = 810;
  }
  else if  (strcmp(name,"MCEV") == 0) {
    ibase = 820;
  }
  else if  (strcmp(name,"MCTK") == 0) {
    ibase = 830;
  }
  else if  (strcmp(name,"MCVX") == 0) {
    ibase = 840;
  }
  else if  (strcmp(name,"PART") == 0) {
    ibase = 850;
  }
  else if  (strcmp(name,"PCO ") == 0) {
    ibase = 860;
  }
  else if  (strcmp(name,"PSO ") == 0) {
    ibase = 870;
  }
  else if  (strcmp(name,"PTDB") == 0) {
    ibase = 880;
  }
  else if  (strcmp(name,"RF  ") == 0) {
    ibase = 890;
  }
  else if  (strcmp(name,"RGLK") == 0) {
    ibase = 900;
  }
  else if  (strcmp(name,"RUNC") == 0) {
    ibase = 910;
  }
  else if  (strcmp(name,"RTSL") == 0) {
    ibase = 920;
  }
  else if  (strcmp(name,"SC  ") == 0) {
    ibase = 930;
  }
  else if  (strcmp(name,"SC1 ") == 0) {
    ibase = 940;
  }
  else if  (strcmp(name,"SCC ") == 0) {
    ibase = 950;
  }
  else if  (strcmp(name,"SCG ") == 0) {
    ibase = 960;
  }
  else if  (strcmp(name,"SCH ") == 0) {
    ibase = 970;
  }
  else if  (strcmp(name,"SCMD") == 0) {
    ibase = 980;
  }
  else if  (strcmp(name,"SCP ") == 0) {
    ibase = 990;
  }
  else if  (strcmp(name,"SCPB") == 0) {
    ibase = 1000;
  }
  else if  (strcmp(name,"SCPE") == 0) {
    ibase = 1010;
  }
  else if  (strcmp(name,"SCR ") == 0) {
    ibase = 1020;
  }
  else if  (strcmp(name,"SCS ") == 0) {
    ibase = 1030;
  }
  else if  (strcmp(name,"SCDI") == 0) {
    ibase = 1040;
  }
  else if  (strcmp(name,"SCMT") == 0) {
    ibase = 1050;
  }
  else if  (strcmp(name,"SCMW") == 0) {
    ibase = 1060;
  }
  else if  (strcmp(name,"SCRC") == 0) {
    ibase = 1070;
  }
  else if  (strcmp(name,"SPAR") == 0) {
    ibase = 1080;
  }
  else if  (strcmp(name,"ST  ") == 0) {
    ibase = 1090;
  }
  else if  (strcmp(name,"ST1 ") == 0) {
    ibase = 1100;
  }
  else if  (strcmp(name,"STH ") == 0) {
    ibase = 1110;
  }
  else if  (strcmp(name,"STPE") == 0) {
    ibase = 1120;
  }
  else if  (strcmp(name,"STPB") == 0) {
    ibase = 1130;
  }
  else if  (strcmp(name,"STR ") == 0) {
    ibase = 1140;
  }
  else if  (strcmp(name,"STS ") == 0) {
    ibase = 1150;
  }
  else if  (strcmp(name,"S1ST") == 0) {
    ibase = 1160;
  }
  else if  (strcmp(name,"SYNC") == 0) {
    ibase = 1170;
  }
  else if  (strcmp(name,"TACO") == 0) {
    ibase = 1180;
  }
  else if  (strcmp(name,"TAGE") == 0) {
    ibase = 1190;
  }
  else if  (strcmp(name,"TAGI") == 0) {
    ibase = 1200;
  }
  else if  (strcmp(name,"TAGR") == 0) {
    ibase = 1210;
  }
  else if  (strcmp(name,"TAGT") == 0) {
    ibase = 1220;
  }
  else if  (strcmp(name,"TBER") == 0) {
    ibase = 1230;
  }
  else if  (strcmp(name,"TBID") == 0) {
    ibase = 1240;
  }
  else if  (strcmp(name,"TBLA") == 0) {
    ibase = 1250;
  }
  else if  (strcmp(name,"TBTR") == 0) {
    ibase = 1260;
  }
  else if  (strcmp(name,"TCSB") == 0) {
    ibase = 1270;
  }
  else if  (strcmp(name,"TDPL") == 0) {
    ibase = 1280;
  }
  else if  (strcmp(name,"TGBI") == 0) {
    ibase = 1290;
  }
  else if  (strcmp(name,"TGPB") == 0) {
    ibase = 1300;
  }
  else if  (strcmp(name,"TGS ") == 0) {
    ibase = 1310;
  }
  else if  (strcmp(name,"TRGS") == 0) {
    ibase = 1320;
  }
  else if  (strcmp(name,"TRKS") == 0) {
    ibase = 1330;
  }
  else if  (strcmp(name,"TRGT") == 0) {
    ibase = 1340;
  }
  else if  (strcmp(name,"UNUS") == 0) {
    ibase = 1350;
  }
  else if  (strcmp(name,"VERT") == 0) {
    ibase = 1360;
  }
  else if  (strcmp(name,"MVRT") == 0) {
    ibase = 1370;
  }
  else if  (strcmp(name,"MTRK") == 0) {
    ibase = 1380;
  }
  else if  (strcmp(name,"SGMP") == 0) {
    ibase = 1390;
  }
  else if  (strcmp(name,"CLST") == 0) {
    ibase = 1400;
  } 
  else if  (strcmp(name,"TRL1") == 0) {
    ibase = 1410;
  }
  else if (strcmp(name,"LS2 ") == 0) {
    ibase = 1420;
      
  }
  else
    ibase = 0;

  return(ibase * BASE + sec);
}

void PrintHeader(FILE *fp,itape_header_t *buffer)
{
  fprintf(fp,"Dump Event Header:\nRun: %d\tClass: %d\tEvent Number: %d\n",
	  buffer->runNo,buffer->spillNo,buffer->eventNo);
  fprintf(fp,"Time: %s\n",ctime((time_t *) &buffer->time));
  fprintf(fp,"Type: %d\tRoc: %d\n",buffer->trigger,buffer->latch);
}


int fillHeader(itape_header_t *buffer)
{
  int ret = 0;
 clasHEAD_t *HEAD = (clasHEAD_t *) getBank(&bcs_,"HEAD");
  if (HEAD) {
    buffer->runNo  = HEAD->head[0].nrun;
    buffer->spillNo = HEAD->head[0].evtclass;
    buffer->eventNo = HEAD->head[0].nevent;
    buffer->time = HEAD->head[0].time;
    buffer->trigger = HEAD->head[0].type;
    buffer->latch = HEAD->head[0].roc;
    ret = buffer->runNo;
    if (verbose)
      PrintHeader(stderr,buffer);
  }
  else 
    ret = 0;
  return(ret);
    
}


int addGroup(itape_header_t *buffer,int bufsize,char *bank)
{
  static int maxWrite = 10;
  int type;
  int size;
  int ret = 0;
  void *ptr;
  bankHeader_t  *BANK = (bankHeader_t *)getBank(&bcs_,bank); 
  if (verbose)
    fprintf(stderr,"addGroup: Consider bank %s\n",bank);
  if (BANK) {
    type = GroupNum(bank,BANK->nr);
    if (type) {
      size = BANK->nwords * sizeof(int) + sizeof(bankHeader_t);
      if ( (ptr = data_addGroup((void *)buffer,bufsize,type,size)) ) {
	memcpy(ptr,BANK,size);
	if (verbose) {
	  fprintf(stderr,"addGroup: adding %s bank\n",bank);
	}
      }
 	
    }
    else {
      if (maxWrite-- > 0)
	fprintf(stderr,"unable to identify type of bank %s (%d)\n",bank,maxWrite);
    }
    ret = 1;
  }
  return(ret);

}
int addGroups(itape_header_t *buffer,int bufsize,char *bank)
{
  static int maxWrite = 20;
  int type;
  int size;
  int ret = 0;
  void *ptr;
  bankHeader_t  *BANK = (bankHeader_t *)getBank(&bcs_,bank); 
  if (BANK) {

    do {
      if ( (type = GroupNum(bank,BANK->nr))) {
	size = BANK->nwords * sizeof(int) + sizeof(bankHeader_t);
	ptr = (void *) data_addGroup((void *)buffer,bufsize,type,size);
	memcpy(ptr,BANK,size);
      }
      else {
	if (maxWrite-- > 0) {
	fprintf(stderr,"Unable to identify bank: %s\n",bank);
	}
      }
    } while ((BANK = (bankHeader_t *) getNextBank(&bcs_,BANK)));
    ret = 1;
  }
    
  return(ret);
  

}

char *GroupName(int group,int *sec)
{
  static char ret[5];
  int ibase = group/BASE;
  *sec = group % BASE;
  switch (ibase) {
  case 10:
    strcpy(ret,"BMPR");
    break;
  case 20:
    strcpy(ret,"BREP");
    break;
  case 30:
    strcpy(ret,"CALL");
    break;
  case 40:
    strcpy(ret,"CC0 ");
    break;
  case 50:
    strcpy(ret,"CC01");
    break;
  case 60:
    strcpy(ret,"CC1 ");
    break;
  case 70:
    strcpy(ret,"CCDI");
    break;
  case 80:
    strcpy(ret,"CCH ");
    break;
  case 90:
    strcpy(ret,"CCH1");
    break;
  case 100:
    strcpy(ret,"CCH2");
    break;
  case 110:
    strcpy(ret,"CCMT");
    break;
  case 120:
    strcpy(ret,"CCPB");
    break;
  case 130:
    strcpy(ret,"CCPE");
    break;
  case 140:
    strcpy(ret,"CCRC");
    break;
  case 150:
    strcpy(ret,"CCS ");
    break;
  case 160:
    strcpy(ret,"CHI2");
    break;
  case 170:
    strcpy(ret,"CPED");
    break;
  case 180:
    strcpy(ret,"ECPC");
    break;
  case 190:
    strcpy(ret,"ECPE");
    break;
  case 200:
    strcpy(ret,"CL01");
    break;
  case 210:
    strcpy(ret,"DC0 ");
    break;
  case 220:
    strcpy(ret,"DC1 ");
    break;
  case 230:
    strcpy(ret,"DCDW");
    break;
  case 240:
    strcpy(ret,"DCGM");
    break;
  case 250:
    strcpy(ret,"DCGW");
    break;
  case 260:
    strcpy(ret,"DCH ");
    break;
  case 270:
    strcpy(ret,"DCPB");
    break;
  case 280:
    strcpy(ret,"DCV1");
    break;
  case 290:
    strcpy(ret,"DCV2");
    break;
  case 300:
    strcpy(ret,"DCV3");
    break;
  case 310:
    strcpy(ret,"DDLY");
    break;
  case 320:
    strcpy(ret,"DGEO");
    break;
  case 330:
    strcpy(ret,"DHCL");
    break;
  case 340:
    strcpy(ret,"DOCA");
    break;
  case 350:
    strcpy(ret,"DPSP");
    break;
  case 360:
    strcpy(ret,"DSPC");
    break;
  case 370:
    strcpy(ret,"DSPS");
    break;
  case 380:
    strcpy(ret,"DSTC");
    break;
  case 390:
    strcpy(ret,"DTRK");
    break;
  case 400:
    strcpy(ret,"EC  ");
    break;
  case 410:
    strcpy(ret,"EC01");
    break;
  case 420:
    strcpy(ret,"EC1 ");
    break;
  case 430:
    strcpy(ret,"EC1P");
    break;
  case 440:
    strcpy(ret,"EC1R");
    break;
  case 450:
    strcpy(ret,"ECCA");
    break;
  case 460:
    strcpy(ret,"ECCL");
    break;
  case 470:
    strcpy(ret,"ECCT");
    break;
  case 480:
    strcpy(ret,"ECDI");
    break;
  case 490:
    strcpy(ret,"ECG ");
    break;
  case 500:
    strcpy(ret,"ECH ");
    break;
  case 510:
    strcpy(ret,"ECHB");
    break;
  case 520:
    strcpy(ret,"ECMT");
    break;
  case 530:
    strcpy(ret,"ECP ");
    break;
  case 540:
    strcpy(ret,"ECP1");
    break;
  case 550:
    strcpy(ret,"ECPB");
    break;
  case 560:
    strcpy(ret,"ECPI");
    break;
  case 570:
    strcpy(ret,"ECPO");
    break;
  case 580:
    strcpy(ret,"ECRB");
    break;
  case 590:
    strcpy(ret,"ECS ");
    break;
  case 600:
    strcpy(ret,"EID0");
    break;
  case 610:
    strcpy(ret,"EPIC");
    break;
  case 620:
    strcpy(ret,"EVNT");
    break;
  case 630:
    strcpy(ret,"FBPM");
    break;
  case 640:
    strcpy(ret,"GPAR");
    break;
  case 650:
    strcpy(ret,"G1SL");
    break;
  case 660:
    strcpy(ret,"G2SL");
    break;
  case 670:
    strcpy(ret,"G3SL");
    break;
  case 680:
    strcpy(ret,"G4SL");
    break;
  case 690:
    strcpy(ret,"HBER");
    break;
  case 700:
    strcpy(ret,"HBID");
    break;
  case 710:
    strcpy(ret,"HBLA");
    break;
  case 720:
    strcpy(ret,"HBTB");
    break;
  case 730:
    strcpy(ret,"HBTR");
    break;
  case 740:
    strcpy(ret,"HCAL");
    break;
  case 750:
    strcpy(ret,"HDPL");
    break;
  case 760:
    strcpy(ret,"HEAD");
    break;
  case 770:
    strcpy(ret,"HEVT");
    break;
  case 780:
    strcpy(ret,"HLS ");
    break;
  case 790:
    strcpy(ret,"LCDI");
    break;
  case 800:
    strcpy(ret,"LCPB");
    break;
  case 810:
    strcpy(ret,"KFIT");
    break;
  case 820:
    strcpy(ret,"MCEV");
    break;
  case 830:
    strcpy(ret,"MCTK");
    break;
  case 840:
    strcpy(ret,"MCVX");
    break;
  case 850:
    strcpy(ret,"PART");
    break;
  case 860:
    strcpy(ret,"PCO ");
    break;
  case 870:
    strcpy(ret,"PSO ");
    break;
  case 880:
    strcpy(ret,"PTDB");
    break;
  case 890:
    strcpy(ret,"RF  ");
    break;
  case 900:
    strcpy(ret,"RGLK");
    break;
  case 910:
    strcpy(ret,"RUNC");
    break;
  case 920:
    strcpy(ret,"RTSL");
    break;
  case 930:
    strcpy(ret,"SC  ");
    break;
  case 940:
    strcpy(ret,"SC1 ");
    break;
  case 950:
    strcpy(ret,"SCC ");
    break;
  case 960:
    strcpy(ret,"SCG ");
    break;
  case 970:
    strcpy(ret,"SCH ");
    break;
  case 980:
    strcpy(ret,"SCMD");
    break;
  case 990:
    strcpy(ret,"SCP ");
    break;
  case 1000:
    strcpy(ret,"SCPB");
    break;
  case 1010:
    strcpy(ret,"SCPE");
    break;
  case 1020:
    strcpy(ret,"SCR ");
    break;
  case 1030:
    strcpy(ret,"SCS ");
    break;
  case 1040:
    strcpy(ret,"SCDI");
    break;
  case 1050:
    strcpy(ret,"SCMT");
    break;
  case 1060:
    strcpy(ret,"SCMW");
    break;
  case 1070:
    strcpy(ret,"SCRC");
    break;
  case 1080:
    strcpy(ret,"SPAR");
    break;
  case 1090:
    strcpy(ret,"ST  ");
    break;
  case 1100:
    strcpy(ret,"ST1 ");
    break;
  case 1110:
    strcpy(ret,"STH ");
    break;
  case 1120:
    strcpy(ret,"STPE");
    break;
  case 1130:
    strcpy(ret,"STPB");
    break;
  case 1140:
    strcpy(ret,"STR ");
    break;
  case 1150:
    strcpy(ret,"STS ");
    break;
  case 1160:
    strcpy(ret,"S1ST");
    break;
  case 1170:
    strcpy(ret,"SYNC");
    break;
  case 1180:
    strcpy(ret,"TACO");
    break;
  case 1190:
    strcpy(ret,"TAGE");
    break;
  case 1200:
    strcpy(ret,"TAGI");
    break;
  case 1210:
    strcpy(ret,"TAGR");
    break;
  case 1220:
    strcpy(ret,"TAGT");
    break;
  case 1230:
    strcpy(ret,"TBER");
    break;
  case 1240:
    strcpy(ret,"TBID");
    break;
  case 1250:
    strcpy(ret,"TBLA");
    break;
  case 1260:
    strcpy(ret,"TBTR");
    break;
  case 1270:
    strcpy(ret,"TCSB");
    break;
  case 1280:
    strcpy(ret,"TDPL");
    break;
  case 1290:
    strcpy(ret,"TGBI");
    break;
  case 1300:
    strcpy(ret,"TGPB");
    break;
  case 1310:
    strcpy(ret,"TGS ");
    break;
  case 1320:
    strcpy(ret,"TRGS");
    break;
  case 1330:
    strcpy(ret,"TRKS");
    break;
  case 1340:
    strcpy(ret,"TRGT");
    break;
  case 1350:
    strcpy(ret,"UNUS");
    break;
  case 1360:
    strcpy(ret,"VERT");
    break;
  case 1370:
    strcpy(ret,"MVRT");
    break;
  case 1380:
    strcpy(ret,"MTRK");
    break;
  case 1390:
    strcpy(ret,"SGMP");
    break;
  case 1400:
    strcpy(ret,"CLST");
    break;
  case 1410:
    strcpy(ret,"TRL1");
    break;

  case 1420:
    strcpy(ret,"LS2 ");
    break;

  default:
    strcpy(ret,"UNKN");
    break;
  }
  return(ret);
}
    
int itape2bos(itape_header_t *buffer,BOSbank *bcs,int verbose)
{
  clasHEAD_t *hptr;
  char Elist[1024];

  int ngroups,*groups,*size;
  char name[5];
  int sec;
  int i;
  char *ptr,*ptrold;
  group_header_t *gheader;
  bankHeader_t bankH;
  data_listGroups(buffer,&ngroups,&groups,&size);
  Elist[0] = '\0';

  for(i = 0; i < ngroups; ++i) {
    strcpy(name,GroupName(groups[i],&sec));
    if (verbose) 
      fprintf(stderr,"itape2bos: %d\t%d\t%d\t%s\t%d\t",i,groups[i],size[i],name,sec);
    gheader = (group_header_t *)data_getGroupHeader(buffer,groups[i]);
    ptrold = (char *)data_getGroup(buffer,groups[i]);
    memcpy(&bankH,ptrold,sizeof(bankHeader_t));
    if (verbose)
      fprintf(stderr,"itape2bos: %d\t%d\n",gheader->length,gheader->type);
    ptr = (char *)makeBank(bcs,name,sec,bankH.ncol,bankH.nrow);
    ptr += sizeof(bankHeader_t);
    if (verbose)
      fprintf(stderr,"itape2bos: %s\t%d\t%d\t%d\t%d\n",bankH.name,bankH.ncol, bankH.nrow,bankH.nr,bankH.nwords);
    ptrold += sizeof(bankHeader_t);
    memcpy(ptr,ptrold,gheader->length - sizeof(bankHeader_t));
    strcat(Elist,name);
  }
  free(groups);
  free(size);

  bankList(bcs,"E=",Elist);
  if (verbose)
    fprintf(stderr,"itape2bos: E= %s\n",Elist);
   return(1);
}

int countBanks(int *jw)
{
#define ICK 30
#define LUP 6
#define INM 14 
#define NHW 6
#define IGP 15 
  int ret = 0;
   int ind;
  int i = 0;
  int ndel = 0;
  ind = jw[INM-1]+NHW-1;

  while (ind <= jw[IGP-1]) {
    if (jw[ind - 1] <= 0) {
      /* deleted bank */
      ndel -= jw[ind - 1];
      ind -= jw[ind - 1];
    }
    else {
      /* not deleted */
	   ret++;
      ind += jw[ind -1] + NHW;
    }
  }

  return(ret);
}

int addToList(char **ptr,char *bank,int n)
{
  int ret = 1;

  while(n--) {
    if (strcmp(*ptr,bank) == 0) {
      ret = 0;
      break;
    }
    ptr++;
  }
   return(ret);
}
void nGenBanks(itape_header_t *buffer,int bufsize,int *jw,char *list)
{

#define ICK 30
#define LUP 6
#define INM 14 
#define NHW 6
#define IGP 15 

  char bank[5];

  int ind;
  int i = 0;
  int j;
  int k;
  int ndel = 0;
  int *buf,*fmtbuf;

  bankHeader_t *hdr;
  int *xptr;

  int nbanks;
  char *banks;
  char *ptr[1000];
  nbanks = bosLdump(jw,list,&buf,&fmtbuf);
  banks = (char *)malloc(5 * nbanks); 
  if (verbose) {
    fprintf(stderr,"GenBanks: We have %d banks\n",nbanks);
  }    

  
 k = 0;

  for (i = 0; i < nbanks; ++i) {
    xptr =  &jw[buf[i]] - sizeof(bankHeader_t)/sizeof(int);
    hdr = (bankHeader_t *)xptr;
    if (verbose) {
      fprintf(stderr,"Nrow: %d Ncol: %d Nwords: %d\n",hdr->ncol,hdr->nrow,hdr->nwords);
      fprintf(stderr,"Bank: %s\n",hdr->name);
    }
    ptr[i] = banks + i * 5;
    cprint(hdr->name,bank);
    if (addToList(ptr,bank,k)) {
      if (verbose)
	fprintf(stderr,"GenBanks: adding %s\n",bank);
      strcpy(ptr[k++],bank);
    }
      
  }
  


  for (j = 0; j < k; ++j) {
    addGroups(buffer,bufsize,ptr[j]);
  }
}


void GenBanks(itape_header_t *buffer,int bufsize,int *jw)
{


  char bank[5];

  int ind;
  int i = 0;
  int j;
  int ndel = 0;

  int nbanks = countBanks(jw);
  char *banks = (char *)malloc(5 * nbanks);
  char *ptr[1000];

   if (verbose) { 
    fprintf(stderr,"GenBanks: jw\n");
    for (i =  0; i < 10; ++i)
      fprintf(stderr,"%d\t%d\t%x\n",i,*(jw+i),*(jw+i));
  }

  for (i = 0; i < nbanks; ++i)
    ptr[i] = banks + i * 5;
  
  if (verbose)
    fprintf(stderr,"GenBanks: We have %d banks\n",nbanks);

  ind = jw[INM-1]+NHW-1;

  i = 0;


  while (ind <= jw[IGP-1]) {
    if (jw[ind - 1] <= 0) {
      /* deleted bank */
      ndel -= jw[ind - 1];
      ind -= jw[ind - 1];
    }
    else {
      /* not deleted */
      cprint( (char *) &jw[ind-4],bank);
      if (addToList(ptr,bank,i)) {
	if (verbose)
	  fprintf(stderr,"GenBanks: adding %s\n",bank);
	strcpy(ptr[i++],bank);
      }

      ind += jw[ind -1] + NHW;
    }
  }
  for (j = 1; j < i; ++j) {
     addGroups(buffer,bufsize,ptr[j]);
  }
}

void cprint(char *c,char *b)
{
  memcpy(b,c,4);
  b[4] = '\0';
}


int initProcess(char *name,int dispatcherPipelinePrefill)
{
  return( isDispatcher(name) ? initDispatcher(name,dispatcherPipelinePrefill) :  initFile(name));
}

int initDisplay(char *name,int dispatcherPipelinePrefill)
{
  return( isDispatcher(name) ? initDispatcherDisplay(name,dispatcherPipelinePrefill) :  initFile(name));
}
int initDispatcher(char *server,int dispatcherPipelinePrefill)
{
  RawMode = 1;
  useDispatcher = 1;
  return(dispatcherConnect(server,dispatcherPipelinePrefill));

}

int initDispatcherDisplay(char *server,int dispatcherPipelinePrefill)
{
  int ret; 
  useDispatcher = 1;
  RawMode = 0;
  fprintf(stderr,"connect to host %s\n",server);
  if ( (ret = disIO_connect(server,0)) == 0) {
    disIO_command("Display:0xFFFF:0xFFFF");
    disIO_command("REQUEST_DISPLAY");
  }
  return(ret);
}

int initFile(char *filename)
{
  int ret;
  char mess[1024];
  if (fileLun) {
    sprintf(mess,"CLOSE BOSINPUT UNIT=7");
    fparm_c(mess);
    fileLun = 0;
  }
  sprintf(mess,"OPEN BOSINPUT UNIT=7 FILE=\"%s\" READ", filename);
  if (!fparm_c(mess)) {
    fprintf(stderr,"Unable to open file \'%s\': %s\n\n",filename,strerror(errno));
    ret = 0;
  }
  else {
    ret = fileLun = 7;
  }
  return(ret);

}

int send2Dispatcher(BOSbank *bcs,char *list)
{
  int ret;
  nGenBanks(buffer,BUFSIZE,bcs->iw,list);
  ret = disIO_writeCOOKED(buffer,buffer->length + 4);
  if (verbose) {
    fprintf(stderr,"Send data to Dispatcher %d- Run: %d\tEvent: %d\n",ret,buffer->runNo,buffer->eventNo);
  }

}

int getData(BOSbank *bcs,char *list)
{

  if (RawMode)
    return(getDataRAW(bcs,list));
  else
    return(getDataCOOKED(bcs,list));
}
	   


int getDataCOOKED(BOSbank *bcs,char *list)
{ 
  int nodata = 0;
  int retval;
  int wait = 0;
  int ret = 0;
  int raw_size;
  if (useDispatcher) {  if (!buffer) {
    if(!(buffer = (itape_header_t *)malloc(BUFSIZE))) {
      fprintf(stderr,"Error on the memory allocation\n");
      fprintf(stderr,"look at code of file %s at line %d\n", __FILE__, __LINE__);
      exit(1);
    }
  }

  do {

   
  ret = disIO_readCOOKED(buffer,BUFSIZE,&raw_size,wait);

  if (ret != DISIO_NODATA)

  if (verbose) {
    fprintf(stderr,"GetData: %d %d %d\n",ret,raw_size,DISIO_EOF);
  }

  switch (ret) {
  case DISIO_NODATA:
    break;
  case DISIO_EOF:
    nodata = 0;
    fprintf(stderr,"eof from dispatcher\n");
    retval = 0;
    break;
  case DISIO_COMMAND:
    {
    char *cmd = (char *) malloc(strlen((char *)buffer));
    const char *ptr;
    strcpy(cmd,(char *)buffer);
    nodata = 0;
    if (verbose) {
      fprintf(stderr,"Received command: %s\n",cmd);
    }
     if (strcmp(cmd,"ENDTAPE") == 0)
      retval = 0;
     else
       retval = DISIO_COMMAND;
     ptr = strtok(cmd,":");
     if (verbose)
       fprintf(stderr,"Request data...\n");
   disIO_command("REQUEST_DISPLAY");
   if (cmd)
     free(cmd);
     }
    break;
  case DISIO_OK: 
    nodata = 0;
    if (verbose) {
      fprintf(stderr,"Received %d bytes (DISIO_OK)\n",raw_size);
      fprintf(stderr,"Run: %d\tEvent: %d\n",buffer->runNo,buffer->eventNo);
    }
    itape2bos(buffer,bcs,verbose);
    disIO_command("REQUEST_DISPLAY");
    retval = DISIO_DATA;
    break;
  default: 
    nodata = 0;
    fprintf(stderr,"Unrecognized command %s\n",ret);
  }
  }
  while (ret == DISIO_NODATA);
  }
  else if (fileLun) {
    ret = getBOS(bcs,fileLun,list);
    retval = ret ? DISIO_DATA : ret;
  }
  else {
    fprintf(stderr,"Please initialize data source\n");
    ret = 0;
  }
  return(retval);
}


int getDataRAW(BOSbank *bcs,char *list)
{ 
  int nodata = 0;
  int retval;
  int wait = 0;
  int ret = 0;
  int raw_size;
  if (useDispatcher) {  if (!buffer) {
    if(!(buffer = (itape_header_t *)malloc(BUFSIZE))) {
      fprintf(stderr,"Error on the memory allocation\n");
      fprintf(stderr,"look at code of file %s at line %d\n", __FILE__, __LINE__);
      exit(1);
    }
  }

  do {

   
  ret = disIO_readRAW(buffer,BUFSIZE,&raw_size,wait);

  if (ret != DISIO_NODATA)

    if (verbose)
      fprintf(stderr,"GetData: %d %d %d\n",ret,raw_size,DISIO_EOF);

  switch (ret) {
  case DISIO_NODATA:
    break;
  case DISIO_EOF:
    nodata = 0;
    fprintf(stderr,"eof from dispatcher\n");
    retval = 0;
    break;
  case DISIO_COMMAND:
    {
    char *cmd = (char  *)buffer;
    const char *ptr;
    nodata = 0;
    if (verbose) {
      fprintf(stderr,"Received command: %s\n",cmd);
    }
     if (strcmp(cmd,"ENDTAPE") == 0)
      retval = 0;
     else
       retval = DISIO_COMMAND;
     ptr = strtok(cmd,":");
     if (verbose)
       fprintf(stderr,"Request data...\n");
   disIO_command("REQUEST_DATA");	    
     }
    break;
  case DISIO_OK: 
    nodata = 0;
    if (verbose) {
      fprintf(stderr,"Received %d bytes (DISIO_OK)\n",raw_size);
      fprintf(stderr,"Run: %d\tEvent: %d\n",buffer->runNo,buffer->eventNo);
    }
    itape2bos(buffer,bcs,verbose);
    disIO_command("REQUEST_DATA");
    retval = DISIO_DATA;
    break;
  default: 
    nodata = 0;
    fprintf(stderr,"Unrecognized type: %d\n",ret);
  }
  }
  while (ret == DISIO_NODATA);
  }
  else if (fileLun) {
    ret = getBOS(bcs,fileLun,list);
    retval = ret ? DISIO_DATA : ret;
  }
  else {
    fprintf(stderr,"Please initialize data source\n");
    ret = 0;
  }
  return(retval);
}



int dispatcherConnect(const char*host,int pipelinePrefill)
{
  int i;
  int retry = 2;
  int sleepTime = 5; /* seconds between retries */

  if (disIO_socket >= 0)
    {
      disIO_command("QUIT");
      disIO_disconnect();
    }
	  
  while (retry--)
    {
      int ret = disIO_connect(host,0);
      if (ret==0)
	{
	  fprintf(stderr,"Connected to the Dispatcher at %s\n",host);
	  
	  for (i=0; i<pipelinePrefill; i++) 
	    disIO_command("REQUEST_DATA");
	  return 0;
	}

      fprintf(stderr,"Cannot connect to the Dispatcher at %s, waiting %d sec before retry...\n",host,sleepTime);
 
      sleep(sleepTime);
    }

  fprintf(stderr,"Cannot connect to the Dispatcher at %s, giving up.\n",host);

  return -1;
}

char *getBuffer()
{
  return( (char *) buffer);
}

typedef struct {

          time_t           time;
          unsigned short   millitm;
          short            timezone;
          short            dstflag;

} myTime_t;

double tim()
{
  double fret;
  myTime_t tp;
  ftime(&tp);
  fret = tp.millitm;
  fret /= 1000.0;
  fret += tp.time;
  return( fret );
}

int isDispatcher(char *str)
{
  int ret = 0;
  char local[1000];
  char *word1,*word2;
  strcpy(local,str);
  if (word1 = strtok(local,":")) {
    word2 = strtok( (char *)NULL," \n");
    ret = word2 ? strlen(word2) : 0;
  }
  return(ret);
}
    
  
