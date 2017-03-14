

/*
-----------------------------------------------------------
-
-   File:     bos_event.c
-
-
-
-  Revision history:
-
-  Date       Programmer     Comments
----------------------------------------------------------------
-  9/6/94   DPH     Initial version
---------------------------------------------------------------
-  11/20/95     JAM             split the event print out from
-                               event processing.
---------------------------------------------------------------
-
*/

#include <stdlib.h>
#include <string.h>
#include "bos.h"

/*----- evout_printDCbank -------*/

void  evout_printDCbank(hits, numhits)

DCDataPtr    hits;
int          numhits;

{
  int  j;
  char *header = "\t layer\t  wire\t   tdc\t   adc\n\t-------------------------------";

  if ((hits == NULL) || (numhits == 0))
    return;

  printf("\n\tDC DATA   Number of Hits = %d\n%s\n", numhits, header);

  for (j = 0; j < numhits; j++) {
    printf("\t%6d\t%6d\t%6d\t%6d\n", hits->id/256, hits->id%256, hits->tdc, hits->adc);
    hits++;
  }

}

/*----- evout_printDC0bank -------*/

void  evout_printDC0bank(hits, numhits)

DC0DataPtr    hits;
int          numhits;

{
  int  j;
  char *header = "\t   Layer   Wire    tdc\n\t-------------------------------";

  if ((hits == NULL) || (numhits == 0))
    return;

  printf("\n\tDC0 DATA   Number of Hits = %d\n%s\n", numhits, header);

  for (j = 0; j < numhits; j++) {
    printf("\t%6d\t%6d\t%6d\n", (hits->id / 256), (hits->id % 256), hits->tdc);
    hits++;
  }

}

/*----- evout_printDCHbank -------*/

void  evout_printDCHbank(hits, numhits)
DCHDataPtr   hits;
int          numhits;
{
  int        j;

  printf ("\n\nDrift Chamber Hits bank: number of hits = %d\n\n", numhits);
    printf ("       X          Y        Z       Step    dedx        ptot      tofg      Cx        Cy          Cz       TrkNum   pdg id     layer\n");
  for (j=0; j<numhits; j++) {
    printf ("%9.3g %9.3g %9.3g %9.3g %9.3g %9.3g %9.3g %9.3g %9.3g %9.3g %9d %9d %9d\n",
 hits->X, hits->Y, hits->Z, hits->step, hits->dedx, hits->ptot, hits->tofg, hits->cx, hits->cy, hits->cz, hits->trkNum, hits->pdg_id, hits->wlayer);
    hits++;
  }
}

/*----- evout_printPIbank -------*/

void  evout_printPIbank(hits, numhits)

PixDataPtr   hits;
int          numhits;

{
  int  j;
  char *header = "\t id \t Uid\t Vid\t Wid\t E(GeV)\t X (cm)\t Y (cm)\t Z (cm)\n\t\
---------------------------------------------------------------";

  if ((hits == NULL) || (numhits == 0))
    return;

  printf("\n\tEcPi (inner EC)  DATA   Number of Hits = %d\n%s\n", numhits, header);

  for (j = 0; j < numhits; j++) {
    printf("\t%4d\t%4d\t%4d\t%4d\t%6.3f\t%6.2f\t%6.2f\t%6.2f\n",
        hits->id, hits->Uid, hits->Vid, hits->Wid, hits->E, hits->T,
        hits->X, hits->Y, hits->Z);
    hits++;
  }

}

/*----- evout_printPObank -------*/

void  evout_printPObank(hits, numhits)

PixDataPtr   hits;
int          numhits;

{
  int  j;
  char *header = "\t id \t Uid\t Vid\t Wid\t E(MeV)\t X (cm)\t Y (cm)\t Z (cm)\n\t\
---------------------------------------------------------------";

  if ((hits == NULL) || (numhits == 0))
    return;

  printf("\n\tEcPo (outer EC)  DATA   Number of Hits = %d\n%s\n", numhits, header);

  for (j = 0; j < numhits; j++) {
    printf("\t%4d\t%4d\t%4d\t%4d\t%6.3f\t%6.2f\t%6.2f\t%6.2f\n",
        hits->id, hits->Uid, hits->Vid, hits->Wid, hits->E, hits->T,
        hits->X, hits->Y, hits->Z);
    hits++;
  }

}

/*----- evout_printCCbank -------*/
/*
 * for Alex rename number of hits to number of digi.
 */
void  evout_printCCbank(hits, numhits)

NormalDataPtr    hits;
int              numhits;

{
  int  j;
  char *header = "\t    ID\t   tdc\t   adc\n\t------------------------";

  printf("\n\tFORWARD ANGLE CC DATA   Number of digi = %d\n%s\n", numhits, header);

  for (j = 0; j < numhits; j++) {
    printf("\t%6d\t%6d\t%6d\n", hits->id, hits->tdc, hits->adc);
    hits++;
  }

}

/*----- evout_printCC1bank -------*/
/*
 * for Alex - rename number of hits to number of digi
 */
void  evout_printCC1bank(hits, numhits)

NormalDataPtr    hits;
int              numhits;

{
  int  j;
  char *header = "\t    ID\t   tdc\t   adc\n\t------------------------";

  if ((hits == NULL) || (numhits == 0))
    return;

  printf("\n\tLARGE ANGLE CC DATA   Number of digi = %d\n%s\n", numhits, header);

  for (j = 0; j < numhits; j++) {
    printf("\t%6d\t%6d\t%6d\n", hits->id, hits->tdc, hits->adc);
    hits++;
  }

}

/*----- evout_printCCHbank ------*/

void evout_printCCHbank(hits, numhits)
CCHDataPtr   hits;
int          numhits;
{
int          j;

  printf ("\n\nCerenkov Hits Bank: number of hits = %d\n", numhits);
    printf ("      Xi       Yi          Zi       Xf        Yf         Zf        P        Pnum      Trk       Cseg     itdc        flag\n");
  for (j=0; j<numhits; j++) {

    printf ("%9.3g %9.3g %9.3g %9.3g %9.3g %9.3g %9.3g %9d %9d %9d %9d %9d\n",
        hits->Xi, hits->Yi, hits->Zi, hits->Xf, hits->Yf, hits->Zf, hits->P, hits->num,
        hits->trk, hits->cseg, hits->itdc, hits->hflg);
    hits++;
  }
  printf ("\n");
}


/*----- evout_printECbank -------*/

void  evout_printECbank(hits, numhits)

ECDataPtr    hits;
int          numhits;

{
  int            j, la, st;
  static char   *layernames[7];
  static Boolean firsttime = True;
  char          *header = "\t plane\t  strip\t   tdc\t   adc\n\t-------------------------------";

  if (firsttime) {
    for (j = 0; j < 7; j++)
      layernames[j] = (char *)malloc(7);

    strcpy(layernames[0], "???");
    strcpy(layernames[1], "u-in");
    strcpy(layernames[2], "v-in");
    strcpy(layernames[3], "w-in");
    strcpy(layernames[4], "u-out");
    strcpy(layernames[5], "v-out");
    strcpy(layernames[6], "w-out");
    firsttime = False;
  }

  if ((hits == NULL) || (numhits == 0))
    return;

  printf("\n\tFORWARD ANGLE EC   Number of Hits = %d\n%s\n", numhits, header);

  for (j = 0; j < numhits; j++) {
    la = hits->id / 256;
    st = hits->id % 256;
    if ((la > 6) || (la < 1))
      printf("\t%s\t%6d\t%6d\t%6d\n", layernames[0], st, hits->tdc, hits->adc);
    else
      printf("\t%s\t%6d\t%6d\t%6d\n", layernames[la], st, hits->tdc, hits->adc);

    hits++;
  }

}

/*----- evout_printECHbank -------*/

void  evout_printECHbank(hits, numhits)
ECHDataPtr   hits;
int          numhits;
{
  int        j;

  printf ("\n\nCalorimeter Hits bank: number of hits = %d\n\n", numhits);
  printf ("\tX  \t Y  \t   Z  \t   Cx  \t     Cy  \tCz  \t P \t TrkNum    pdg id\n");
  for (j=0; j<numhits; j++) {
    printf ("%9.3g %9.3g %9.3g %9.3g %9.3g %9.3g %9.3g %9d %9d\n",
        hits->X, hits->Y, hits->Z, hits->cx, hits->cy, hits->cz, hits->P, hits->trkNum, hits->pdg_id);

    hits++;
  }

  printf ("\n");
}

/*----- evout_printEC1bank -------*/

void  evout_printEC1bank(hits, numhits)

EC1DataPtr    hits;
int          numhits;

{
  int            j;
  char          *layernames[5];
  static Boolean firsttime = True;
  char          *header = "\t plane\t  strip\t  tdcL\t  adcL\t  tdcR\t  adcR\n\t----------------------------------------------";

  if (firsttime) {
    for (j = 0; j < 5; j++)
      layernames[j] = (char *)malloc(7);

    strcpy(layernames[0], "???");
    strcpy(layernames[1], "x-in");
    strcpy(layernames[2], "y-in");
    strcpy(layernames[3], "x-out");
    strcpy(layernames[4], "y-out");
    firsttime = False;
  }

  if ((hits == NULL) || (numhits == 0))
    return;

  printf("\n\tLARGE ANGLE EC   Number of Hits = %d\n%s\n", numhits, header);

  for (j = 0; j < numhits; j++) {

    if ((hits->layer > 4) || (hits->layer < 1))
      printf("\t%s\t%6d\t%6d\t%6d\t%6d\t%6d\n", layernames[0], hits->strip,
          hits->tdcleft, hits->adcleft, hits->tdcright, hits->adcright);
    else
      printf("\t%s\t%6d\t%6d\t%6d\t%6d\t%6d\n", layernames[hits->layer], hits->strip,
          hits->tdcleft, hits->adcleft, hits->tdcright, hits->adcright);
    hits++;
  }

}

/*----- evout_printSTHbank -------*/

void  evout_printSTHbank(hits, numhits)

STHDataPtr   hits;
int          numhits;
{
  int        j;

  printf ("\n\nSTH Hits bank: number of hits = %d\n\n", numhits);
    printf ("      X         Y        Z         Cx        Cy        Cz         P         TrkNum       pdg id\n");
  for (j=0; j<numhits; j++) {

    printf ("%9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %12d %12d\n",
        hits->X, hits->Y, hits->Z, hits->cx, hits->cy, hits->cz, hits->P,
        hits->trkNum, hits->pdg_id);
    hits++;
  }
}

/*----- evout_printSCHbank -------*/

void  evout_printSCHbank(hits, numhits)

SCHDataPtr   hits;
int          numhits;
{
  int        j;

  printf ("\n\nScintillator Hits bank: number of hits = %d\n\n", numhits);
  printf ("      X         Y        Z         Cx        Cy        Cz         P         TrkNum       pdg id\n");
  for (j=0; j<numhits; j++) {
    printf ("%9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %12d %12d\n",
        hits->X, hits->Y, hits->Z, hits->cx, hits->cy, hits->cz, hits->P,
        hits->trkNum, hits->pdg_id);
    hits++;
  }
}


/*----- evout_printSCbank -------*/

void  evout_printSCbank(hits, numhits)

SCDataPtr    hits;
int          numhits;

{
  int  j;
  char *header = "\t    id\t tdc L\t adc L\t tdc R\t adc R\n\t---------------------------------------";

  if ((hits == NULL) || (numhits == 0))
    return;

  printf("\n\tSC DATA   Number of Hits = %d\n%s\n", numhits, header);

  for (j = 0; j < numhits; j++) {
    printf("\t%6d\t%6d\t%6d\t%6d\t%6d\n", hits->id, hits->tdcleft,
        hits->adcleft, hits->tdcright, hits->adcright);
    hits++;
  }

}


/*----- evout_printHEADbank -------*/

void  evout_printHEADbank(head)

HEADDataPtr  head;

{
  if (head == NULL)
    return;

  printf("\n-------  HEAD bank --------\n");

  printf("\tVersion Number\t%d\n", head->VersionNumber);
  printf("\t    Run Number\t%d\n", head->RunNumber);
  printf("\t  Event Number\t%d\n", head->EventNumber);
  printf("\t    Event Time\t%d\n", head->EventTime);

  if (head->EventType == -3)
    printf("\t    Event Type\tCLASSim\n");
  else if (head->EventType == -2)
    printf("\t    Event Type\tGEANT\n");
  else if (head->EventType == -1)
    printf("\t    Event Type\tSDA\n");

  else if (head->EventType == 16)
    printf("\t    Event Type\tSync Event\n");
  else if (head->EventType == 17)
    printf("\t    Event Type\tPrestart Event\n");
  else if (head->EventType == 18)
    printf("\t    Event Type\tGo Event\n");
  else if (head->EventType == 19)
    printf("\t    Event Type\tPause\n");
  else if (head->EventType == 20)
    printf("\t    Event Type\tEnd\n");
  else if ((head->EventType > 0) && (head->EventType < 16))
    printf("\t    Event Type\tPhysics Type: %d\n", head->EventType);
  else
    printf("\t    Event Type\t%d\n", head->EventType);

  printf("\t    ROC Status\t%d\n", head->ROCStatus);
  printf("\t   Event Class\t%d\n", head->EventClass);
  printf("\t  Event Weight\t%d\n", head->EventWeight);
}


/*----- evout_printMCINbank -------*/
/*
 *  Changed header for elliott. JAM - 8/9/95
 */
void  evout_printMCINbank(mcin, num)

MCINDataPtr  mcin;
int          num;

{
  int  j, k, ioflag;
  char *header = " Xo  \t Yo  \t Zo  \t Cx  \t Cy  \t Cz  \tP[GeV]\tM[Mev]\t  Q   \tLundID";
  char pname[12];


  if ((mcin == NULL) || (num == 0))
    return;


  printf("\n\tMCIN DATA   Number of MCIN = %d\n%s\n", num, header);

  for (j = 0; j < num; j++) {

/*    ioflag = jpname_(&(mcin->LundID), pname);   */

    if (ioflag == 0) {
      for (k = 11; k > 0; k--)
    if (pname[k] != ' ')
      break;
    else
      pname[k] = '\0';
    }
    else
      strcpy(pname, "??");
    printf("%6.3f\t%6.3f\t%6.3f\t%6.3f\t%6.3f\t%6.3f\t%-6.3f\t%-6.1f\t%3.0f   \t%s\n",
        mcin->Xo, mcin->Yo, mcin->Zo, mcin->Px, mcin->Py, mcin->Pz, mcin->P, 1000.0*mcin->M, mcin->Q, pname);
    mcin++;
  }

}
/* -------------------------- MCEV ------------------ */
void  evout_printMCEVbank(hits, num)

MCEVDataPtr  hits;
int          num;

{
  int  j, k, ioflag;
  char *header = " Seed 1           Seed2";
  char pname[12];


  if ((hits == NULL) || (num == 0))
    return;


  printf("\n\tMCEV DATA   Number of MCEV = %d\n%s\n", num, header);

  printf ("%12d,%12d\n", hits->seed1, hits->seed2);
}

/* -------------------------- MCPA ------------------ */

void  evout_printMCPAbank(hits, num)

MCPADataPtr  hits;
int          num;

{
  int  j, k, ioflag;
  char *header = " Dum1          Dum2";
  char pname[12];


  if ((hits == NULL) || (num == 0))
    return;


  printf("\n\tMCPA DATA   Number of MCPA = %d\n%s\n", num, header);

  printf ("%9.3f,%9.3f\n", hits->dum1, hits->dum2);
}


/*----- evout_printMCVXbank -------*/

void  evout_printMCVXbank(mcvx, num)

MCVXDataPtr  mcvx;
int          num;

{
  int  j, k, ioflag;
  char *header = "\t X   \t Y   \t Z   \t TOF \t Vertex Flag";
  char pname[12];


  if ((mcvx == NULL) || (num == 0))
    return;


  printf("\n\tMCVX DATA   Number of MCVX = %d\n%s\n", num, header);

  for (j = 0; j < num; j++) {
/*
    ioflag = jpname_(&(mcin->LundID), pname);

    if (ioflag == 0) {
      for (k = 11; k > 0; k--)
    if (pname[k] != ' ')
      break;
    else
      pname[k] = '\0';
    }
    else
      strcpy(pname, "??");
*/
    printf("\t%6.3f\t%6.3f\t%6.3f\t%6.3f\t%6d\n",
        mcvx->X, mcvx->Y, mcvx->Z, mcvx->tof, mcvx->vtx_flag);
    mcvx++;
  }

}


/*----- evout_printMCTKbank -------*/

void  evout_printMCTKbank(mctk, num)

MCTKDataPtr  mctk;
int          num;

{
  int  j, k, ioflag;
  char *header = "\n\t Cx   \t Cy   \t Cz";


/*   P   \t P   \t M\t Q\t lund_id\t track_flag\t beg_vtx\t end_vtx\t parent";   */
  char pname[12];


  if ((mctk == NULL) || (num == 0))
    return;


  printf("\n\tMCTK DATA   Number of MCTK = %d\n%s\n", num, header);

  for (j = 0; j < num; j++) {
/*
    ioflag = jpname_(&(mcin->LundID), pname);

    if (ioflag == 0) {
      for (k = 11; k > 0; k--)
    if (pname[k] != ' ')
      break;
    else
      pname[k] = '\0';
    }
    else
      strcpy(pname, "??");
*/
    printf("\t%6.3f\t%6.3f\t%6.3f\n\n\tP\t\t%6.3f\n\tM\t\t%6.3f\n\tQ\t\t%6.3f\n\tlund_id\t\t%6d\n\ttrack_flag\t%6d\n\tbeg_vtx\t\t%6d\n\tend_vtx\t\t%6d\n\tparent\t\t%6d\n\n",
        mctk->Cx, mctk->Cy, mctk->Cz, mctk->P, mctk->M, mctk->Q, mctk->lund_id, mctk->track_flag, mctk->beg_vtx, mctk->end_vtx, mctk->parent);
    mctk++;
  }

}







