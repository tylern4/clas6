#include "ced.h"
#include <limits.h>

#ifdef __hpux
#define LONG_BIT 32
#endif
#ifdef __linux__
#define LONG_BIT 32
#endif


/******** local prototypes *********/

static void   freeHugeWord(HugeWordPtr   *hw);

static Boolean GoodHugeWord(HugeWordPtr  hw);

static int  BitCountHugeWord(HugeWordPtr  hw);

static void  NegateHugeWord(HugeWordPtr  hw);

static void  CopyHugeWord(HugeWordPtr  hws,
			  HugeWordPtr  hwd);

static int AddAndCheck(unsigned long u,
		       unsigned long v,
		       unsigned long *sum);

static int SubtractAndCheck(unsigned long u,
			    unsigned long v,
			    unsigned long *diff);


static void HugeWordAdd(HugeWordPtr hwa,
			HugeWordPtr hwb,
			HugeWordPtr hwc);

static void HugeWordSubtract(HugeWordPtr hwa,
			     HugeWordPtr hwb,
			     HugeWordPtr hwc);

static void  RightShiftHugeWord(HugeWordPtr hw,
				int n);

static void  LeftShiftHugeWord(HugeWordPtr hw,
			       int n);


static void  ANDHugeWord(HugeWordPtr  hwa,
			 HugeWordPtr  hwb,
			 HugeWordPtr  hwc);

static void  ORHugeWord(HugeWordPtr  hwa,
			HugeWordPtr  hwb,
			HugeWordPtr  hwc);

static void  XORHugeWord(HugeWordPtr  hwa,
			 HugeWordPtr  hwb,
			 HugeWordPtr  hwc);

static int NonzeroHugeWord(HugeWordPtr hw);

static void  BleedRightHugeWord(HugeWordPtr hw,
				int         n);

static void  BleedRightHugeWord1(HugeWordPtr b,
				 int         n);

static void  BleedLeftHugeWord(HugeWordPtr hw,
			       int n);

static int CountDataHits(HugeWordPtr  *data,
			 int          nlayers);

static int CountDataHits(HugeWordPtr  *data,
			 int          nlayers);

static void RemoveSuperLayerNoise(HugeWordPtr *data,
				  int          nlayers,
				  int          nwires,
				  int          missingLayers,
				  int          *layershifts);

static void FindPossibleSegments(HugeWordPtr    *data,
				 HugeWordPtr    *mydata,
				 HugeWordPtr    *misses,
				 HugeWordPtr    *workspace,
				 HugeWordPtr    segments,
				 int            nlayers,
				 int            missingLayers,
				 int           *layershifts,
				 Boolean        LeftLeaner);


/*---- mallocHugeWord ----*/

HugeWordPtr    mallocHugeWord(int numbits)

{
  HugeWordPtr   temp;

  temp = (HugeWordPtr)malloc(sizeof(HugeWord));
  temp->n = 1 + ((numbits-1)/LONG_BIT);

/*fprintf(stdout, "num longs = %d\n", temp->n); */

  temp->words = (unsigned long *)malloc(temp->n*sizeof(long));
  return temp;
}

/*----- freeHugeWord --------*/

static void   freeHugeWord(HugeWordPtr   *hw)

{
  HugeWordPtr  temp;

  if (hw == NULL)
      return;

  temp = *hw;

  if (temp) {
    if (temp->words)
      free (temp->words);
    free (temp);
    *hw = NULL;
  }
}


/* ----- GoodHugeWord -----*/

static Boolean GoodHugeWord(HugeWordPtr  hw)

{
  if ((!hw) || (!(hw->words))) {
    fprintf(stdout, "bad huge word \n");
    return False;
  }

  return True;
}

/* ----- PrintHugeWord ----*/

void PrintHugeWord(HugeWordPtr  hw)


{
  int  i;
  unsigned long *w;

  if (!GoodHugeWord(hw))
    return;
/*fprintf(stdout, "#words = %d\n", hw->n); */

  w = hw->words;
  for (i = (hw->n - 1); i >= 0; i--) {
    BinaryPrint(w[i], NULL);
  fprintf(stdout, " ");
  }

  fprintf(stdout, "\n");
}


/* ----- ClearHugeWord -----*/

void ClearHugeWord(HugeWordPtr  hw)

{
  int  i;
  unsigned long *w;

  if (!GoodHugeWord(hw))
    return;

  
  w = hw->words;
  for (i = 0; i < hw->n; i++)
    w[i] = 0L;
}


/* ----- BitCountHugeWord ------*/

static int  BitCountHugeWord(HugeWordPtr  hw)

{
  int  count = 0;
  unsigned long *w;
  int  i;

  w = hw->words;
  for (i = 0; i < hw->n; i++)
    count += BitCount(w[i]);

  return  count;
}

/* ----- NegateHugeWord ------*/

static void  NegateHugeWord(HugeWordPtr  hw)

{
  unsigned long *w;
  int  i;
  
  w = hw->words;
  for (i = 0; i < hw->n; i++)
    w[i] = ~w[i];
}

/* ----- CopyHugeWord ------*/


static void  CopyHugeWord(HugeWordPtr  hws,
			  HugeWordPtr  hwd)
{
  int  n;

  n = Hv_iMin(hws->n, hwd->n);

  n = n*sizeof(long);

  memcpy(hwd->words, hws->words, n);
}


/*------- AddAndCheck -------*/

static int AddAndCheck(unsigned long u,
		       unsigned long v,
		       unsigned long *sum)

/* adds two unsigned longs and returns a overflow bit */

{
  unsigned long tu, tv;
  static unsigned long UMAXOVER2 = 0;

  if (UMAXOVER2 == 0)
    UMAXOVER2 = ULONG_MAX/2;

  *sum = u+v;

  if (u > v) {
    tu = u;
    tv = v;
  }
  tu = v;
  tv = u;

  if (tu < UMAXOVER2)
    return 0;

  if (tv > (ULONG_MAX - tu))
    return 1;

  return 0;
}

/*------- SubtractAndCheck -------*/

static int SubtractAndCheck(unsigned long u,
			    unsigned long v,
			    unsigned long *diff)

/* subtracts two unsigned longs and returns a borrow bit */

{
  *diff = u - v;
  if (v > u)
    return -1;

  return 0;
}

/*------- HugeWordAdd --------*/

static void HugeWordAdd(HugeWordPtr hwa,
			HugeWordPtr hwb,
			HugeWordPtr hwc)
{

  int    j;
  unsigned long  *u, *v, *w, k, sum;
  int tk;

  k = 0;

  for (j = 0; j < hwa->n; j++) {
    u = hwa->words + j;
    v = hwb->words + j;
    w = hwc->words + j;
    tk = AddAndCheck(*u, *v, &sum);   /* tk will be 1 if overflowed */

/* need to check remote possibility that k causes an overflow */

    if ((tk == 0) && (k == 1))
      if (sum == ULONG_MAX)
	tk = 1;

    *w = sum + k;
    k = tk;
  }
  
}

/*------- HugeWordSubtract --------*/

static void HugeWordSubtract(HugeWordPtr hwa,
			     HugeWordPtr hwb,
			     HugeWordPtr hwc)
{

  int    j;
  unsigned long  *u, *v, *w, diff;
  
  int k, tk;

  k = 0;

  for (j = 0; j < hwa->n; j++) {
    u = hwa->words + j;
    v = hwb->words + j;
    w = hwc->words + j;
    tk = SubtractAndCheck(*u, *v, &diff);   /* tk will be -1 if borrowed */

/* need to check remote possibility that k causes a borrow */

    if ((tk == 0) && (k == -1))
      if (diff == 0)
	tk = -1;

    *w = diff + k;
    k = tk;
  }
  
}

/* ----- RightShiftHugeWord ------*/

static void  RightShiftHugeWord(HugeWordPtr hw,
				int n)
{
  unsigned long *w;
  int  i, nm1;
  unsigned long temp;
  int shift;

  
  w = hw->words;
  nm1 = hw->n - 1;

  shift = LONG_BIT - n;

  for (i = 0; i < hw->n; i++) {
    w[i] >>= n;

/* now must account for boundaries */

    if (i < nm1) {
      temp = w[i+1] << shift;
      w[i] |= temp;
    }
  }
}

/* ----- LeftShiftHugeWord ------*/

static void  LeftShiftHugeWord(HugeWordPtr hw,
			       int n)
{
  unsigned long *w;
  int  i;
  unsigned long temp;
  int shift;

  w = hw->words;

  shift = LONG_BIT - n;

  for (i = hw->n; i > 0;) {
    w[--i] <<= n;

/* now must account for boundaries */

    if (i > 0) {
      temp = w[i-1] >> shift;
      w[i] |= temp;
    }
  }
}

/*-------- CheckBitHugeWord ------------*/


Boolean CheckBitHugeWord(HugeWordPtr hw,
			 int n)
{
  unsigned long *w;
  int whatword;
  int whatbit;
  unsigned long mask = 1;
  

  whatword = n / LONG_BIT;
  whatbit = n % LONG_BIT;

  if ((whatword < 0) || (whatword >= hw->n))
    return False;

  mask <<= whatbit;

  w = hw->words;

  return ((w[whatword] & mask) == mask);
}


/* ----- SetBitHugeWord ------*/

void  SetBitHugeWord(HugeWordPtr hw,
			    int n)

{
  unsigned long *w;
  int whatword;
  int whatbit;
  unsigned long mask = 1;
  

  whatword = n / LONG_BIT;
  whatbit = n % LONG_BIT;

  if ((whatword < 0) || (whatword >= hw->n))
    return;

  mask <<= whatbit;

  w = hw->words;
  w[whatword] |= mask;

}

/* ----- ANDHugeWord ------*/

static void  ANDHugeWord(HugeWordPtr  hwa,
			 HugeWordPtr  hwb,
			 HugeWordPtr  hwc)

/* hwc = hwa && hwb */

{
  int   i;

  for (i = 0; i < hwa->n; i++)
    hwc->words[i] = hwa->words[i] & hwb->words[i];
}

/* ----- ORHugeWord ------*/

static void  ORHugeWord(HugeWordPtr  hwa,
			HugeWordPtr  hwb,
			HugeWordPtr  hwc)


/* hwc = hwa && hwb */

{
  int  i;
  
  for (i = 0; i < hwa->n; i++)
    hwc->words[i] = hwa->words[i] | hwb->words[i];
}

/* ----- XORHugeWord ------*/

static void  XORHugeWord(HugeWordPtr  hwa,
			 HugeWordPtr  hwb,
			 HugeWordPtr  hwc)

/* hwc = hwa && hwb */


{
  int  i;

  for (i = 0; i < hwa->n; i++)
    hwc->words[i] = hwa->words[i] ^ hwb->words[i];
}

/* ----- BleedRightHugeWord ------*/

static void  BleedRightHugeWord(HugeWordPtr hw,
				int         n)

{

  int  j, m, k;
  static HugeWordPtr  thw = NULL;

  
  if (!thw)
    thw = mallocHugeWord(192);

  CopyHugeWord(hw, thw);

  if (n < 4) {
    for (j = 0; j < n; j++) {
      RightShiftHugeWord(thw, 1);
      ORHugeWord(hw, thw, hw);
    }
    return;
  }

  m = (n+1)/2;
  k = n-m;

  for (j = 0; j < m; j++) {
    RightShiftHugeWord(thw, 1);
    ORHugeWord(hw, thw, hw);
  }
  
  CopyHugeWord(hw, thw);
  RightShiftHugeWord(thw, k);
  ORHugeWord(hw, thw, hw);
}


/******** NonzeroHugeWord *******/

static int NonzeroHugeWord(HugeWordPtr hw)

{
  unsigned long *w;
  int i;
  
  w = hw->words;
  for (i = 0; i < hw->n; i++)
    if (w[i] != 0)
      return 1;
  
  return 0;
}

/* ----- BleedRightHugeWord1 ------*/

static void  BleedRightHugeWord1(HugeWordPtr b,
				 int         n)

{

  int                 shift;
  static HugeWordPtr  tb = NULL;
  static HugeWordPtr  t2 = NULL;
  static HugeWordPtr  one = NULL;

  if (n < 1)
    return;
  
  if (!tb) {
    tb = mallocHugeWord(192);
    t2 = mallocHugeWord(192);
    one = mallocHugeWord(192);
    ClearHugeWord(one);
    one->words[0] = 1;
  }

/* first compute b & ((1<<n) - 1), which is really
   the lower n bits of b */

  ClearHugeWord(t2);
  shift = LONG_BIT - n;
  t2->words[0] = (b->words[0] << shift) >> shift;

/* now compute b - (b>>n) */

  CopyHugeWord(b, tb);
  RightShiftHugeWord(tb, n);
  HugeWordSubtract(b, tb, tb);
/*
  if (!!(t2->words[0]))
    HugeWordSubtract(tb, one, tb);
*/
  ORHugeWord(b, tb, b);

}


/* ----- BleedLeftHugeWord ------*/

static void  BleedLeftHugeWord(HugeWordPtr hw,
			       int n)
{

  int  j, m, k;
  static HugeWordPtr  thw = NULL;

  
  if (!thw)
    thw = mallocHugeWord(192);

  CopyHugeWord(hw, thw);


  if (n < 4) {
    for (j = 0; j < n; j++) {
      LeftShiftHugeWord(thw, 1);
      ORHugeWord(hw, thw, hw);
    }
    return;
  }

  m = (n+1)/2;
  k = n-m;

  for (j = 0; j < m; j++) {
    LeftShiftHugeWord(thw, 1);
    ORHugeWord(hw, thw, hw);
  }

  CopyHugeWord(hw, thw);
  LeftShiftHugeWord(thw, k);
  ORHugeWord(hw, thw, hw);
}

/*------- FindPossibleSegments --------*/

static void FindPossibleSegments(HugeWordPtr    *data,
				 HugeWordPtr    *mydata,
				 HugeWordPtr    *misses,
				 HugeWordPtr    *workspace,
				 HugeWordPtr    segments,
				 int            nlayers,
				 int            missingLayers,
				 int           *layershifts,
				 Boolean        LeftLeaner)

/********************************************************
HugeWordPtr    *data;    the real data
HugeWordPtr    *mydata;  workspace to make copy
HugeWordPtr    *misses;
HugeWordPtr    *workspace;
HugeWordPtr    segments;
int            nlayers;
Boolean        LeftLeaner;  if true, look for left leaners else right leaners
****************************************************************/

/* finds possible segments in dc data based on the
   bucket define in layershifts */


{
  int  i, j, numcheck;
  static HugeWordPtr AllOnes = NULL;

  if (!AllOnes) {
    AllOnes = mallocHugeWord(192);
    ClearHugeWord(AllOnes);
    NegateHugeWord(AllOnes);   /* misses start out as all 1's */
  }


/* initialization */

  for (i = 0; i < nlayers; i++) {
    CopyHugeWord(data[i], mydata[i]);
    CopyHugeWord(AllOnes, misses[i]);
  }
  
/* shift the data for each layer according to the layer shift
   (i.e the buckets) and the type (left or right) */

  for (i = 1; i < nlayers; i++) {
    if (LeftLeaner)
      BleedRightHugeWord(mydata[i], layershifts[i]);
    else
      BleedLeftHugeWord(mydata[i], layershifts[i]);
  }
  

/* segments start out as copy of first layer */

  CopyHugeWord(mydata[0], segments);

/* now AND the other layers, which have been shifted to accomodate
   the buckets */

  for (i = 0; i < nlayers;) {
    if (i > 0) 
      ANDHugeWord(segments, mydata[i], segments);


/* Now take missing layers into account. missingLayers
   is the max number of missing layers allowed. However
   there is no need to check more misses the layer that we
   are presently investigating.*/

    if (++i < missingLayers)   /* note from this step i is the "NEXT" layer */
      numcheck = i;
    else
      numcheck = missingLayers;

/* note: numcheck is always > 0 unless a level shift is set
   to zero which is unlikely. (in which case segments will
   be unnecessarily copied onto workspace[0] and back again.
   The algorithm still would work. I have chosen to save the
   check on (numcheck > 0) since it shouldn't happen */

    CopyHugeWord(segments, workspace[0]);
    for (j = 0; j < numcheck; j++) {

/* first step: use whatever misses are left for this j */

      ORHugeWord(workspace[j], misses[j], workspace[j+1]);

/* second step: remove used up misses */

      ANDHugeWord(misses[j], workspace[j], misses[j]);
    }
    CopyHugeWord(workspace[numcheck], segments);
    
  } /* end of layer loop */

}

/*-------------- RemoveDCNoise --------------*/

void           RemoveDCNoise(Hv_View     View,
			     int         sect)

{
    int rgn, supl, lay, nlay, nwire;
    HugeWordPtr   hwd, hwc;

/* first copy the dirty data to the clean data */

/*    fprintf(stderr, "In removenoise sect = %d\n", sect+1); */

    for (rgn = 0; rgn < 3; rgn++)
	for (supl = 0; supl < 2; supl++) {
	    nlay  = dcbitdata[sect][rgn][supl].nlayer;
	    nwire = dcbitdata[sect][rgn][supl].nwire;
	    for (lay = 0; lay < nlay; lay++) {
		hwd = dcbitdata[sect][rgn][supl].data[lay];
		hwc = cleandcbitdata[sect][rgn][supl].data[lay];
		CopyHugeWord(hwd, hwc);
	    }
	    RemoveSuperLayerNoise(cleandcbitdata[sect][rgn][supl].data,
				  nlay,
				  nwire,
				  missinglayers[rgn][supl],
				  layershifts);
	}
    
}

/* ------ RemoveSuperLayerNoise -----*/

static void RemoveSuperLayerNoise(HugeWordPtr *data,
				  int          nlayers,
				  int          nwires,
				  int          missingLayers,
				  int          *layershifts)

/************************************************
HugeWordPtr *data;     actual data with noise 
int          nlayers;  number of layers in this superlayer
int          nwires;   max number of wires in a layer in thissuperlayer
**************************************************/
{
  static HugeWordPtr  Lsegments  = NULL;       /*cumulative left segments*/
  static HugeWordPtr  Rsegments  = NULL;       /*cumulative rght segments*/
  static HugeWordPtr  *mydata    = NULL;
  static HugeWordPtr  *misses    = NULL;
  static HugeWordPtr  *workspace = NULL;
  static int          nw = 0;
  static int          nl = 0;

  int    i;

/* (re)malloc data if
       a) it hasn't been malloced yet  (Lsegments = NULL) or
       b) the number of superlayers has changed
       c) the number of wires has changed

note that if we are remallocing we must first free the old data
and nl will be the number of layers most recently malloced */

 

  if (!Lsegments || (nl != nlayers) || (nw != nwires)) {

/* free if we have some */

      if (mydata) {
	  freeHugeWord(&Lsegments);
	  freeHugeWord(&Rsegments);

 

	  for (i = 0; i < nl; i++) {
	      freeHugeWord(&(mydata[i]));
	      freeHugeWord(&(misses[i]));
	      if (workspace != NULL)
		  freeHugeWord(&(workspace[i]));
	  }
	  
	  if (workspace != NULL)
	      freeHugeWord(&(workspace[nl]));   /*workspace has nl + 1 hugewords */

	  if (mydata != NULL)
	      free(mydata);

	  if (misses != NULL)
	      free(misses);

	  if (workspace != NULL)
	      free(workspace);

      } /* end of freeing */

#ifdef DEBUG
      fprintf(stdout, "all required frees succeeded\n");
#endif  

/* now set nl to the present number of super layers */

      nl = nlayers;
      nw = nwires;

/* bits set in segments correspond to possible left or right leaning
   tracks originating from that wire in layer one (which may in fact
   be missing! */
    
      Lsegments = mallocHugeWord(nw);
      Rsegments = mallocHugeWord(nw);
      
      misses = (HugeWordPtr *)malloc(nl*sizeof(HugeWordPtr));
      mydata = (HugeWordPtr *)malloc(nl*sizeof(HugeWordPtr));
      workspace = (HugeWordPtr *)malloc((nl+1)*sizeof(HugeWordPtr));
    
      for (i = 0; i < nl; i++) {
	  misses[i] = mallocHugeWord(nw);
	  mydata[i] = mallocHugeWord(nw);
	  workspace[i] = mallocHugeWord(nw);
      }
      workspace[nl] = mallocHugeWord(nw);
  } /* end of allocation block */


#ifdef DEBUG
  fprintf(stdout, "Looking for possible segments\n");
#endif  
  
  FindPossibleSegments(data, mydata, misses, workspace, Lsegments,
		       nl, missingLayers, layershifts, True);   /*left leaners */
  FindPossibleSegments(data, mydata, misses, workspace, Rsegments,
		       nl, missingLayers, layershifts, False);  /*rght leaners */



#ifdef DEBUG
  fprintf(stdout, "Removing Noise\n");
#endif  

/* now remove the noise */


/* first, set mydata[0] to contain overlap of both sets of segments
   NOTE: the first layer (layer 0) NEVER has a layer shift.*/

  ORHugeWord(Lsegments, Rsegments, mydata[0]);
  ANDHugeWord(data[0], mydata[0], data[0]);


  for (i = 1; i < nlayers; i++) {
/* copy segments onto a given layer and bleed to create left
   and right buckets */

    CopyHugeWord(Lsegments, mydata[i]);
    BleedLeftHugeWord(mydata[i], layershifts[i]); 

    CopyHugeWord(Rsegments, workspace[0]);
    BleedRightHugeWord(workspace[0], layershifts[i]);

/* combine left and right buckets */

    ORHugeWord(mydata[i], workspace[0], mydata[i]);

/* now get overlap of original data with buckets */

    ANDHugeWord(data[i], mydata[i], data[i]);

  }


}


/*------ CountDataHits -----------*/

static int CountDataHits(HugeWordPtr  *data,
			 int          nlayers)

{
  int   count = 0;
  int   i;


  for (i = 0; i < nlayers; i++)
    count += BitCountHugeWord(data[i]);
  
  return count;
}


