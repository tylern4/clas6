#include <stdio.h>

typedef struct {
  int nCol;
  int nRow;
  char name[4];
  int NR;
  int NextIndex;
  int nDataWords;
  int n[1];
} namedBank_t;

typedef struct {
  int nsyst;
  int names;
  int nprim;
  int idnam;
  int idptr;
  int idfmt;
  int ndumm;
  int nresr;
  int nlplm;
  int narr;
  int iarr[10];
  int iefmt;
  int tleft;
  int lepio;
  int nami;
  int indi;
  int indj;
  int inta[200];
} sysbos_t;

int printSysBos(FILE *);

extern sysbos_t sysbos_;
  

  

bosSystem(int *iw,FILE *fp)
{

  char str[100];
  int i;
  int index;
  int next;

  /* dump first 50 system words of bos bank */

  int *base = iw;

  iw += 3;

  fprintf(fp,"BOS SYSTEM DUMP\n");
  fprintf(fp,"\tprint flag for card image records\t%d\n",*iw++);
  fprintf(fp,"\tcard image input\t\t\t%d\n",*iw++);
  fprintf(fp,"\tprint output unit\t\t\t%d\n",*iw++); 
  fprintf(fp,"\tnumber of messages to be printed\t%d\n",*iw++);
  fprintf(fp,"\tnumber of banks to be printed\t\t%d\n",*iw++);

  /* 7 */

 memcpy(str,iw,8);
 str[8] = NULL;

 fprintf(fp,"\tName or array\t\t\t\t%s\n",str);
 iw += 2;
  fprintf(fp,"\tnumber of the array\t\t\t%d\n",*iw++);
 fprintf(fp,"\tlength of the array\t\t\t%d\n",*iw++);
 fprintf(fp,"\taddress of the array\t\t\t%d\n",*iw++);
 index = *iw;
 fprintf(fp,"\tfirst index of the named bank area\t%d\n",*iw++);
 fprintf(fp,"\tfirst index of gap\t\t\t%d\n",*iw++);
 fprintf(fp,"\tfirst index of work bank area\t\t%d\n",*iw++);
 fprintf(fp,"\tnumber of deleted words of named banks\t%d\n",*iw++);
 fprintf(fp,"\tnumber of deleted words of work bands\t%d\n",*iw++);
 fprintf(fp,"\tindex of lowest deleted named bank\t%d\n",*iw++);
 fprintf(fp,"\tindex of highest deleted work bank\t%d\n",*iw++);
 fprintf(fp,"\tindex of link bank\t\t\t%d\n",*iw++);
  /* 20 */

 fprintf(fp,"\tindex of work bank C\t\t\t%d\n",*iw++);
 fprintf(fp,"\tindex of work bank E\t\t\t%d\n",*iw++);
 fprintf(fp,"\tindex of work bank R\t\t\t%d\n",*iw++);
 fprintf(fp,"\tindex of work bank S\t\t\t%d\n",*iw++);
 fprintf(fp,"\tindex of work bank T\t\t\t%d\n",*iw++);
 fprintf(fp,"\tindex of work bank ?\t\t\t%d\n",*iw++);
  /* 26 */

 iw++;
 fprintf(fp,"\tindex of IO statistic bank\t\t%d\n",*iw++);
 fprintf(fp,"\tcheck word (=12345)\t\t\t%d\n",*iw++);
 
 /* 29 */

 fprintf(fp,"\tnumber of return codes\t\t\t%d\n",*iw++);
 fprintf(fp,"\tnumber of return codes\t\t\t%d\n",*iw++);
 fprintf(fp,"\tnumber of return codes\t\t\t%d\n",*iw++);
 fprintf(fp,"\tnumber of return codes\t\t\t%d\n",*iw++);
 fprintf(fp,"\tnumber of return codes\t\t\t%d\n",*iw++);
 fprintf(fp,"\tnumber of return codes\t\t\t%d\n",*iw++);
 /* 35 */

 iw++;
 fprintf(fp,"\tnumber of garbage collections for named banks \t%d\n",*iw++);
 fprintf(fp,"\tnumber of garbage collections (BGARB)\t%d\n",*iw++);
 fprintf(fp,"\tnumber of garbage collections (WGARB)\t%d\n",*iw++); 

 fprintf(fp," histogram of used space before garbage collection\n\tin BGARB and WGARB\n");
 fprintf(fp,"\tand in BDROP before the banks are dropped, in 10 percent bins\n");

 for (i=0; i < 10;++i)
   fprintf(fp,"%d ",iw++);
 fprintf(fp,"\n");

 bosBankIndices(base,100,fp);

 
 /* go through 200 indices, dump bank when index is > 0 */
 for (i = 0; i < 200; ++i) {
   if ( (index = *(base + 50 + i)) > 0) {
     fprintf(fp,"Dump bank index = %d\n",index);
     bosSystemBank(base + index - 6,fp);
   }
 }

 printSysBos(fp);
}

bosBankIndices(int *iw,int ndump,FILE *fp)
{
#define NLINESP 10
  int k = ndump/NLINESP + 1;
  int i,j;
  int *base = iw + 50;
  fprintf(fp,"Dump %d words of BOS index area\n",ndump);
  


  for (i = 0; i < k; ++i) {
    fprintf(fp,"%5d - %5d",i * NLINESP,(i + 1) * NLINESP - 1);
    for (j = 0; j < NLINESP; ++j) {
      fprintf(fp,"%10d ",*base++);
    }
    fputc('\n',fp);
  }
}

 
bosSystemBank(int *iw,FILE *fp)
{

  char str[100];
  int i;
  namedBank_t *bank;

  /* dump sytem words of named bos bank*/

  int *base = iw;
  char name[5];

  bank = (namedBank_t *) base;
  fprintf(fp,"BOS Bank System\n");
  fprintf(fp,"\tThe number of columns\t\t\t%d\n",bank->nCol);
  fprintf(fp,"\tThe number of rows\t\t\t%d\n",bank->nRow);

  memcpy(name,bank->name,4);
  name[4] = NULL;
  fprintf(fp,"\tName:\t\t\t\t%s\n",name);
   fprintf(fp,"\tNR\t\t\t\t%d\n",bank->NR);
  fprintf(fp,"\tIndex of next bank\t\t\t%d\n",bank->NextIndex);
  fprintf(fp,"\tNumber of data words\t\t\t%d\n",bank->nDataWords);


  return(bank->NextIndex);
}

char *asciiP(int *iw)
{
  static char ret[5];
  memcpy(ret,iw,sizeof(int));
  ret[4] = NULL;
  return(ret);
}


superDump(int *iw,int n,FILE *fp)
{

    int i;
  for (i = 0;i < n; ++i) {
    fprintf(fp,"%d\t\t%d\t%s\n",i,*iw,asciiP(iw));
    iw++;
  }
}


printSysBos(FILE *fp)

{

  fprintf(fp,"Number of system words\t\t\t\t\t%d\n",sysbos_.nsyst);
  fprintf(fp,"Max. Number of names\t\t\t\t\t%d\n",sysbos_.names);
  fprintf(fp,"Prime Number for hash search\t\t\t\t%d\n",sysbos_.nprim);
  fprintf(fp,"INDEX OF WORK BANK WITH NAMES\t\t\t\t%d\n",sysbos_.idnam);
  fprintf(fp,"INDEX OF WORK BANK WITH POINTERS\t\t\t%d\n",sysbos_.idptr);
  fprintf(fp,"INDEX OF WORK BANK WITH FORMAT INDICES\t\t\t%d\n",sysbos_.idfmt);
  fprintf(fp,"DUMMY\t\t\t\t\t\t\t%d\n",sysbos_.ndumm);
  fprintf(fp,"NUMBER OF RESERVED WORDS (WBANK)\t\t\t%d\n",sysbos_.nresr);
  fprintf(fp,"NUMBER OF BANKS FOR GIVEN NAME WITHOUT LINK BANK\t%d\n",sysbos_.nlplm);
  fprintf(fp,"NUMBER OF INITIALIZED ARRAYS\t\t\t\t%d\n",sysbos_.narr);
  fprintf(fp,"ERROR FLAG FOR FORMATS\t\t\t\t\t%d\n",sysbos_.iefmt);
  fprintf(fp,"TIME LEFT AT START OF BOS\t\t\t\t%d\n",sysbos_.tleft);
  fprintf(fp,"INIT FLAG FOR EPIO PACKAGE\t\t\t\t%d\n",sysbos_.lepio);
  fprintf(fp,"NAME INDEX\t\t\t\t\t\t%d\n",sysbos_.nami);
  fprintf(fp,"LAST INDEX\t\t\t\t\t\t%d\n",sysbos_.indi);
  fprintf(fp,"INDEX ORIGIN OF INDI\t\t\t\t\t%d\n",sysbos_.indj);
}


  
