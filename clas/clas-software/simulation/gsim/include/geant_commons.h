/* Defines */ 

#define AVO (0.60221367D0)
#define BIG (10000000000.D0)
#define C2TOC1 (7.7)
#define C3TOC1 (2)
#define CLIGHT (29979245800.D0)
#define DEGRAD (0.0174532925199432958D0)
#define EMASS (0.0005109990615D0)
#define EMMU (0.105658387D0)
#define IBLOWN (23)
#define ILTAB (200)
#define ISHLS (29)
#define KSHLS (6)
#define KWBANK (69000)
#define KWWORK (5200)
#define L1SHLS (8)
#define L2SHLS (7)
#define L3SHLS (8)
#define LENGTH (1409)
#define LSTACK (5000)
#define MAXELZ (100)
#define GMAXINT (13)
#define MAXJMP (30)
#define MAXMDT (3)
#define MAXME1 (30)
#define MAXMEC (30)
#define MAXPOW (4)
#define MAXSHL (24)
#define MSLIST (32)
#define MULTRA (50)
#define MXGKIN (100)
#define MXGKIN (100)
#define MXPHOT (800)
#define MXSLNK (100)
#define NMECA (45)
#define NSBOX (1)
#define NSCONE (7)
#define NSCONS (8)
#define NSCTUB (29)
#define NSELTU (13)
#define NSGTRA (28)
#define NSHYPE (14)
#define NSPARA (10)
#define NSPCON (12)
#define NSPGON (11)
#define NSSPHE (9)
#define NSTRAP (4)
#define NSTRD1 (2)
#define NSTRD2 (3)
#define NSTUBE (5)
#define NSTUBS (6)
#define NTRCG (1)
#define NWB (207)
#define NWINT (11)
#define NWREAL (12)
#define NWREV (100)
#define NWS (1500)
#define NWSTAK (12)
#define NWTRAC (NWINT+NWREAL+5)
#define GPI (3.14159265358979324D0)
#define PIBY2 (1.57079632679489662D0)
#define PMASS (0.9382723128D0)
#define RADDEG (57.2957795130823209D0)
#define TVLIM (1296)
#define TWOPI (6.28318530717958648D0)

/* End of Defines */ 

/* ------ Start of file gcasho.inc */

typedef struct { /*  GCASHO */ 
     float zmed;
     float amed;
     float dmed;
     float e0med;
     float zsmed[50];
     float esmed[50];
     float alfa;
     float step;
     float plin;
     float plog;
     float be2;
     float plasm;
     float trnsma;
     float bosc[50];
     float aosc[50];
     float eosc[50];
     float zosc[50];
     float emean;
     float cmgo[2000];
     float emgo;
     float emgomi;
     int nsmed;
     int iosc[50];
     int nosc;
     int nmgo;
     int nmgoma;
}GCASHO_t ; 

/* ------ End of file gcasho.inc */

/* ------ Start of file gcbank.inc */

typedef struct { /*  GCBANK */ 
     int nzebra;
     float gversn;
     float zversn;
     int ixstor;
     int ixdiv;
     int ixcons;
     float fendq[16];
     int lmain;
     int lr1;
     float ws[KWBANK];
}GCBANK_t ; 

/* ------ End of file gcbank.inc */

/* ------ Start of file gccurs.inc */

typedef struct { /*  GCCURS */ 
     int intfla;
     float sizd2;
     float fachv;
     float half;
     float savplx;
     float savply;
     float yplt;
     float xplt;
}GCCURS_t ; 

/* ------ End of file gccurs.inc */

/* ------ Start of file gccuts.inc */

typedef struct { /*  GCCUTS */ 
     float cutgam;
     float cutele;
     float cutneu;
     float cuthad;
     float cutmuo;
     float bcute;
     float bcutm;
     float dcute;
     float dcutm;
     float ppcutm;
     float tofmax;
     float gcuts[5];
}GCCUTS_t ; 

/* ------ End of file gccuts.inc */

/* ------ Start of file gcdraw.inc */

typedef struct { /*  GCDRAW */ 
     int numnod;
     int maxnod;
     int numnd1;
     int levver;
     int levhor;
     int maxv;
     int ipick;
     int mlevv;
     int mlevh;
     int nwcut;
     int jnam;
     int jmot;
     int jxon;
     int jbro;
     int jdup;
     int jsca;
     int jdvm;
     int jpsm;
     int jnam1;
     int jmot1;
     int jxon1;
     int jbro1;
     int jdup1;
     int jsca1;
     int julev;
     int jvlev;
     int looktb[16];
     float grmat0[10];
     float gtran0[3];
     int idrnum;
     float gsin[41];
     float gcos[41];
     float sinpsi;
     float cospsi;
     float gtheta;
     float gphi;
     float gpsi;
     float gu0;
     float gv0;
     float gscu;
     float gscv;
     int ngview;
     int icutfl;
     int icut;
     float ctheta;
     float cphi;
     float dcut;
     int nsurf;
     int isurf;
     float gzua;
     float gzva;
     float gzub;
     float gzvb;
     float gzuc;
     float gzvc;
     float pltrnx;
     float pltrny;
     int linatt;
     int linatp;
     int itxatt;
     int ithrz;
     int iprj;
     float dpers;
     int itr3d;
     int ipkhit;
     int iobj;
     int linbuf;
     int maxgu;
     int morgu;
     int maxgs;
     int morgs;
     int maxtu;
     int mortu;
     int maxts;
     int morts;
     int igu;
     int igs;
     int itu;
     int its;
     int nkview;
     int idview;
     int nopen;
     int igmr;
     int ipions;
     int itrkop;
     int ihiden;
     float zzfu;
     float zzfv;
     int   myisel;
     float ddummy[15];
}GCDRAW_t ; 

/* ------ End of file gcdraw.inc */

/* ------ Start of file gcfdim.inc */

typedef struct { /*  GCFDIM */ 
     int nproc;
     int nmptot;
     float tseqto;
     float totmby;
     float tseq;
     float tlat;
     float tnet;
     float x1min;
     float x1max;
     float y1min;
     float y1max;
     float z1min;
     float z1max;
     float xcut;
     float ycut;
     float zcut;
}GCFDIM_t ; 

/* ------ End of file gcfdim.inc */

/* ------ Start of file gcflag.inc */

typedef struct { /*  GCFLAG */ 
     int idebug;
     int idemin;
     int idemax;
     int itest;
     int idrun;
     int idevt;
     int ieorun;
     int ieotri;
     int ievent;
     int iswit[10];
     int ifinit[20];
     int nevent;
     int nrndm[2];
}GCFLAG_t ; 

typedef struct { /*  GCFLAX */ 
     int batch;
     int nolog;
}GCFLAX_t ; 

/* ------ End of file gcflag.inc */

/* ------ Start of file gcgobj.inc */

typedef struct { /*  GCGOBJ */ 
     int   ist;
     int   ifcg;
     int   ilcg;
     int   ntcur;
     int   nfilt;
     int   ntnex;
     int   kcgst;
     int   ncgvol;
     int   ivfun;
     int   ivclos;
     int   ifacst;
     int   nclas1;
     int   nclas2;
     int   nclas3;
}GCGOBJ_t ; 

typedef struct { /*  CGBLIM */ 
     int   ihole;
     float cgxmin;
     float cgxmax;
     float cgymin;
     float cgymax;
     float cgzmin;
     float cgzmax;
}CGBLIM_t ; 

/* ------ End of file gcgobj.inc */

/* ------ Start of file gchil2.inc */

typedef struct { /*  GCHIL2 */ 
     int larett[2];
     int jtick;
     int jmyll;
     int jfimot;
     int jfisca;
     int jfinam;
     int jaass1;
     int jaass2;
     int jaass3;
     int jaass4;
     int jticks;
     int jmylls;
     int jmymot;
}GCHIL2_t ; 

/* ------ End of file gchil2.inc */

/* ------ Start of file gchiln.inc */

typedef struct { /*  GCHILN */ 
     int larecg[2];
     int jcgobj;
     int jcgcol;
     int jcount;
     int jclips;
     int impoin;
     int imcoun;
     int jsix;
     int jsiy;
     int jsiz;
     int jpxc;
     int jpyc;
     int jpzc;
     int iclip1;
     int iclip2;
}GCHILN_t ; 

/* ------ End of file gchiln.inc */

/* ------ Start of file gchvir.inc */

typedef struct { /*  GCHVIR */ 
     int jvirt;
     int jvdiv;
     int jcont2;
     int jcont;
}GCHVIR_t ; 

/* ------ End of file gchvir.inc */

/* ------ Start of file gcjloc.inc */

typedef struct { /*  GCJLOC */ 
     int njloc[2];
     int jtm;
     int jma;
     int jloss;
     int jprob;
     int jmixt;
     int jphot;
     int janni;
     int jcomp;
     int jbrem;
     int jpair;
     int jdray;
     int jpfis;
     int jmunu;
     int jrayl;
     int jmulof;
     int jcoef;
     int jrang;
}GCJLOC_t ; 

typedef struct { /*  GCJLCK */ 
     int njlck[2];
     int jtckov;
     int jabsco;
     int jeffic;
     int jindex;
     int jcurin;
     int jpolar;
     int jtstra;
     int jtstco;
     int jtsten;
     int jtasho;
}GCJLCK_t ; 

/* ------ End of file gcjloc.inc */

/* ------ Start of file gcjump.inc */

typedef struct { /*  GCJUMP */ 
     int judcay;
     int judigi;
     int judtim;
     int jufld;
     int juhadr;
     int juiget;
     int juinme;
     int juinti;
     int jukine;
     int junear;
     int juout;
     int juphad;
     int juskip;
     int justep;
     int juswim;
     int jutrak;
     int jutrev;
     int juview;
     int jupara;
}GCJUMP_t ; 

/* ------ End of file gcjump.inc */

/* ------ Start of file gckine.inc */

typedef struct { /*  GCKINE */ 
     int ikine;
     float pkine[10];
     int itra;
     int istak;
     int ivert;
     int ipart;
     int itrtyp;
     char napart[4*5];
     float amass;
     float charge;
     float tlife;
     float vert[3];
     float pvert[4];
     int ipaold;
}GCKINE_t ; 

/* ------ End of file gckine.inc */

/* ------ Start of file gcking.inc */

typedef struct { /*  GCKING */ 
     char kcase[4];
     int ngkine;
     float gkin[MXGKIN][5];
     float tofd[MXGKIN];
     int iflgk[MXGKIN];
}GCKING_t ; 

typedef struct { /*  GCKIN2 */ 
     int ngphot;
     float xphot[MXPHOT][11];
}GCKIN2_t ; 

typedef struct { /*  GCKIN3 */ 
     float gpos[MXGKIN][3];
}GCKIN3_t ; 

/* ------ End of file gcking.inc */

/* ------ Start of file gclink.inc */

typedef struct { /*  GCLINK */ 
     int jdigi;
     int jdraw;
     int jhead;
     int jhits;
     int jkine;
     int jmate;
     int jpart;
     int jrotm;
     int jrung;
     int jset;
     int jstak;
     int jgstat;
     int jtmed;
     int jtrack;
     int jvertx;
     int jvolum;
     int jxyz;
     int jgpar;
     int jgpar2;
     int jsklt;
}GCLINK_t ; 

/* ------ End of file gclink.inc */

/* ------ Start of file gclist.inc */

typedef struct { /*  GCLIST */ 
     int nhsta;
     int nget;
     int nsave;
     int nsets;
     int nprin;
     int ngeom;
     int nview;
     int nplot;
     int nstat;
     int lhsta[20];
     int lget[20];
     int lsave[20];
     int lsets[20];
     int lprin[20];
     int lgeom[20];
     int lview[20];
     int lplot[20];
     int lstat[20];
}GCLIST_t ; 

/* ------ End of file gclist.inc */

/* ------ Start of file gclund.inc */

typedef struct { /*  GCLUND */ 
     int iflund;
     float eclund;
}GCLUND_t ; 

/* ------ End of file gclund.inc */

/* ------ Start of file gcmate.inc */

typedef struct { /*  GCMATE */ 
     int nmat;
     char namate[4*5];
     float a;
     float z;
     float dens;
     float radl;
     float absl;
}GCMATE_t ; 

/* ------ End of file gcmate.inc */

/* ------ Start of file gcmulo.inc */

typedef struct { /*  GCMULO */ 
     float sinmul[101];
     float cosmul[101];
     float sqrmul[101];
     float omcmol;
     float chcmol;
     float ekmin;
     float ekmax;
     int nekbin;
     int nek1;
     float ekinv;
     float geka;
     float gekb;
     float ekbin[200];
     float elow[200];
}GCMULO_t ; 

/* ------ End of file gcmulo.inc */

/* ------ Start of file gcmutr.inc */

typedef struct { /*  GCMUTR */ 
     int ncvols;
     int kshift;
     int nshift;
     int icube;
     int nain;
     int jjj;
     int niet;
     int ioldsu;
     int ivoold;
     int iwpoin;
     int ihpoin;
     int ivecvo[100];
     float porgx;
     float porgy;
     float porgz;
     float pox[15];
     float poy[15];
     float poz[15];
     float gboom;
     float pormir[18];
     float pormar[18];
     int ipornt;
     int icgp;
     float clipmi[6];
     float clipma[6];
     float abcd[4];
     float bmin[6];
     float bmax[6];
     float cgb[16000];
     float cgb1[16000];
     float gxmin[MULTRA];
     float gxmax[MULTRA];
     float gymin[MULTRA];
     float gymax[MULTRA];
     float gzmin[MULTRA];
     float gzmax[MULTRA];
     float gxxxx[MULTRA];
     float gyyyy[MULTRA];
     float gzzzz[MULTRA];
}GCMUTR_t ; 

typedef struct { /*  GCMUTC */ 
     char gnash[MULTRA][4];
     char gnnvv[MULTRA][4];
     char gnvnv[MULTRA][4];
}GCMUTC_t ; 

/* ------ End of file gcmutr.inc */

/* ------ Start of file gcmzfo.inc */

typedef struct { /*  GCMZFO */ 
     int iomate;
     int iopart;
     int iotmed;
     int iosejd;
     int iosjdd;
     int iosjdh;
     int iostak;
     int iomzfo[13];
}GCMZFO_t ; 

/* ------ End of file gcmzfo.inc */

/* ------ Start of file gcnum.inc */

typedef struct { /*  GCNUM */ 
     int nmate;
     int nvolum;
     int nrotm;
     int ntmed;
     int ntmult;
     int ntrack;
     int npart;
     int nstmax;
     int nvertx;
     int nhead;
     int nbit;
}GCNUM_t ; 

typedef struct { /*  GCNUMX */ 
     int nalive;
     int ntmsto;
}GCNUMX_t ; 

/* ------ End of file gcnum.inc */

/* ------ Start of file gcomis.inc */

typedef struct { /*  GCOMIS */ 
     int   juinit;
     int   jugeom;
     int jukine;
     int justep;
     int juout;
     int   julast;
}GCOMIS_t ; 

/* ------ End of file gcomis.inc */

/* ------ Start of file gconst.inc */

typedef struct { /*  GCONST */ 
     float pi;
     float twopi;
     float piby2;
     float degrad;
     float raddeg;
     float clight;
     float big;
     float emass;
}GCONST_t ; 

typedef struct { /*  GCONSX */ 
     float emmu;
     float pmass;
     float avo;
}GCONSX_t ; 

/* ------ End of file gconst.inc */

/* ------ Start of file gcopti.inc */

typedef struct { /*  GCOPTI */ 
     int ioptim;
}GCOPTI_t ; 

/* ------ End of file gcopti.inc */

/* ------ Start of file gcpara.inc */

typedef struct { /*  GCPARA */ 
     float epsix0[LSTACK];
     int   idrphi[LSTACK];
     int   idrtet[LSTACK];
     int   idrout[LSTACK];
     int   jplost[LSTACK];
     int   iphtmp[LSTACK];
     int bitphi[LSTACK];
     int bittet[LSTACK];
     int bitpot[LSTACK];
     int   jjlost;
     int   jjfill;
     int   jentry;
     int   jempty;
     float epsmax;
     int   jjtemp;
     int   jjwork;
     int   jjstk1;
     int   j1temp;
     int   j1stk1;
     int   ifounp;
     int   ifount;
     int   ifnpot;
     int symphi;
     int symteu;
     int symted;
}GCPARA_t ; 

/* ------ End of file gcpara.inc */

/* ------ Start of file gcparm.inc */

typedef struct { /*  GCPARM */ 
     int iparam;
     float pcutga;
     float pcutel;
     float pcutne;
     float pcutha;
     float pcutmu;
     int nspara;
     int mpstak;
     int npgene;
}GCPARM_t ; 

/* ------ End of file gcparm.inc */

/* ------ Start of file gcphnr.inc */

typedef struct { /*  GCPHNR */ 
     int ignrfn[MAXELZ][8];
}GCPHNR_t ; 

/* ------ End of file gcphnr.inc */

/* ------ Start of file gcphpr.inc */

typedef struct { /*  GCPHPR */ 
     float gflupr[MAXELZ][4];
}GCPHPR_t ; 

/* ------ End of file gcphpr.inc */

/* ------ Start of file gcphrd.inc */

typedef struct { /*  GCPHRD */ 
     float gphrat[MAXELZ][ISHLS];
     int ishlus[4][24];
     int ishltr[ISHLS];
}GCPHRD_t ; 

/* ------ End of file gcphrd.inc */

/* ------ Start of file gcphxs.inc */

typedef struct { /*  GCPXRN */ 
     char crngup[MAXELZ][GMAXINT][6];
}GCPXRN_t ; 

typedef struct { /*  GCPXCF */ 
     float cofs[MAXELZ][GMAXINT][MAXPOW];
     float gpomin[MAXELZ];
}GCPXCF_t ; 

/* ------ End of file gcphxs.inc */

/* ------ Start of file gcphys.inc */

typedef struct { /*  GCPHYS */ 
     int ipair;
     float spair;
     float slpair;
     float zintpa;
     float steppa;
     int icomp;
     float scomp;
     float slcomp;
     float zintco;
     float stepco;
     int iphot;
     float sphot;
     float slphot;
     float zintph;
     float stepph;
     int ipfis;
     float spfis;
     float slpfis;
     float zintpf;
     float steppf;
     int idray;
     float sdray;
     float sldray;
     float zintdr;
     float stepdr;
     int ianni;
     float sanni;
     float slanni;
     float zintan;
     float stepan;
     int ibrem;
     float sbrem;
     float slbrem;
     float zintbr;
     float stepbr;
     int ihadr;
     float shadr;
     float slhadr;
     float zintha;
     float stepha;
     int imunu;
     float smunu;
     float slmunu;
     float zintmu;
     float stepmu;
     int idcay;
     float sdcay;
     float slife;
     float sumlif;
     float dphys1;
     int iloss;
     float sloss;
     float soloss;
     float stloss;
     float dphys2;
     int imuls;
     float smuls;
     float somuls;
     float stmuls;
     float dphys3;
     int irayl;
     float srayl;
     float slrayl;
     float zintra;
     float stepra;
}GCPHYS_t ; 

typedef struct { /*  GCPHLT */ 
     int ilabs;
     float slabs;
     float sllabs;
     float zintla;
     float stepla;
     int isync;
     int istra;
}GCPHLT_t ; 

/* ------ End of file gcphys.inc */

/* ------ Start of file gcpixe.inc */

typedef struct { /*  GCPIXE */ 
     int limpre;
     int iflape;
     int icolor;
     int ixxx;
     int iyyy;
     int isseen;
     int iscolo;
     int islsty;
     int islwid;
     int isfill;
     int imap;
     int jon;
     int nmap;
     float uuu;
     float vvv;
     float zuv;
     float znmap1;
}GCPIXE_t ; 

/* ------ End of file gcpixe.inc */

/* ------ Start of file gcpoly.inc */

typedef struct { /*  GCPOLY */ 
     int izsec;
     int ipsec;
}GCPOLY_t ; 

/* ------ End of file gcpoly.inc */

/* ------ Start of file gcpush.inc */

typedef struct { /*  GCPUSH */ 
     int ncvert;
     int nckine;
     int ncjxyz;
     int npvert;
     int npkine;
     int npjxyz;
}GCPUSH_t ; 

/* ------ End of file gcpush.inc */

/* ------ Start of file gcrayt.inc */

typedef struct { /*  GCRAYT */ 
     int inten;
     int iombra;
     int ixyfla;
     int noflag;
     float xlpos;
     float ylpos;
     float zlpos;
     float xldir;
     float yldir;
     float zldir;
     float apflah;
     float ccxx[4];
     float ccyy[4];
     float cczz[4];
     float boflag;
     float apflag;
     float xcosxs;
     float ycosys;
     float zcoszs;
     float vdx;
     float vdy;
     float vdz;
     float ssleng;
     float xpints;
     float ypints;
     float zpints;
     float fpintx;
     float fpinty;
     float fpintz;
     float arots[4][4];
     float zrots[4][4];
     float rrr[4];
}GCRAYT_t ; 

/* ------ End of file gcrayt.inc */

/* ------ Start of file gcrz.inc */

typedef struct { /*  GCRZ1 */ 
     int nrecrz;
     int nrget;
     int nrsave;
     int lrget[20];
     int lrsave[20];
}GCRZ1_t ; 

typedef struct { /*  GCRZ2 */ 
     char rztags[8][4];
}GCRZ2_t ; 

/* ------ End of file gcrz.inc */

/* ------ Start of file gcscal.inc */

typedef struct { /*  GCSCAL */ 
     int islink[MXSLNK];
}GCSCAL_t ; 

/* ------ End of file gcscal.inc */

/* ------ Start of file gcscan.inc */

typedef struct { /*  GCSCAN */ 
     int scanfl;
     int nphi;
     float phimin;
     float phimax;
     int nteta;
     float tetmin;
     float tetmax;
     int modtet;
     int iphimi;
     int iphima;
     int iphi1;
     int iphil;
     int nslmax;
     int nslist;
     int islist[MSLIST];
     float vscan[3];
     float factx0;
     float factl;
     float factr;
     int iphi;
     int iteta;
     int iscur;
     float sx0;
     float sabs;
     float tetmid[MAXMDT];
     float tetmad[MAXMDT];
     float sx0s;
     float sx0t;
     float sabss;
     float sabst;
     float factsf;
     float dltphi;
     float dlteta;
     float dphim1;
     float dtetm1;
     float fcx0m1;
     float fcllm1;
     float fcrrm1;
}GCSCAN_t ; 

typedef struct { /*  GCSCAC */ 
     char sfin[80];
     char sfout[80];
}GCSCAC_t ; 

/* ------ End of file gcscan.inc */

/* ------ Start of file gcsets.inc */

typedef struct { /*  GCSETS */ 
     int ihset;
     int ihdet;
     int iset;
     int idet;
     int idtype;
     int nvname;
     int numbv[20];
}GCSETS_t ; 

/* ------ End of file gcsets.inc */

/* ------ Start of file gcshpt.inc */


typedef struct { /*  GCSHPT */ 
     float nshlst[MAXSHL];
     float n1st[MAXELZ];
     float nshlls[MAXELZ];
     float eshell[LENGTH];
}GCSHPT_t ; 

/* ------ End of file gcshpt.inc */

/* ------ Start of file gcspee.inc */

typedef struct { /*  GCSPEE */ 
     float s1;
     float s2;
     float s3;
     float ss1;
     float ss2;
     float ss3;
     int lep;
     int iporli;
     int isubli;
     float sragmx;
     float sragmn;
     float raint1;
     float raint2;
     float rmin1;
     float rmin2;
     float rmax1;
     float rmax2;
     int jporjj;
     int itstcu;
     int ioldcu;
     int iscop;
     int ntim;
     int ntflag;
     int lpass;
     int jsc;
}GCSPEE_t ; 

/* ------ End of file gcspee.inc */

/* ------ Start of file gcstak.inc */

typedef struct { /*  GCSTAK */ 
     int njtmax;
     int njtmin;
     int ntstkp;
     int ntstks;
     int ndbook;
     int ndpush;
     int njfree;
     int njgarb;
     int njinvo;
     int linsav[15];
     int lmxsav[15];
}GCSTAK_t ; 

/* ------ End of file gcstak.inc */

/* ------ Start of file gcstra.inc */

typedef struct { /*  GCSTRA */ 
     float emax;
     float em[200];
     float sfint;
     float epsr[ILTAB];
     float epsi[ILTAB];
     float fint[ILTAB];
     float emin;
     float epps;
     float beta2;
     float gamma2;
     float wp2;
     float s2;
     float meev;
     float emm[200];
     float gamlog[21];
     int np;
     int ntab;
     int ie;
     int nfact;
     int   nicoll;
}GCSTRA_t ; 

/* ------ End of file gcstra.inc */

/* ------ Start of file gctime.inc */

typedef struct { /*  GCTIME */ 
     float timint;
     float timend;
     int itime;
     int igdate;
     int igtime;
}GCTIME_t ; 

/* ------ End of file gctime.inc */

/* ------ Start of file gctmed.inc */

typedef struct { /*  GCTMED */ 
     int numed;
     char natmed[4*5];
     int isvol;
     int ifield;
     float fieldm;
     float tmaxfd;
     float stemax;
     float deemax;
     float epsil;
     float stmin;
     float cfield;
     float prec;
     int iupd;
     int istpar;
     int numold;
}GCTMED_t ; 

typedef struct { /*  GCTLIT */ 
     float thrind;
     float pmin;
     float dp;
     float dndl;
     int jmin;
     int itckov;
     int imckov;
     int npckov;
}GCTLIT_t ; 

/* ------ End of file gctmed.inc */

/* ------ Start of file gctrak.inc */

typedef struct { /*  GCTRAK */ 
     float vect[7];
     float getot;
     float gekin;
     float vout[7];
     int nmec;
     int lmec[MAXMEC];
     char namec[MAXMEC][4];
     int nstep;
     int maxnst;
     float destep;
     float destel;
     float safety;
     float sleng;
     float step;
     float snext;
     float sfield;
     float tofg;
     float gekrat;
     float upwght;
     int ignext;
     int inwvol;
     int istop;
     int igauto;
     int iekbin;
     int ilosl;
     int imull;
     int ingoto;
     int nldown;
     int nlevin;
     int nlvsav;
     int istory;
}GCTRAK_t ; 

typedef struct { /*  GCTPOL */ 
     float polar[3];
     char namec1[MAXME1][4];
}GCTPOL_t ; 

/* ------ End of file gctrak.inc */

/* ------ Start of file gcunit.inc */

typedef struct { /*  GCUNIT */ 
     int lin;
     int lout;
     int nunits;
     int lunits[5];
}GCUNIT_t ; 

typedef struct { /*  GCMAIL */ 
     char chmail[132];
}GCMAIL_t ; 

/* ------ End of file gcunit.inc */

/* ------ Start of file gcursb.inc */

typedef struct { /*  GCURSB */ 
     int numnds;
     int iaddi;
     int numnd2;
     int nnpar;
     int iiselt;
}GCURSB_t ; 

typedef struct { /*  GCURSC */ 
     char momo[4];
}GCURSC_t ; 

/* ------ End of file gcursb.inc */

/* ------ Start of file gcvdma.inc */

typedef struct { /*  GCVDMA */ 
     int nvmany;
     int manyle[20];
     int manyna[15][20];
     int manynu[15][20];
     int nfmany;
     int mycoun;
     int imyse;
     float raytra;
     float veccos[3];
}GCVDMA_t ; 

/* ------ End of file gcvdma.inc */

/* ------ Start of file gcvol1.inc */

typedef struct { /*  GCVOL1 */ 
     int   nlevl1;
     char names1[15][4];
     int numbr1[15];
     int lvolu1[15];
}GCVOL1_t ; 

/* ------ End of file gcvol1.inc */

/* ------ Start of file gcvol2.inc */

typedef struct { /*  GCVOL2 */ 
     int nleve2;
     char names2[15][4];
     int numb2[15];
     int lvol2[15];
     int lind2[15];
     int infro2;
     int nldev2[15];
     int linmx2[15];
     float gtran2[15][3];
     float grmat2[15][10];
     float gonly2[15];
     float glx2[15];
}GCVOL2_t ; 

/* ------ End of file gcvol2.inc */

/* ------ Start of file gcvolu.inc */

typedef struct { /*  GCVOLU */ 
     int nlevel;
     char names[15][4];
     int number[15];
     int lvolum[15];
     int lindex[15];
     int infrom;
     int nlevmx;
     int nldev[15];
     int linmx[15];
     float gtran[15][3];
     float grmat[15][10];
     float gonly[15];
     float glx[3];
}GCVOLU_t ; 

/* ------ End of file gcvolu.inc */

/* ------ Start of file gcxlun.inc */

typedef struct { /*  GCXLUN */ 
     int   lunit[128];
}GCXLUN_t ; 

/* ------ End of file gcxlun.inc */

/* ------ Start of file gfkdis.inc */

typedef struct { /*  GFKDIS */ 
     float zine;
     float zela;
     float ztot;
     int   iint; 
     float sine;
     float sela;
     float fsig;
     int   ifmat;
     int   igf;
}GFKDIS_t ; 

/* ------ End of file gfkdis.inc */

/* ------ Start of file gsecti.inc */

typedef struct { /*  GSECTI */ 
     float aiel[20];
     float aiin[20];
     float aifi[20];
     float aica[20];
     float alam;
     int k0flag;
}GSECTI_t ; 

/* ------ End of file gsecti.inc */

/* ------ Start of file ludat1.inc */

typedef struct { /*  LUDAT1 */ 
     int mstu[200];
     float paru[200];
     int mstj[200];
     float parj[200];
}LUDAT1_t ; 

/* ------ End of file ludat1.inc */

/* ------ Start of file ludat3.inc */

typedef struct { /*  LUDAT3 */ 
     int mdcy[3][500];
     int mdme[2][2000];
     float brat[2000];
     int kfdp[5][2000];
}LUDAT3_t ; 

/* ------ End of file ludat3.inc */

/* ------ Start of file lujets.inc */

typedef struct { /*  LUJETS */ 
     int n;
     int k[5][4000];
     float p[5][4000];
     float v[5][4000];
}LUJETS_t ; 

/* ------ End of file lujets.inc */

/* ------ Start of file pawc.inc */

typedef struct { /*  PAWC */ 
     int nwpaw;
     int ixpawc;
     int ihbook;
     int ixhigz;
     int ixkuip;
     int ifence[5];
     int llmain;
     float wws[9989];
}PAWC_t ; 

/* ------ End of file pawc.inc */
