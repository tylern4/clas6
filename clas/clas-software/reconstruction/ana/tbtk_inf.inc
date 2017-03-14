c      include file TBTRK
c
c  Author  :  G. Gavalian 
c  Created :  MON MAR 09, 1998
c  -------
c
c  RCS information: 
      CHARACTER*(132)  CITBTRK_NT
      DATA CITBTRK_NT/
     1'$Id: tbtk_inf.inc,v 1.4 2001/11/01 02:52:03 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in TRK_NT common block
c  --------------------------------------------
        INTEGER  N_trk
        REAL     tX(34,20),tY(34,20),tZ(34,20)
        REAL     tCX(34,20),tCY(34,20),tCZ(34,20)
        REAL     TLEN(34,20)
        INTEGER  sect_trk(20)
c        INTEGER  STAT(34,20)
        INTEGER  WIRE(34,20)
        REAL     DTIME(34,20)
        REAL     ALPHA(34,20)
        REAL     WLEN(34,20)
        REAL     FDOCA(34,20)
        REAL     CDOCA(34,20)

c
c_end_var
c
       COMMON/TBTK/n_trk,sect_trk,wire,tx,ty,tz,tcx,tcy,tcz,
     +tlen,dtime,alpha,wlen,FDOCA,CDOCA

c----6----------------------------------------------------------------72




