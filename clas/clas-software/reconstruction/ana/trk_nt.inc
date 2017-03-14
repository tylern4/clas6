c      include file TRK_NT
c
c_begin_doc
c  RCS ID string
c  $Id: trk_nt.inc,v 1.1 1997/06/11 04:46:18 stepanya Exp $
c
c  Documentation for include file TRK_NT
c
c  Purpose:
c  --------
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author:  S. Stepanyan       Created:  Sat Feb 15 18:08:34 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c
c_end_doc
c
c
c  Remember NO NESTED INCLUDES
c
c  COMMON BLOCK NAME should be the same as include file name!
c
c_begin_var
c  variables for include file TRK_NT:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CITRK_NT
      DATA CITRK_NT/
     1'$Id: trk_nt.inc,v 1.1 1997/06/11 04:46:18 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in TRK_NT common block
c  --------------------------------------------
      INTEGER MAXtrk
      PARAMETER (MAXtrk=10)
      INTEGER ntrk
      INTEGER itrk(MAXtrk)
      INTEGER isect(MAXtrk)
      REAL ptrk(MAXtrk)
      REAL mtrk(MAXtrk)
      REAL chtrk(MAXtrk)
      REAL xtrktrg(MAXtrk)
      REAL ytrktrg(MAXtrk)
      REAL ztrktrg(MAXtrk)
      REAL cxtrktrg(MAXtrk)
      REAL cytrktrg(MAXtrk)
      REAL cztrktrg(MAXtrk)
c
c_end_var
c
       COMMON/TRK_NT/ntrk,itrk,isect,
     +ptrk,mtrk,chtrk,
     +xtrktrg,ytrktrg,ztrktrg,cxtrktrg,cytrktrg,cztrktrg

c----6----------------------------------------------------------------72




