c      include file ECHB_NT
c
c_begin_doc
c  RCS ID string
c  $Id: echb_nt.inc,v 1.1 1997/06/11 04:46:17 stepanya Exp $
c
c  Documentation for include file ECHB_NT
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
c  Author:         Created:  Fri Dec 13 00:42:29 EST 1996
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
c  variables for include file ECHB_NT:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIECHB_NT
      DATA CIECHB_NT/
     1'$Id: echb_nt.inc,v 1.1 1997/06/11 04:46:17 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in ECHB_NT common block
c  --------------------------------------------
c
c_end_var
c
      INTEGER  nhit 
      INTEGER  nhitl 
      INTEGER  nhitid 
      INTEGER  layer 
      INTEGER  nsecthit 
      INTEGER  nstrpu 
      INTEGER  nstrpv 
      INTEGER  nstrpw 
      INTEGER  match1
      INTEGER  match2
      REAL     ehit 
      REAL     thit 
      REAL     ihit 
      REAL     jhit 
      REAL     xhit 
      REAL     yhit 
      REAL     zhit 
      REAL     dihit 
      REAL     djhit 
      REAL     dxhit 
      REAL     dyhit 
      REAL     dzhit 
      REAL     widths 
      REAL     distu 
      REAL     distv 
      REAL     distw 
      REAL     pathu 
      REAL     pathv 
      REAL     pathw 
      REAL     m2uhit 
      REAL     m2vhit 
      REAL     m2whit 
      REAL     m3uhit 
      REAL     m3vhit 
      REAL     m3whit
      REAL     m4uhit 
      REAL     m4vhit 
      REAL     m4whit
      REAL     c2match1
      REAL     c2match2
      INTEGER  mxhits
      PARAMETER (mxhits=30)
*
      COMMON/ECHB_NT/
     +nhit,
     +nsecthit(mxhits),
     +layer(mxhits),
     +nhitl(mxhits),
     +nhitid(mxhits),
     +nstrpu(mxhits),
     +nstrpv(mxhits),
     +nstrpw(mxhits),
     +match1(mxhits),
     +match2(mxhits),
     +ehit(mxhits),
     +thit(mxhits),
     +ihit(mxhits),
     +jhit(mxhits),
     +xhit(mxhits),
     +yhit(mxhits),
     +zhit(mxhits),
     +dihit(mxhits),
     +djhit(mxhits),
     +dxhit(mxhits),
     +dyhit(mxhits),
     +dzhit(mxhits),
     +widths(mxhits),
     +distu(mxhits),
     +distv(mxhits),
     +distw(mxhits),
     +pathu(mxhits),
     +pathv(mxhits),
     +pathw(mxhits),
     +m2uhit(mxhits),
     +m2vhit(mxhits),
     +m2whit(mxhits),
     +m3uhit(mxhits),
     +m3vhit(mxhits),
     +m3whit(mxhits),
     +m4uhit(mxhits),
     +m4vhit(mxhits),
     +m4whit(mxhits),
     +c2match1(mxhits),
     +c2match2(mxhits)
c----6----------------------------------------------------------------72




