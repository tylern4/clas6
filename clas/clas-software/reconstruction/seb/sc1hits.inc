c      include file SC1HITS
c
c_begin_doc
c  RCS ID string
c  $Id: sc1hits.inc,v 1.1 1998/03/10 00:07:54 stepanya Exp $
c
c  Documentation for include file SC1HITS
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
c  Author:         Created:  Sun Mar  1 23:08:45 EST 1998
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
c  variables for include file SC1HITS:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CISC1HITS
      DATA CISC1HITS/
     1'$Id: sc1hits.inc,v 1.1 1998/03/10 00:07:54 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in SC1HITS common block
c  --------------------------------------------
c
       INTEGER SC1_hits(Maxsector)
       INTEGER SC1_pd(Maxschits,Maxsector)
       REAL sc1_l(sc_time:sc_gev,Maxschits,Maxsector)
       REAL sc1_r(sc_time:sc_gev,Maxschits,Maxsector)
c_end_var
c
       COMMON/SC1HITS/SC1_hits,SC1_pd,sc1_l,sc1_r
c----6----------------------------------------------------------------72




