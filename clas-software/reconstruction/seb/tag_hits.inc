c      include file TAG_HITS
c
c_begin_doc
c
c  Documentation for include file TAG_HITS
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
c  Author:  B.Asavapibhop       Created:  Mon Apr 6 1998
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
c  variables for include file TAG_HITS:
c  -----------------------------------
c----6----------------------------------------------------------------72
c  
c  Variables contained in TAG_HITS common block
c  --------------------------------------------
          
      INTEGER Maxtaghits
      PARAMETER (Maxtaghits=200)
c
      INTEGER taghits,tagstat(Maxtaghits),Tid(Maxtaghits),Eid(Maxtaghits)
      REAL Egamma(Maxtaghits),Tgamma_noRF(Maxtaghits),
     &     Tgamma(Maxtaghits) 
      
c_end_var
c
      COMMON/TAG_HITS/taghits,Egamma,Tgamma_noRF,Tgamma,tagstat,Tid,Eid
c----6----------------------------------------------------------------72




