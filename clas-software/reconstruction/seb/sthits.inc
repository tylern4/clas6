c      include file STHITS
c
c_begin_doc
c
c  Documentation for include file STHITS
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
c  Author:  B.Asavapibhop       Created:  Tue Dec 23 1997
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
c  variables for include file STHITS:
c  -----------------------------------
c----6----------------------------------------------------------------72
c  
c  Variables contained in STHITS common block
c  --------------------------------------------
          
      INTEGER Maxsthits
      PARAMETER (Maxsthits=200)
c
      INTEGER sthits,stfit_nhits(maxsector),stfit_trkno(Maxsthits,maxsector)
      INTEGER stfit_status(Maxsthits,maxsector)
      INTEGER stfit_pd(Maxsthits,maxsector)
      REAL stfit_time(Maxsthits,maxsector),stfit_length(Maxsthits,maxsector)
      INTEGER sttype                    ! Start Counter TYPE (0=old, 1 =new)
      
c_end_var
c
      COMMON/STHITS/sthits,stfit_nhits,stfit_pd,stfit_time,stfit_length,stfit_trkno,
     + stfit_status,sttype
c----6----------------------------------------------------------------72




