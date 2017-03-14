c      include file HBT_PLANES
c
c_begin_doc
c  RCS ID string
c  $Id: hbt_planes.inc,v 1.9 2008/03/16 04:33:23 fklein Exp $
c
c  Documentation for include file HBT_PLANES
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
c  Author:   S.Stepanyan      Created:  Thu Apr 10 20:53:53 EDT 1997
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
c  variables for include file HBT_PLANES:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIHBT_PLANES
      DATA CIHBT_PLANES/
     1'$Id: hbt_planes.inc,v 1.9 2008/03/16 04:33:23 fklein Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in HBT_PLANES common block
c  --------------------------------------------
c
      INTEGER Maxlink
      INTEGER Maxplane
      INTEGER Maxpar
      PARAMETER (Maxlink=40)
      PARAMETER (Maxpar=12)
      PARAMETER (Maxplane=11)
c
      LOGICAL tracking_hb
      INTEGER ntr_link
c      INTEGER sc_plane
      INTEGER itr_sect(Maxlink)
      INTEGER trackid(Maxlink)
      REAL trkp(Maxpar,Maxplane,Maxlink)
c_end_var
c
      COMMON/HBT_PLANES/ntr_link,itr_sect,trackid,trkp,tracking_hb
c----6----------------------------------------------------------------72




