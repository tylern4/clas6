c      include file SCHITS
c
c_begin_doc
c  RCS ID string
c  $Id: schits.inc,v 1.5 2008/03/16 04:33:23 fklein Exp $
c
c  Documentation for include file SCHITS
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
c  Author:  S.Stepanya       Created:  Thu Apr 10 21:19:00 EDT 1997
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
c  variables for include file SCHITS:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CISCHITS
      DATA CISCHITS/
     1'$Id: schits.inc,v 1.5 2008/03/16 04:33:23 fklein Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in SCHITS common block
c  --------------------------------------------
      INTEGER sc_time,sc_gev
      PARAMETER (sc_time=1)
      PARAMETER (sc_gev=sc_time+1)
      INTEGER sc_x,sc_y,sc_z
      PARAMETER (sc_x=sc_gev+1)
      PARAMETER (sc_y=sc_x+1)
      PARAMETER (sc_z=sc_y+1)
      INTEGER sc_dix,sc_diy,sc_diz
      PARAMETER (sc_dix=sc_z+1)
      PARAMETER (sc_diy=sc_dix+1)
      PARAMETER (sc_diz=sc_diy+1)
      INTEGER sc_djx,sc_djy,sc_djz
      PARAMETER (sc_djx=sc_diz+1)
      PARAMETER (sc_djy=sc_djx+1)
      PARAMETER (sc_djz=sc_djy+1)
      INTEGER sc_dkx,sc_dky,sc_dkz
      PARAMETER (sc_dkx=sc_djz+1)
      PARAMETER (sc_dky=sc_dkx+1)
      PARAMETER (sc_dkz=sc_dky+1)
      INTEGER Maxschits
      PARAMETER (Maxschits=60)
      INTEGER scfit_nhits(maxsector)
      INTEGER scfit_hits(Maxschits,maxsector)
      INTEGER scpd_stat(57,maxsector)
c
      REAL scfit_paddle(sc_time:sc_dkz,Maxschits,maxsector)
c_end_var
c
      COMMON/SCHITS/scfit_nhits,scfit_hits,scfit_paddle,scpd_stat
c----6----------------------------------------------------------------72




