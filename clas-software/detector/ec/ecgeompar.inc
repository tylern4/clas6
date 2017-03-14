c      include file ECGEOMPAR
c
c_begin_doc
c  RCS ID string
c  $Id: ecgeompar.inc,v 1.2 1997/05/01 19:50:17 freyberg Exp $
c
c  Documentation for include file ECGEOMPAR
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
c  Author:         Created:  Wed Dec 18 15:16:18 EST 1996
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
c  variables for include file ECGEOMPAR:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIECGEOMPAR
      DATA CIECGEOMPAR/
     1'$Id: ecgeompar.inc,v 1.2 1997/05/01 19:50:17 freyberg Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in ECGEOMPAR common block
c  --------------------------------------------
      real ylow,yhi    ! 
      real dylow,dyhi
      real thickness, tgrho
      real xoff(1:MAXsectors)   ! Xoffset of local coordinate system  
      real yoff(1:MAXsectors)   ! Yoffset of local coordinate system 
      real zoff(1:MAXsectors)   ! Zoffset of local coordinate system
      real n2sect(3,1:MAXsectors)
      real zEClow, zEChi
      real pi,d2rad
c
c_end_var
c
       COMMON/ECGEOMPAR/ylow,yhi,dylow,dyhi,thickness, tgrho,
     +                  xoff,yoff,zoff,n2sect,zEClow, zEChi,pi,d2rad
c----6----------------------------------------------------------------72




