c      include file USER_ALIGN
c
c_begin_doc
c  RCS ID string
c  $Id: user_align.inc,v 1.1 2001/08/30 16:51:46 feuerbac Exp $
c
c  Documentation for include file USER_ALIGN
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
c  Author:   Robert Feuerbach      Created:  Wed Feb 11 17:02:27 EST 1998
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
c  variables for include file USER_ALIGN:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIUSER_ALIGN
      DATA CIUSER_ALIGN/
     1'$Id: user_align.inc,v 1.1 2001/08/30 16:51:46 feuerbac Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in USER_ALIGN common block
c  --------------------------------------------
c
c_end_var
c
      integer nt_align_id/50/
      integer maxtr,maxlay
      parameter(maxtr=10,maxlay=36)
      integer Evnt,NTrack,sector(maxtr),wire(maxlay,maxtr),
     $     layer(maxlay,maxtr),opstrcks(maxtr)
      real chi2(maxtr),theta(maxtr),phi(maxtr),x0(maxtr),y0(maxtr),
     $ z0(maxtr),alpha(maxlay,maxtr),fitdoca(maxlay,maxtr),
     $ calcdoca(maxlay,maxtr),dtime(maxlay,maxtr),sigma(maxlay,maxtr),
     $ dx(maxlay,maxtr),dy(maxlay,maxtr),dz(maxlay,maxtr),
     $ x(maxlay,maxtr),y(maxlay,maxtr),z(maxlay,maxtr)
      COMMON/USER_ALIGN/Evnt,NTrack,sector,chi2,theta,
     $     phi,x0,y0,z0,opstrcks,
     $     layer,wire,alpha,fitdoca,calcdoca,sigma,dtime,
     $     dx,dy,dz,x,y,z
c----6----------------------------------------------------------------72




