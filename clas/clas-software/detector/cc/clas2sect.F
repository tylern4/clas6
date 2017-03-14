      SUBROUTINE CLAS2SECT(nsect,x_in,x_out)
c
c_begin_doc
c  RCS ID string
c  $Id: clas2sect.F,v 1.1.1.1 1997/03/28 20:49:51 marki Exp $
c
c  Documentation for subroutine CLAS2SECT
c
c  Purpose:   To transform vector x_in(3) from CLAS R.S. to Sector R.S.
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c                     x_in(3) - real*4 Initial vector
c                     nsect   - sector number
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c                     x_out(3) - real*4 Output vector
c
c  Notes:             x_in and x_out could be the same
c  ------
c
c  Author:   Alexander Vlassov    Created:  Fri Aug 30 09:16:35 EDT 1996
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      integer nsect
      real*4 x_in(3),x_out(3)
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: clas2sect.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:49:51 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: clas2sect.F,v 1.1.1.1 1997/03/28 20:49:51 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='CLAS2SECT')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      real x(3),ss(6),cs(6)
      data ss / 0.,    0.866025403,   0.866025403,
     &          0.,  - 0.866025403, - 0.866025403/          
      data cs / 1., 0.5, -0.5, -1., -0.5, 0.5    /
c_end_var
c
c  executable code for routine CLAS2SECT:
c----6----------------------------------------------------------------72
c
      if(nsect.lt.1 .or. nsect.gt.6.) then
        write(CRMESS,*) 'Wrong SECTOR number :',nsect
        CALL RECMES(CRNAME,'E',CRMESS)
        return
      end if
c
      x(1) =  x_in(1)*cs(nsect) + x_in(2)*ss(nsect)
      x(2) = -x_in(1)*ss(nsect) + x_in(2)*cs(nsect)
      x(3) =  x_in(3)
c
      x_out(1) = x(1)
      x_out(2) = x(2)
      x_out(3) = x(3)
c
      RETURN
      END
c
c-----------------------------------------------------------------------




