      real function cc_nphe(theta, phy)
c     ==============================
c
c_begin_doc
c  RCS ID string
c  $Id: cc_nphe.F,v 1.1 1998/05/12 16:55:15 vlassov Exp $
c
c  Documentation for subroutine CC_BEVT
c
c  Purpose:   returnes the mean number of photoelectrons as a 
c  --------   function of projective angles theta, phy (in degrees)
c
c
c
c  Author:   Alexander Vlassov    Created:  May 10 18:16:11 EDT 1998
c  -------
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: cc_nphe.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/05/12 16:55:15 $')
      PARAMETER (CAUTHO = '$Author: vlassov $')
      DATA CRCSID/   
     1'$Id: cc_nphe.F,v 1.1 1998/05/12 16:55:15 vlassov Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*200  CRMESS
      PARAMETER (CRNAME='CC_NPHE')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
       real*4 theta, phy, nph(100,60)
       integer i,j,init
       data init /0/
c
       cc_nphe = 0.
       if(init.eq.0) then
         open(unit=41,file='/group/clas/parms/cc_nphe.txt',
     &   status='OLD', access='SEQUENTIAL',ERR=33)
         read (41,*) nph
         init = 1
       end if
c
       i = 2.0*(theta - 5.)
       if(i.le.1) i =1
       if(i.ge.100) i = 100
       j = phy + 30
       if(j.le.1) j = 1
       if(j.ge.100) j = 100
c
       cc_nphe = nph(i,j)
c       
       return
 33    continue
       CRMESS= ' Can not open file /group/clas/parms/cc_nphe.txt !! '
       CALL RECMES(CRNAME,'F',CRMESS)
       return
       end






