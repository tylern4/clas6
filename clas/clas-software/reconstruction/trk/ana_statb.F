
C======================================================================
      SUBROUTINE ana_statb(icut,isec,q)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : keep tallies on different types of
C-                         analysis cuts
C-
C-
C-   Inputs  : icut       - cut type
C-
C-   Outputs :
C-
C-   Controls:
C-
C-   Calls              : none
C-
C-   Created  JUL-20-1992  Billy Leung
C-   mod.     AUG-1997     F.Klein  (similar to SDA '97)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
*  I/O variables
*  -------------
      INTEGER  icut, isec
*
*  Include Files
*  -------------
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: ana_statb.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.5 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2004/09/01 20:26:03 $')
      PARAMETER ( CAUTHO = '$Author: lcsmith $')
      DATA CRCSID/
     1'$Id: ana_statb.F,v 1.5 2004/09/01 20:26:03 lcsmith Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='ANA_STATB')
      PARAMETER (CRAUTH='Franz Klein')
C
      INTEGER TYPE,dtype,ddtype,irun,ievnt,irec,i,j,k,ievnt_old,sum(3,6,10)
      REAL xcut,q
      LOGICAL ack,good(3),newev,icutflag(10)
      CHARACTER*30 caption
      data ievnt_old/0/
C
      COMMON/ErrBook/ type(3,6,10), dtype(10), ddtype(10), xcut(10), ack(10), caption(10)
C

      call rernev(irun,ievnt,irec)
      newev = ievnt.gt.ievnt_old
      
      if (newev) then
        do i=1,10
          icutflag(i)=.false.
          do j=1,6
            do k=1,3
              sum(k,j,i)=0
            enddo
          enddo
        enddo
      endif
      
      IF( ack(icut) ) THEN
        
	if (.not.icutflag(icut)) then
          ddtype(icut)=ddtype(icut)+1
          dtype(icut) = 1
        endif
        icutflag(icut)=.true.
	good(1)=abs(q).lt.2
	good(2)=q.lt.0
	good(3)=q.gt.0
	do i=1,3
	if (good(i).and.sum(i,isec,icut).eq.0) 
     1TYPE(i,isec,icut) = TYPE(i,isec,icut) + 1
	if (good(i)) sum(i,isec,icut)=1
	enddo
      ENDIF
      
      ievnt_old = ievnt
      END
