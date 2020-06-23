      SUBROUTINE ALIGN_BOOK
c
c_begin_doc
c  RCS ID string
c  $Id: align_book.F,v 1.2 2002/05/27 23:17:23 sam Exp $
c
c  Documentation for subroutine ALIGN_BOOK
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Robert Feuerbach      Created:  Wed Feb 11 16:53:58 EST 1998
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
c BOS common block  uncomment the next line for BOS include file
c#include "bcs.inc"
#include "user_align.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: align_book.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2002/05/27 23:17:23 $')
      PARAMETER (CAUTHO = '$Author: sam $')
      DATA CRCSID/   
     1'$Id: align_book.F,v 1.2 2002/05/27 23:17:23 sam Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ALIGN_BOOK')
      PARAMETER (CRAUTH='Robert Feuerbach')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      character*500 chform,title
      integer lform,sect

c_end_var
c
c  executable code for routine ALIGN_BOOK:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

      call hbnt(nt_align_id,'Alignment',' ')
      
      
      chform(:) = ' '
      call hnform(chform,'evnt',0,'I',0.,0.)
      call hnform(chform,'ntrack',0,'I',0.,real(maxtr))
      call hnform(chform,'sector(ntrack)',0,'I',0.,0.)
      call hnform(chform,'chi2(ntrack)',0,'R',0.,0.)
      call hnform(chform,'theta(ntrack)',0,'R',0.,0.)
      call hnform(chform,'phi(ntrack)',0,'R',0.,0.)
      call hnform(chform,'x0(ntrack)',0,'R',0.,0.)
      call hnform(chform,'y0(ntrack)',0,'R',0.,0.)
      call hnform(chform,'z0(ntrack)',0,'R',0.,0.)
      call hnform(chform,'opstrcks(ntrack)',0,'I',0.,0.)
      call hnform(chform,'layer(*,ntrack)',maxlay,'I',0.,36.)
      call hnform(chform,'wire(*,ntrack)',maxlay,'I',-193.,193.)
      call hnform(chform,'alpha(*,ntrack)',maxlay,'R',0.,0.)
      call hnform(chform,'fitdoca(*,ntrack)',maxlay,'R',0.,0.)
      call hnform(chform,'calcdoca(*,ntrack)',maxlay,'R',0.,0.)
      call hnform(chform,'sigma(*,ntrack)',maxlay,'R',0.,0.)
      call hnform(chform,'dtime(*,ntrack)',maxlay,'R',0.,0.)
      call hnform(chform,'dx(*,ntrack)',maxlay,'R',0.,0.)
      call hnform(chform,'dy(*,ntrack)',maxlay,'R',0.,0.)
      call hnform(chform,'dz(*,ntrack)',maxlay,'R',0.,0.)
      call hnform(chform,'x(*,ntrack)',maxlay,'R',0.,0.)
      call hnform(chform,'y(*,ntrack)',maxlay,'R',0.,0.)
      call hnform(chform,'z(*,ntrack)',maxlay,'R',0.,0.)

      chform = 'evnt:I,ntrack[0,10]:I,sector(ntrack):I,' //
     $     'chi2(ntrack):R,theta(ntrack):R,phi(ntrack):R,x0(ntrack):R,' //
     $     'y0(ntrack):R,z0(ntrack):R,opstrcks(ntrack):I,' //
     $     'layer(36,ntrack)[0,36]:I,wire(36,ntrack)[-193,193]:I,' //
     $     'alpha(36,ntrack):R,fitdoca(36,ntrack):R,calcdoca(36,ntrack):R,' //
     $     'sigma(36,ntrack):R,dtime(36,ntrack):R,' //
     $     'dx(36,ntrack):R,dy(36,ntrack):R,dz(36,ntrack):R,' //
     $     'x(36,ntrack):R,y(36,ntrack):R,z(36,ntrack):R '
      

      lform = index(chform,' ') - 1

      write(*,'(1x,A,A)')'chform is',chform(:lform)
      write(*,*)' '
      call hbname(nt_align_id,'Shifts',evnt,chform(:lform))

      do sect = 1,6
c        write(title,'(A,I2,A)')'Calc-fitdoca vs layer Sector ',sect,'$'
        write(title,'(A,I2,A)')'fitdoct-Calc vs layer Sector ',sect,'$'
        call hbook2(1000+sect,title,400,-2.,2.,36,0.5,36.5,0.)
c        write(title,'(A,I2,A)')'Calc-fitdoca vs layer Sector ',sect,
        write(title,'(A,I2,A)')'fitdoct-Calc vs layer Sector ',sect,
     &        '15"L#theta"L#25$'
        call hbook2(1100+sect,title,400,-2.,2.,36,0.5,36.5,0.)
c        write(title,'(A,I2,A)')'Calc-fitdoca vs layer Sector ',sect,
        write(title,'(A,I2,A)')'fitdoct-Calc vs layer Sector ',sect,
     &        '75"L#theta"L#115$'
        call hbook2(1200+sect,title,400,-2.,2.,36,0.5,36.5,0.)
      enddo

      RETURN
      END
c
c------------------------------------------------------------------------------




