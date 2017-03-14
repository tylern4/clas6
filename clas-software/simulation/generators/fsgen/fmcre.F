      SUBROUTINE FMCRE(chini,chfin,NRETLEN)
c
c_begin_doc
c  RCS ID string
c  $Id: fmcre.F,v 1.1.1.1 2006/04/18 21:08:11 stepanya Exp $
c
c  Documentation for subroutine FMCRE
c
c
c  Purpose:  Convert character string from F,F,F,I,I,I,I,B16 ...
c                                     to   (3F,4I,...
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c                     chini - input character string
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c                     chfin - output character string
c                     NRETLEN - output character string length
c
c  Other routines:
c  ---------------
c  Notes:
c  ------
c
c  Author:   Alexander Vlassov      Created:  Tue Aug  6 19:50:19 EDT 1996
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
      integer NRETLEN
      CHARACTER*(*) chini,chfin
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: fmcre.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2006/04/18 21:08:11 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: fmcre.F,v 1.1.1.1 2006/04/18 21:08:11 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FMCRE')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      integer lenocc
      external lenocc

      integer nrep,k,len,i,l,kk,lout,ichar,itab
c
      character*1 comma, bl, obr, cbr
      character*8 BLANK,cold,cnew,cfig
      data comma /','/
      data bl /' '/
      data obr /'('/
      data cbr /')'/
      data BLANK /'        '/
      data itab / 9 /
c
c_end_var
c
c  executable code for routine FMCRE:
c----6----------------------------------------------------------------72
c
      k = lenocc(chini) + 1
      lout = len(chfin)
      chfin = ' '
c
      chfin(1:1) = obr
      cold = BLANK
      cnew = BLANK
      l = 0
      nrep = 0
      NRETLEN = 1
      do i = 1,min(399,k)
c
        if(chini(i:i).eq.comma .or. chini(i:i).eq.bl .or. 
     &             ICHAR(chini(i:i)).EQ.itab) then
          if(cnew.eq.cold) then
            if(cnew.ne.BLANK) then
              nrep = nrep + 1
              cnew = BLANK
            else 
              go to 10
            end if
          else
            if(cold.eq.BLANK) then
              cold = cnew
              nrep = 1 
            else
              write(cfig,99) nrep
 99           format(I8)
              do kk = 1,8
                if(cfig(kk:kk).ne.bl) then
                  NRETLEN = NRETLEN + 1
                  if(NRETLEN.ge.lout) go to 33
                  chfin(NRETLEN:NRETLEN) = cfig(kk:kk)
                end if 
              end do
              cfig = BLANK
              do kk = 1,8
                if(cold(kk:kk).ne.bl) then
                  NRETLEN = NRETLEN + 1
                  if(NRETLEN.ge.lout) go to 33
                  chfin(NRETLEN:NRETLEN) = cold(kk:kk)
                end if 
              end do
              NRETLEN = NRETLEN + 1
              if(NRETLEN.ge.lout) go to 33
              chfin(NRETLEN:NRETLEN) = comma
              cold = cnew
              cnew = BLANK
              nrep = 1
            end if
          end if
          l = 0
c
        else
          l = l + 1
          cnew(l:l) = chini(i:i)
        end if
   10   continue 
      end do
c
      if (cnew(1:1) .ne. blank .and. cnew(1:1) .ne. comma) then
        chfin(nretlen+1:) = cnew
      endif
      nretlen = lenocc(chfin)
      if (chfin(nretlen:nretlen) .ne. comma)  then
        nretlen = nretlen + 1
      endif
      chfin(NRETLEN:NRETLEN) = cbr
c
      RETURN
c
   33 continue
c     ========
c
c
      END
c
c------------------------------------------------------------------------------







