      SUBROUTINE REFILL
c
c_begin_doc
c  RCS ID string
c  $Id: refill.F,v 1.3 1999/07/03 02:16:21 boiarino Exp $
c
c  Documentation for subroutine REFILL
c
c  Purpose: fills histograms book interactively
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
c  Author:   Arne Freyberger      Created:  Fri Sep 13 13:59:37 EDT 1996
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
#include "bcs.inc"
#include "rhist.inc"
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
      PARAMETER (CFILE = '$RCSfile: refill.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/07/03 02:16:21 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: refill.F,v 1.3 1999/07/03 02:16:21 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='REFILL')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      integer mlink
      external mlink

      character*4 oldbname
      logical lok
      integer ncol_ddl
      character*256 cfmt, pres_dir
      character*8 cdisc(100)
      integer ih, irow, icol, ir, indx, ifmtx, ifmty
      integer irowy, icoly, indy, it, oldrec
      real x, y, xtup(25)
      
c_end_var
c
c  executable code for routine REFILL:
c----6----------------------------------------------------------------72
c
       if (nhist_max .lt. 1) return
c  first move to the correct directory
         call hcdir(PRES_DIR,'R')
         oldbname = 'none'
         do 99 ih = 1, nhist_max
          call hcdir(dirhist(ih),' ')

          IF (NDIMTUP(IH) .eq. 0) THEN
            if (oldbname .ne. xbname(ih) .or. 
     1          xrec(ih) .ne. oldrec) then

                oldbname = xbname(ih)            
                oldrec = xrec(ih)
                indx = mlink(iw,xbname(ih),xrec(ih))
            endif

            if (indx .eq. 0) goto 99

            irow = iw(indx-4)
            icol = iw(indx-5)

            ifmtx = xfmt(ih)
            ifmty = yfmt(ih)

c begin LMQ 6/18/97
            if(ifmtx .eq. 0 .and. ifmty .eq. 0) goto 99
c end

            if (ifmty .ne. 0) then

               if (ybname(ih) .ne. xbname(ih) .or. 
     1               xrec(ih) .ne. yrec(ih))  then
                 indy = mlink(iw,ybname(ih),yrec(ih))
                 if (indy .eq. 0) goto 99
                 irowy = iw(indy-4)
                 icoly = iw(indy-5)
               else
                 indy = indx
                 irowy = irow
                 icoly = icol
               endif

            else
              irowy = irow
            endif


            do ir = 1, min(irow,irowy)
             if (ifmtx .eq. 1) then
              x = iw(indx+(ir-1)*icol+xcol(ih))
             elseif (ifmtx .eq. 2) then
              x = iw16(indx*2+(ir-1)*icol+xcol(ih))
             else
              x = rw(indx+(ir-1)*icol+xcol(ih))
             endif
             y = 0.0
             if (ifmty .ne. 0) then
              if (ifmty .eq. 1) then
               y = iw(indy+(ir-1)*icoly+ycol(ih))
              elseif (ifmty .eq. 2) then
               y = iw16(indy*2+(ir-1)*icoly+ycol(ih))
              else
               y = rw(indy+(ir-1)*icoly+ycol(ih))
              endif

              call hf2(idhist(ih),x,y,1.0)
             else

              call hf1(idhist(ih),x,1.0)
             endif
            enddo             

          ELSE
             indx = mlink(iw,cbnktup(ih,1),xrectup(ih,1))
             if (indx .eq. 0) goto 99
             irow = iw(indx-4)
             icol = iw(indx-5)
             do ir = 1, irow
               do it = 1,ndimtup(ih)
                ifmtx= xfmttup(ih,it) 
                if (ifmtx .eq. 1) then
                 xtup(it) = iw(indx+(ir-1)*icol+xcoltup(ih,it))
                elseif (ifmtx .eq. 2) then
                 xtup(it) = iw16(indx*2+(ir-1)*icol+xcoltup(ih,it))
                else
                 xtup(it) = rw(indx+(ir-1)*icol+xcoltup(ih,it))
                endif
               enddo
               call hfnov(idhist(ih),xtup)
             enddo
          ENDIF
            
 99      continue
            call hcdir(pres_dir,' ')
      RETURN
      END
c
c------------------------------------------------------------------------------




