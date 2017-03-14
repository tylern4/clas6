      SUBROUTINE BNKDMP(BNAME, ISW, NCOL_SCR)
c
c_begin_doc
c  RCS ID string
c  $Id: bnkdmp.F,v 1.7 2000/11/10 21:38:28 boiarino Exp $
c
c  Documentation for subroutine BNKDMP
c
c  Purpose:  dumps to the log file/screen the contents of bank BNAME
c  --------
c
c  Input Parameters:  bname: c4: bank name
c  ----------------   isw  : i : isw=1 IW array isw=2 JW array
c                    ncol  : i : number of columns on the screen
c
c  Output Parameters:  none
c  -----------------
c
c  Other routines:  bnktst
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Tue Jul 30 16:58:12 EDT 1996
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
#include "wcs.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
       CHARACTER*4 BNAME
       INTEGER ISW
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: bnkdmp.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.7 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2000/11/10 21:38:28 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: bnkdmp.F,v 1.7 2000/11/10 21:38:28 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*256  CRMESS
      PARAMETER (CRNAME='BNKDMP')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
       integer ncdump
c       parameter ( ncdump = 7 )
       integer lenocc, mamind
       external lenocc, mamind

       INTEGER NCOL, NROW, NWORDS, I, J, iend, nr, nc, nsect, iloop
       INTEGER nami, istart, i2start, nhead, n, jbeg, jend
       INTEGER JELEMENT, JNDEX, ioffset, k, ncol_scr, field_size
       CHARACTER*8 CDISC(100)
       CHARACTER*256 CFMT,CTMP
       CHARACTER*1 CONE
       character*1024 CHEAD(10)
       LOGICAL LOK
       character*1 crow(27)
       data crow/'*','a','b','c','d','e','f','g','h','i','j','k','l',
     +           'm','n','o','p','q','r','s','t','u','v','w','x',
     +           'y','z'/
       data field_size/14/
c_end_var 
c
c  executable code for routine BNKDMP:
c----6----------------------------------------------------------------72
c
      if (ncol_scr .gt. 0) then
        ncdump = (ncol_scr-10)/field_size 
      endif
      CALL BNKTST(BNAME, LOK, NCOL, CFMT, CDISC)

       IF (.NOT. LOK) THEN
        WRITE(CRMESS,10)BNAME
 10     FORMAT('Bank ',a4,' NOT formatted via ddl file!!')
        CALL RECMES(CRNAME,'W',CRMESS)
        GOTO 9999
       ELSE
        WRITE(CRMESS,11)BNAME, ncol_scr
 11     FORMAT('Performing dump for bank :',a4,': like it or not',I8)
        CALL RECMES(CRNAME,'I',CRMESS)
        
        DO 55 J=1,1+INT((NCOL-1)/ncdump)
         IF (J .NE. 1+INT((NCOL-1)/ncdump) ) THEN
            NC = ncdump
         ELSE
            NC = NCOL - (J-1)*ncdump
         ENDIF
         if (nc .lt. 10) then
           WRITE(CTMP(1:),12)J+2, NC
 12        FORMAT('(','1x,a,x,',I2,'x,',I1,'(5x,a8,x)',')')
         else
           WRITE(CTMP(1:),13) J+2, NC
 13        FORMAT('(','1x,a,x,',I2,'x,',I2,'(5x,a8,x)',')')
         endif
         iend = lenocc(ctmp)
         WRITE(chead(j),ctmp(1:iend))
     1       crow(j),(CDISC(I),I=1+(J-1)*ncdump,(j-1)*ncdump+nc)

c         CALL RECMES(CRNAME,'LN',CHEAD(J))
 55     CONTINUE

c  now find the bank and format the contents for output

        if (isw .ne. 2) then
          nami = mamind(iw,bname)
          Istart = iw(nami)
        else
          nami = mamind(jw,bname)
          Istart = jw(nami)
        endif
 99     continue              
        if (istart .eq. 0) then
          write(crmess,14)bname
 14       format('Sorry no ',a4,' bank found in this event')
          call recmes(crname,'i',crmess)
          return
        else
          if (isw .ne. 2) then
           nsect = iw(istart-2)
           nc = iw(istart - 5)
           nr = iw(istart - 4)
          else
           nsect = jw(istart-2)
           nc = jw(istart - 5)
           nr = jw(istart - 4)
          endif

          if (nc .ne. ncol) then
           write(crmess,115)nc, ncol
115        format('Number of columns in ddl =',i3,
     1            'number of columns in BANK =',i3)
           call recmes(crname,'W',crmess)
          endif

           write(crmess,15)bname,nsect,nc,nr
 15        format('Bank= ',a4,'  Sector= ',i3,
     x        ' Columns=',i3,' Rows=',i3)
           call recmes(crname,'LN',crmess)

           do j = 1, 1+int((nc-1)/ncdump)
             CALL RECMES(CRNAME,'LN',CHEAD(j))
           enddo

           i2start = istart
           if (cfmt(1:3) .eq. 'B16' .or. cfmt(1:3) .eq. 'B32') then
c  double up index since this is I*2 words
            if (cfmt(1:3) .eq. 'B16') then
             i2start = 2*istart
            endif
            do 88 iloop =1,nr
             do j = 1, 1+int((nc-1)/ncdump)
              IF (J .NE. 1+INT(NCOL/ncdump)) THEN
               N = ncdump
              ELSE
               N = NC - (J-1)*ncdump
              ENDIF
              WRITE(CTMP(1:),16) J,N
 16           FORMAT('(','1x,a',',',I1,'x,',I2,'(I12,2x)',')')
              jbeg = i2start + 1 + (iloop-1)*nc + (j-1)*ncdump
              jend = jbeg + N - 1
              iend = lenocc(ctmp)

              if (isw .ne. 2) then
               if (cfmt(1:3) .eq. 'B16') WRITE(CRMESS,ctmp(1:iend))
     1                                  crow(j),(iw16(I),I=jbeg,jend)
               if (cfmt(1:3) .eq. 'B32') WRITE(CRMESS,ctmp(1:iend))
     1                                  crow(j),(iw(I),I=jbeg,jend)
              else
               if (cfmt(1:3) .eq. 'B16') WRITE(CRMESS,ctmp(1:iend))
     1                                  crow(j),(jw16(I),I=jbeg,jend)
               if (cfmt(1:3) .eq. 'B32') WRITE(CRMESS,ctmp(1:iend))
     1                                  crow(j),(jw(I),I=jbeg,jend)
              endif
              CALL RECMES(CRNAME,'LN',CRMESS)
             enddo
 88         continue
           else
c               not B16 not B32 must be variable format oh what fun
c               to do this we must parse CFMT and add the elements
c               to the output CHARACTER string via internal write
c               one by one
c               
              IOFFSET = 3 
              do 199 iloop = 1,nr
                 jelement = 0
                 do 198 j=1,1+int((nc-1)/ncdump)
                  IF (J .NE. 1+INT((NC-1)/ncdump)) THEN
                    N = ncdump
                  ELSE
                    N = NC - (J-1)*ncdump
                  ENDIF
                  crmess(1:) = crow(j)
                  do 197 k=1,n
                    jelement= jelement+1
                    CONE = CFMT((JELEMENT-1)*2+1:(JELEMENT-1)*2+1)
                    jndex = i2start + (iloop-1)*nc + (j-1)*ncdump + k
                    if (k.eq.1) then
                       jbeg = 1+J
                    else
                       jbeg  = jbeg + 15
                    endif
                    JEND = JBEG + 13
                    IF (CONE .EQ. 'F') THEN
                     if (isw .ne. 2) then
                      WRITE(CRMESS(JBEG:JEND),330)RW(JNDEX)
                     else
                      WRITE(CRMESS(JBEG:JEND),330)RJW(JNDEX)
                     endif
 330                 FORMAT(F12.4,'  ')
                    ELSEIF (CONE .EQ. 'I') THEN
                     if (isw .ne. 2) then
                      WRITE(CRMESS(JBEG:JEND),331)IW(JNDEX)
                     else
                      WRITE(CRMESS(JBEG:JEND),331)JW(JNDEX)
                     endif
 331                 FORMAT(i12,'  ')
                    ELSE
                     WRITE(CRMESS(JBEG:JEND),332)
 332                 FORMAT('format??????')                     
                    ENDIF
 197              continue
                  CALL RECMES(CRNAME,'LN',CRMESS)
 198             continue
 199          continue
           endif

c           do j = 1, 1+int((nc-1)/ncdump)
c             CALL RECMES(CRNAME,'LN',CHEAD(j))
c           enddo

           istart = iw(istart-1)
           if (istart .ne. 0) goto 99
          
        endif

       ENDIF
 9999  CONTINUE
      RETURN
      END
c
c------------------------------------------------------------------------------




