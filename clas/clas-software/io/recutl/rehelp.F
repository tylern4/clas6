      SUBROUTINE REHELP(CHELP)
c
c_begin_doc
c  RCS ID string
c  $Id: rehelp.F,v 1.1.1.1 1997/03/28 20:58:29 marki Exp $
c
c  Documentation for subroutine REHELP
c
c  Purpose:   dumps out help message to screen or across socket
c  --------
c
c  Input Parameters:  CHELP : Char*(*) : character string containing help
c  ----------------                      command
c
c  Output Parameters: NONE
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Thu May 16 08:56:25 EDT 1996
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
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: rehelp.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:29 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: rehelp.F,v 1.1.1.1 1997/03/28 20:58:29 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='REHELP')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  ----------------------------- 
      CHARACTER SPACES*200
      INTEGER LENOCC, TCL_EVAL
      EXTERNAL LENOCC, TCL_EVAL, SPACES
c
      CHARACTER*(*) CHELP
      CHARACTER*(200) FULLPATH, CCMD, help_prompt, chelp2
      CHARACTER*(20) CWORDS(20),CTMP, cwords_new(20)
      INTEGER IEND, NWORDS, IW, ITCL_ERR, NBYT, IBEG, nwords_new
c_end_var
c
c  executable code for routine REHELP:
c----6----------------------------------------------------------------72
c
      help_prompt = 'help> '
      call tmpro(help_prompt(1:lenocc(help_prompt)+1))
 100  continue
c
c  first check how many words are on the line
c
      iend = lenocc(chelp)
      call reword(chelp(1:iend), nwords, cwords)
      cwords(1) = 'help'

 80   continue
       ibeg = 1
       ccmd = ' '
       help_prompt = ' '
       do 10 iw = 1, nwords
        ctmp = cwords(iw)
        if (iw .ne. 1 .and. ctmp .eq. 'help') goto 10
        write(ccmd(ibeg:ibeg+lenocc(ctmp)),23)ctmp(1:lenocc(ctmp))
        write(help_prompt(ibeg:ibeg+lenocc(ctmp)),23)
     1                                          ctmp(1:lenocc(ctmp))
        ibeg = ibeg + lenocc(ctmp) + 1
 23     format(a,'/')
 10    continue
        ctmp = cwords(nwords)
        iend = ibeg +lenocc(ctmp)+3
        write(ccmd(ibeg:iend),43)ctmp(1:lenocc(ctmp))
        ibeg = iend - 3
 43     format(a,'.hlp')
        
        call revinm('RECSIS',ccmd(1:iend),fullpath)
        iend = lenocc(fullpath)
        write(ccmd,37)fullpath(1:iend)

c        write(*,*)ccmd(1:lenocc(ccmd))

 37     format('exec more ',a)
        ITCL_ERR = tcl_eval(ccmd(1:lenocc(ccmd)))

        if (itcl_err .eq. 1) then
         write(crmess,31)ctmp(1:lenocc(ctmp))
 31      format('Sorry no help available on ',a)
         call recmes(crname,'i',crmess)
c
         nwords = nwords - 1
         if (nwords .ge. 1) goto 80
        else
         help_prompt(lenocc(help_prompt):lenocc(help_prompt)+2) = '> '
         call tmpro(help_prompt(1:lenocc(help_prompt)+1))
        endif
c
 90   call asynck_c(nbyt)
      if (nbyt .gt. 0) then
        read(*,13)chelp2
 13     format(a)
        chelp2 = spaces(chelp2,1)
        iend = lenocc(chelp2)
c
c watch out for empty lines
c 
        if (iend .eq. 0) then
          nwords = nwords - 1
          if (nwords .lt. 1) goto 110
          goto 80
        endif
        call cutol(chelp2(1:iend))
        if (chelp2(1:4) .eq. 'quit') goto 110
        call reword(chelp2(1:iend), nwords_new, cwords_new)
        do iw = nwords+1, nwords+nwords_new
         cwords(iw) = cwords_new(iw-nwords)
        enddo
        nwords = nwords + nwords_new
        goto 80
      endif
      goto 90
 110  continue
      RETURN
      END
c
c---------------------------------------------------------------------------

