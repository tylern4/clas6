      subroutine gmail(before,after)

c_begin_doc

c  Documentation for subroutine gmail

c  $Id: gmail.F,v 1.2 1997/04/24 15:38:35 holtrop Exp $

c  Purpose:
c  --------
c     intercepts Geant messages and routes them through Arne's recmes system

C.    ******************************************************************
C.    *                                                                *
C.    *       Send a mail on the output device (usually unit LOUT)     *
C.    *       The mail is in character array CHMAIL of /GCMAIL/        *
C.    *       Max length is 132 chars and trailing blanks are stripped *
C.    *       LINBEF lines are skipped before mail and LINAFT after    *
C.    *                                                                *
C.    *    ==>Called by : many routines                                *
C.    *       Authors : R.Brun, P.Zanarini   *********                 *
C.    *                                                                *
C.    ******************************************************************
C.
c  Notes:
c  ------

c  Author:
c  -------
c     E.Wolin, W&M, 23-jun-95

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: gmail.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/04/24 15:38:35 $')
      parameter (cautho= '$Author: holtrop $')
      parameter (crname= 'GMAIL')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: gmail.F,v 1.2 1997/04/24 15:38:35 holtrop Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcunit.inc"
c_end_inc

c  input/output variables:
c  -----------------------
      integer before,after

c  local variables:
c  ----------------
      integer i
c_end_var


c  executable code:
c  ----------------

c  blank lines before
      do i=1,before
        write(*,*)
      enddo

c  forward message to recmes
      call recmes(crname,'N',chmail)

c  blank lines after
      do i=1,after
         write(*,*)
      enddo

      return
      end

c---------------------------------------------------------------------------------


