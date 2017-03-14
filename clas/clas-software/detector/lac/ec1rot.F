      SUBROUTINE EC1ROT(XD,XM,N)
      implicit none
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Computes coordinates CLAS  Reference System  coordinates *
C.    *       knowing the coordinates in LAC (Large Angle)             *
C.    *       N is the module number                                   *
C.    ******************************************************************
C.
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec1rot.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:00 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/ 
     1'$Id: ec1rot.F,v 1.1.1.1 1997/03/28 20:58:00 marki Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ec1rot')                                   
      PARAMETER (CRAUTH='avakian')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs

#include "ec1rt.inc"

      REAL XD(3),XM(3)
      INTEGER N
      INTEGER i
C.
C.    ------------------------------------------------------------------
C.
            XM(1) = EC1TRAN(1,N) + EC1RMAT(1,N)*XD(1) + EC1RMAT(4,N)*XD(2) +
     +      EC1RMAT(7 ,N)*XD(3)
            XM(2) = EC1TRAN(2,N) + EC1RMAT(2,N)*XD(1) + EC1RMAT(5,N)*XD(2) +
     +      EC1RMAT(8 ,N)*XD(3)
            XM(3) = EC1TRAN(3,N) + EC1RMAT(3,N)*XD(1) + EC1RMAT(6,N)*XD(2) +
     +      EC1RMAT(9 ,N)*XD(3)
      END
