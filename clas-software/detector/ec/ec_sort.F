      SUBROUTINE EcGus_int_sort(N,list,idx)
*----------------------------------------------------------------------
*-
*-   Purpose and Methods : Sort list by increasing value
*-
*-   Inputs  : N - number of input quantities
*-           : list(*)   - input lists in numerical order (not reorderd)
*-   Outputs : idx(*)    - ordered index pointers
*-
*-   Created  17-Jul-1993 K.B.Beard
*-   Modified  5-Jul-1995 Zhujun Li
*----------------------------------------------------------------------
      IMPLICIT NONE
      integer N,list(*),idx(*)
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec_sort.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/05/01 19:49:15 $')
      PARAMETER (CAUTHO = '$Author: freyberg $')
      DATA CRCSID/ 
     1'$Id: ec_sort.F,v 1.2 1997/05/01 19:49:15 freyberg Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ec_sort')                                   
      PARAMETER (CRAUTH='Zhujun Li')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
      integer m,m1,temp_ID
      logical ordered_OK,swap
*----------------------------------------------------------------------
*
      IF(N.LE.0) THEN    !ignore if no elements
         RETURN
      ELSEIF(N.EQ.1) THEN        !simple
         idx(1)= 1
         RETURN
      ENDIF
*
      DO m= 1,N
         idx(m)= m
      ENDDO
*
*- use a bubble sort to order by increasing list
      ordered_OK= .FALSE.
      DO WHILE (.not.ordered_OK)
         ordered_OK= .TRUE.
         Do m=1,N-1                     !1st element
           m1= m+1                      !next element
           swap= list(idx(m)).GT.list(idx(m1))
           if(swap) then                !swap pair
             temp_ID= idx(m)
             idx(m)= idx(m1)
             idx(m1)= temp_ID
             ordered_OK= .FALSE.        !swapped at least one pair
           endif
         EndDo
      ENDDO
*
      RETURN
      END
**********************************************************************
      SUBROUTINE EcGus_sort(N,size,ID)
*----------------------------------------------------------------------
*-
*-   Purpose and Methods : Sort numerically ordered input by decreasing size
*-
*-   Inputs  : N - number of input quantities
*-           : size(*)   - input sizes in numerical order (not reorderd)
*-           : threshold- minimum size to consider
*-   Outputs : N - number of input quantities (above threshold)
*-           : ID(*)     - ordered id numbers (above threshold)
*-
*-   Created  8-Apr-1992   Kevin B. Beard
*-
*----------------------------------------------------------------------
      IMPLICIT NONE
      integer N,ID(*)
      real size(*)
      integer m,m1,temp_ID
      logical ordered_OK,swap
*----------------------------------------------------------------------
*
      IF(N.LE.0) THEN    !ignore if no elements
         N= 0
         RETURN
      ENDIF
*
      do m=1,N
        id(m)=m
      enddo
*- use a bubble sort to order by decreasing size
      ordered_OK= .FALSE.
      DO WHILE (.not.ordered_OK)
         ordered_OK= .TRUE.
         Do m=1,N-1                     !1st element
           m1= m+1                      !next element
           swap= size(ID(m)).LT.size(ID(m1))
           if(swap) then                !swap pair
             temp_ID= ID(m)
             ID(m)= ID(m1)
             ID(m1)= temp_ID
             ordered_OK= .FALSE.        !swapped at least one pair
           endif
         EndDo
      ENDDO
*
      RETURN
      END
************************************************************************
