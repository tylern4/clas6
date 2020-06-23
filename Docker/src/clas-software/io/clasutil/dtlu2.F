CDECK  ID>, DTLU2.
*CMZ :  2.00/00 10/02/91  09.24.57  by  Stephan Egli
*CMZ :  1.04/00 29/04/90  15.24.29  by  Stephan Egli
*-- Author :
      SUBROUTINE DTLU2(NDIM,NEN,TAB,AUX)
*
*     TLU2 - package for fast table look-up (look-up time does not
*            depend on the table length)
*
*     See in DTLU for full comment on fast table look-up
*
*
*     Subprograms: Subroutine DTLU2
*                  Functions  ITLU2 and JTLU2
*
*
*     Two arrays are used, TAB(2,NDIM) and AUX(2*NDIM), both assumed of
*     type integer. The array TAB(2,NDIM) is the table with a maximum
*     of NDIM entries. The array AUX is an auxiliary array of length
*     2*NDIM. NDIM has to be at least 10 (otherwise the program will
*     stop).
*     The auxilary array AUX is used internally and it has to be
*     initialized by a call of DTLU2 before any other operation.
*
*     CALL DTLU2(NDIM,NEN,TAB,AUX)
*
*               NDIM = dimension parameter of array TAB
*               NEN  = number of entries already stored in table
*               TAB  = table of entries [TAB(2,NDIM)]
*               AUX  = auxiliary array of at least 2*NDIM words
*
*     The auxiliary array AUX is initialized. IF NEN not equal to zero,
*     the array is prepared for these entries. DTU2 has to be called,
*     if the table TAB has been modified by the user.
*
*
*     J = JTLU(ITEM,TAB,AUX)
*
*               ITEM = item to be searched for in the table
*               TAB  = table of entries
*               AUX  = auxiliary array
*
*     The returned value is the index J with ITEM1 = TAB(1,J) and
*     ITEM2 = TAB(2,J). IF the
*     entry was not found, it is added to the table and the correspon-
*     ding index is returned. J = 0 is returned, if there is not enough
*     space in the table (if NDIM =NEN).
*
*     The content of AUX may be checked at any time.
*     AUX(1) contains NDIM, and AUX(3) contains NEN. Thus if
*     AUX(1)=AUX(3), then there is no more space to store an
*     additional entry.
*
*
*     J = ITLU2(ITEM1,ITEM2,TAB,AUX)
*
*               ITEM1,
*               ITEM2= item to be searched for in the table
*               TAB  = table of entries
*               AUX  = auxiliary array
*
*     The returned value is the index J with ITEM1 = TAB(1,J) and
*     ITEM2 = TAB(2,j), if the
*     entry was found, and J = 0, if no such entry is found in the
*     table.
*
*     Instead of calling this function, the user may insert the
*     following six  statements into his program:
*           KTLU=AUX(1)+MOD(IABS(ITEM1+ITEM2),AUX(2))+1
*      2222 KTLU=AUX(KTLU+5)
*           IF(KTLU.NE.0) THEN
*              IF(ITEM1.NE.TAB(1,KTLU)) GOTO 2222
*              IF(ITEM2.NE.TAB(2,KTLU)) GOTO 2222
*           END IF
*     and
*           J=KTLU
*
*     The result is identical, and the time is reduced to about
*     5 micro seconds. If the item should be added to the list, if it
*     is not already in, the user may add the statements:
*           ELSE
*              KTLU=JTLU2(ITEM1,ITEM2,TAB,AUX)
*     before the END IF statement
*
*
*     Author: V. Blobel Jan 88
*
      INTEGER TAB(2,NDIM),AUX(2*NDIM)
*     NDIM at least 10,  otherwise stop
      IF(NDIM.LT.10) STOP
*     Clear aux array ...
      DO 10 I=1,2*NDIM
   10 AUX(I)=0
*     ... and define first three words
      AUX(1)=NDIM
      AUX(2)=0
C     AUX(3)=NEN
*     determine prime number for hash function
      NPRIM=NDIM-3
      IF(MOD(NPRIM,2).EQ.0) NPRIM=NPRIM-1
   20 NPRIM=NPRIM-2
      IF(NPRIM.GT.5) THEN
         DO 30 I=3,INT(SQRT(FLOAT(NPRIM))),2
         J=NPRIM/I
         IF(I*J.EQ.NPRIM) GOTO 20
   30    CONTINUE
      END IF
      AUX(2)=NPRIM
*     Loop to insert index structure in AUX for the NEN existing
*     entries in the table
      DO 50 K=1,MIN(NEN,NDIM)
      ITEM1=TAB(1,K)
      ITEM2=TAB(2,K)
*     search and add (like function JTLU)
      J=AUX(1)+MOD(IABS(ITEM1+ITEM2),AUX(2))+1
   40 I=J
      J=AUX(I+5)
      IF(J.NE.0) THEN
         IF(ITEM1.NE.TAB(1,J)) GOTO 40
         IF(ITEM2.NE.TAB(2,J)) GOTO 40
         AUX(3)=AUX(3)+1
      ELSE
         J=AUX(3)+1
         AUX(3)=J
         AUX(I+5)=J
      END IF
   50 CONTINUE
  100 RETURN
      END
