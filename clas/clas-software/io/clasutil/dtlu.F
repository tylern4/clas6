CDECK  ID>, DTLU.
*CMZ :  2.00/00 10/02/91  09.24.56  by  Stephan Egli
*CMZ :  1.04/00 29/04/90  15.24.29  by  Stephan Egli
*-- Author :
      SUBROUTINE DTLU(NDIM,NEN,TAB,AUX)
*
*     TLU - package for fast table look-up (look-up time does not
*           depend on the table length)
*
*     Assume a table TAB(NDIM) (of type integer), which contains
*     NEN items in TAB(1)...TAB(NEN). The table look-up to check
*     wether the word ITEM is in the table, can be done with a
*     Do-loop like:
*
*           DO 10 I=1,NEN
*           IF(TAB(I).EQ.ITEM) GOTO 20
*        10 CONTINUE
*           I=0
*        20 CONTINUE
*     *     I=0 returned, if ITEM not yet table TAB
*
*     The time required for one table look-up is apparently
*     proportinal to NEN. On the DESY IBM the time for one table look-up
*     in case of random numbers is approximately
*           0.16 * NEN mikro seconds (if the item is found) and twice
*     that number, if the item is not found. There will be an extra
*     time of about 4 mikro seconds, if the loop is done in a function
*     (due to the call/return overhead).
*     For example if there is a table with NEN=1000 entries, the time
*     for one table look-up is about 160 or 320 micro seconds, depending
*     on wether the entry is found (on average after NEN/2 entries) or
*     not found.
*     Since in many applications the number of times a table look-up is
*     done is approximately proportional to the number of entries NEN,
*     the total time spent for table look-up will be proportional to
*     the square of NEN.
*
*     There are of course faster methods. For the binary search the
*     time for one table look-up is approximately proportional to
*     the logarithm of NEN, the number of entries. The disadvantage
*     with this algorithm is, that the table entries have to be ordered.
*     Thus some sorting is necessary for each update of the table.
*
*     The subprograms of this package use the 'hash'-technique.
*     Advantages of this method are:
*     (1) time for one table look-up is constant (no dependence on the
*         length of the table) and about 7 micro seconds for a
*         function including the call/return time
*     (2) the entries of the table can be in any order (no sorting
*         necessary)
*     (3) the code for a table look-up is short (5 statements) and
*         can be included inline in the user program, thus avoiding
*         the call/return time and reducing the time for one table
*         look-up to 3 micro seconds (for any number of entries)
*     (4) the time for one table look-up is the same, wether the entry
*         is found or not.
*
*     There is one disadvantage: an auxiliary array of length 2*NDIM
*     is necessary for a table of length NDIM (maximun of NDIM entries).
*
*     Subprograms: Subroutine DTLU
*                  Functions  ITLU and JTLU
*
*
*     Two arrays are used, TAB(NDIM) and AUX(2*NDIM), both assumed of
*     type integer. The array TAB(NDIM) is the table with a maximum
*     of NDIM entries. The array AUX is an auxiliary array of length
*     2*NDIM. NDIM has to be at least 10 (otherwise the program will
*     stop).
*     The auxilary array AUX is used internally and it has to be
*     initialized by a call of DTLU before any other operation.
*
*     CALL DTLU(NDIM,NEN,TAB,AUX)
*
*               NDIM = dimension parameter of array TAB
*               NEN  = number of entries already stored in table
*               TAB  = table of entries [TAB(NDIM)]
*               AUX  = auxiliary array of at least 2*NDIM words
*
*     The auxiliary array AUX is initialized. IF NEN not equal to zero,
*     the array is prepared for these entries. DTU has to be called,
*     if the table TAB has been modified by the user. The time for
*     initialization is about 3 mikro seconds for each entry in the
*     table [total (10 + 3 * NEN) micro seconds].
*
*
*     J = JTLU(ITEM,TAB,AUX)
*
*               ITEM = item to be searched for in the table
*               TAB  = table of entries
*               AUX  = auxiliary array
*
*     The returned value is the index J with ITEM = TAB(J). If the
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
*     J = ITLU(ITEM,TAB,AUX)
*
*               ITEM = item to be searched for in the table
*               TAB  = table of entries
*               AUX  = auxiliary array
*
*     The returned value is the index J with ITEM = TAB(J), if the
*     entry was found, and J = 0, if no entry ITEM is found in the
*     table.
*
*     Instead of calling this function, the user may insert the
*     following five statements into his program:
*
*           KTLU=AUX(1)+MOD(IABS(ITEM),AUX(2))+1
*      1111 KTLU=AUX(KTLU+5)
*           IF(KTLU.NE.0) THEN
*              IF(ITEM.NE.TAB(KTLU)) GOTO 1111
*           END IF
*     and
*           J=KTLU
*
*     The result is identical, and the time is reduced to about
*     3 micro seconds. If the item should be added to the list, if it
*     is not already in, the user may add the statements:
*           ELSE
*              KTLU=JTLU(ITEM,TAB,AUX)
*     before the END IF statement
*
*     There is another package for the case of a table with two words
*     per entry [TAB(2,NDIM)], called DTLU2.
*
*
*     Author: V. Blobel Jan 88
*
      INTEGER TAB(NDIM),AUX(2*NDIM)
*     NDIM at least 10, otherwise stop
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
      ITEM=TAB(K)
*     search and add (like function JTLU)
      J=AUX(1)+MOD(IABS(ITEM),AUX(2))+1
   40 I=J
      J=AUX(I+5)
      IF(J.NE.0) THEN
         IF(ITEM.NE.TAB(J)) GOTO 40
         AUX(3)=AUX(3)+1
      ELSE
         J=AUX(3)+1
         AUX(3)=J
         AUX(I+5)=J
      END IF
   50 CONTINUE
  100 RETURN
      END
