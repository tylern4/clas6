C======================================================================
      SUBROUTINE sda_flags(ipat,ipat_flg,nrec,nelem,nly)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the corresponding flag to 1 when
C-                         templates can be represented by the first
C-                         one that has "nly" unique fields
C-                         e.g. If nly = 3
C-
C-                         template#        ipat         ipat_flag
C-                             1      -1 -1 0 -1 0 -1        0
C-                             2      -1 -1 0 -1 0  0        1
C-
C-                         It is called by SDA_INIT to setup once
C-                         whenever there is a superlayer w/ >6 layers.
C-
C-   Inputs  : ipat      - an array of templates (records)
C-             nrec      - number of records in the template array
C-             nelem     - number of fields there are in each record
C-             nly       - number of unique fields
C-
C-   Outputs : ipat_flg  - the corresponding flags to all the records
C-   Controls:
C-
C-   Library belongs: libsda.a
C-
C-   Calls: none
C-
C-   Created  9-AUG-1992  Billy Leung
C-
C-   Called by sda_last
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      SAVE
C
C Subroutine parameters
      INTEGER nelem, nrec, nly
      INTEGER ipat(nelem,nrec)
      INTEGER ipat_flg(nrec)
C
C Local variables
      INTEGER i, j, k, isum
C
C
C Assuming "ipat" is sorted coming in
      IF (nrec .LT. 2) GO TO 999     ! exit when there is only 1 record
C
      DO i = 1,nrec                  ! initially all flags are marked 1
        ipat_flg(i) = 1
      ENDDO
      ipat_flg(1) = 0
C
C The first pattern (record) is unique, check subsequent patterns
      DO i = 2,nrec
        DO j = 1,nrec
          isum = 0
          DO k = 1,nly
            IF (ipat(k,i) .EQ. ipat(k,j)) THEN
              isum = isum + 1
            ENDIF
          ENDDO
          IF (isum .EQ. nly) THEN         ! found duplicate
            IF (ipat_flg(j) .EQ. 1) THEN  ! if 1st found was unmarked
              ipat_flg(i) = 0
            ENDIF
            GO TO 13                      ! goto check the next pattern
          ENDIF
        ENDDO                         ! End of DO j
        ipat_flg(i) = 0               ! found no duplicate
   13 ENDDO                           ! End of DO i
C
  999 CONTINUE
      RETURN
      END
