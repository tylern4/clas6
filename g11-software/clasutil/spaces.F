c
c extracted from CERN library to fix a bug - Sergey B. 9/20/01
c
      CHARACTER*(*) FUNCTION SPACES(STR,NSP)
C
C CERN PROGLIB# M433    SPACES          .VERSION KERNFOR  4.14  860211
C ORIG.  6/05/86 M.GOOSSENS/DD
C
C-    The function value SPACES returns the character string STR with
C-    leading blanks removed and each occurence of one or more blanks
C-    replaced by NSPACE blanks inside the string STR
C
      CHARACTER*(*) STR
C
      LENSPA = LEN(SPACES)
      SPACES = ' '
c do not modify input parameter, it can be a constant !!!
c just copy it to another variable - S.B.
      NSPACE = NSP
      IF (NSPACE.LT.0) NSPACE = 0
c
      IBLANK = 1
      ISPACE = 1
  100 INONBL = INDEXC(STR(IBLANK:),' ')
      IF (INONBL.EQ.0) THEN
          SPACES(ISPACE:) = STR(IBLANK:)
                                                    GO TO 999
      ENDIF
      INONBL = INONBL + IBLANK - 1
      IBLANK = INDEX(STR(INONBL:),' ')
      IF (IBLANK.EQ.0) THEN
          SPACES(ISPACE:) = STR(INONBL:)
                                                    GO TO 999
      ENDIF
      IBLANK = IBLANK + INONBL - 1
      SPACES(ISPACE:) = STR(INONBL:IBLANK-1)
      ISPACE = ISPACE + IBLANK - INONBL + NSPACE
      IF (ISPACE.LE.LENSPA)                         GO TO 100
  999 CONTINUE
      END

c
