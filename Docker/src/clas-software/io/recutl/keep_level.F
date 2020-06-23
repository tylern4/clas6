      SUBROUTINE KEEP_LEVEL(LALL,ITOGLE)
c
c  Documentation for subroutine KEEP_LEVEL
c
c  Purpose: Stores the messaging level for loggin
c  --------
c
c  Input Parameters:  LALL(5) - L - logicals to be stored (itogle  < 0) 
c  ----------------   ITOGLE  - I - lessthan 0 mess store, greater than 0
c                                   means return
c
c  Output Parameters:  LALL(5) - L - logicals to be stored (itogle > 0)
c  -----------------
c
c  Author:   Arne Freyberger      Created:  Thu May  4 16:33:28 EDT 1995
c  -------
c
      IMPLICIT NONE
      SAVE
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(132) CRCSID
      DATA CRCSID/   
     1'$Id: keep_level.F,v 1.5 1999/02/25 22:28:22 freyberg Exp $'   
     2/   
c
c  Local User defined variables:
c  -----------------------------
      LOGICAL LALL(5), LALL_KEEP(5)
      INTEGER ITOGLE, I
      integer itcl_int, f_init
c     According to Nirvana, the following are LINFO, LWARN, LDEBUG, LERROR,
c     and  LFATAL
      DATA LALL_KEEP/.TRUE., .TRUE., .TRUE., .TRUE., .TRUE./

      DATA itcl_int,f_init/1,0/
c
c  executable code for routine KEEP_LEVEL:
c
c      if (F_INIT .eq. 0) then 
c         call tcl_linkvar('linfo',lall_keep(1),itcl_int)
c         call tcl_linkvar('lwarn',lall_keep(2),itcl_int)
c         call tcl_linkvar('ldebug',lall_keep(3),itcl_int)
c         call tcl_linkvar('lerror',lall_keep(4),itcl_int)
c         call tcl_linkvar('lfatal',lall_keep(5),itcl_int)
c         f_init = 10
c      endif

      IF (ITOGLE .LT. 0) THEN
       DO 10 I=1,5
         LALL_KEEP(I) = LALL(I)
10     CONTINUE
      ELSE
       DO 20 I=1,5
        LALL(I) = LALL_KEEP(I)
20     CONTINUE
      ENDIF
      RETURN
      END
