      PROGRAM main
*
*     GEANT main program. To link with the MOTIF user interface
*     the routine GPAWPP(NWGEAN,NWPAW) should be called, whereas
*     the routine GPAW(NWGEAN,NWPAW) gives access to the basic
*     graphics version.
*
      implicit none
      character*128 rcsid
      data rcsid/'
     1$Id: gsim_int.F,v 1.8 2008/10/03 13:56:54 avakian Exp $
     1'/
      save

c  include files
c  -------------
#include "gcflag.inc"
c_end_inc

      INTEGER  NWGEAN, NWPAW
      REAL     GEANT, PAW
      PARAMETER (NWGEAN=5000000,NWPAW=1000000)
      COMMON/GCBANK/GEANT(NWGEAN)
      COMMON/PAWC/PAW(NWPAW)
      external uginit,uglast

      integer find_arg


c  executable code:
c  ----------------

c  generate help message
      if((find_arg('h').gt.0).or.(find_arg('help').gt.0))then
         call gsim_help
         stop
      endif

c  set BATCH flag
      BATCH=.FALSE.

c  turn off Arne's log file
      call set_log(.false.)


#ifdef DEBUG
      call clas_enable_fp_trap
#endif

#ifdef NO_MOTIF
      CALL GPAW(NWGEAN,NWPAW)
#else
      CALL GPAWPP(NWGEAN,NWPAW)
#endif

      END
      SUBROUTINE QNEXT
      END
      SUBROUTINE CZOPEN
      END
      SUBROUTINE CZTCP
      END
      SUBROUTINE CZCLOS
      END
      SUBROUTINE CZPUTA
      END

c-----------------------------------------------------------------------------

      subroutine never_gets_called
      
c  to force linking of following globals:

      call uginit
      call uglast
      call gutrev
      call gutrak
      call gukine
      call gustep
      call guskip
      call guout
      call gudigi
      call gufld
      call gtnino

      return
      end

c-----------------------------------------------------------------------------
