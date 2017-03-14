      subroutine recmes_wrap(routine, level, message)
      implicit none
      save
      character*64 routine
      character*1 level
      character*256 message
      call recmes(routine, level, message)
      return
      end
