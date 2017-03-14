cc      include file DD
c
c  Purpose:  holds the DD ring control variables.
c
c  Author:   Gilfoyle 
c  -------
c
c----6----------------------------------------------------------------72
c  
c  Variables contained in DD common block
c  --------------------------------------------
c
c ELENGTH - See DD documentation.
c event - See DD documentation.
c length_dd - size of event read from DD ring.
c mode_dd - Get events on request (=2)
c prescale_dd - Get a subset of the events in the DD ring.
c wait_dd - Get events asynchronously (=1).
c suser_dd - Only one process allowed to attach to FIFO (=0).
c fnlen_dd - length of DD FIFO name.
c fnum_dd - Needed for DD??.
c fname_dd - FIFO name in integer format.
c DD_ok - integer for status after attaching to DD ring.
c DD_NAME - Name of DD system.
c FIFONAME - Name of DD FIFO that you attached to.
c cw1_dd - DD control word (see DD documentation).
c cw2_dd - DD control word (see DD documentation).
c cb1_dd - DD control word (see DD documentation).
c cb2_dd - DD control word (see DD documentation).
c application - project/application name used by ipc system
c               for the online version.
c
      integer ELENGTH
      PARAMETER( ELENGTH = 40500 )
      integer cw1_dd, cb1_dd, cw2_dd, cb2_dd, length_dd
      integer mode_dd, prescale_dd, wait_dd, suser_dd
      integer event(ELENGTH)
      integer fnlen_dd, fnum_dd, fname_dd(4), DD_ok

      character*20 DD_id, DD_NAME, FIFONAME, application

      common /DDEVNT/ mode_dd, prescale_dd, wait_dd, suser_dd,
     1               cw1_dd, cb1_dd, cw2_dd, cb2_dd, 
     2               fname_dd, fnlen_dd, fnum_dd, length_dd, 
     3               event, DD_ok, DD_id, DD_NAME, FIFONAME,
     4               application
