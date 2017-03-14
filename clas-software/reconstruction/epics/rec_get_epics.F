       subroutine rec_get_epics

       implicit none
       save

#include "bcs.inc"
#include "clasmdl.inc"
#include "epics.inc"

       integer mamind, jbit
       character*4 chaint
       external mamind, jbit, chaint

       integer nami_epics, ind, nrow, ichar, irun, ievt, irec
       character*4  epics_chars(8)
       character*32 temp_name
       character*256 crmess
       character*20 crname
       parameter (crname = 'rec_get_epics')
c-------------------------------------------------

       nami_epics = mamind(IW,'EPIC')
       if (iw(nami_epics) .le. 0) then
          call recmes(crname,'d',' no EPIC bank in EPIC record')
       else 
          call rernev(irun,ievt,irec)
          epics_last_evt_num = prev_event
          epics_last_run_num = irun
          ind = iw(nami_epics)
          nrow = iw(ind-4)
          IF(nrow.GT.N_EPICS) nrow=N_EPICS
          do 1000 tot_epics=1, nrow
             epics_value(tot_epics) = rw(ind+1+(tot_epics-1)*9)
             do ichar=1,8
                epics_chars(ichar) = chaint(iw(ind+1+ichar+(tot_epics-1)*9))
                temp_name(1+(ichar-1)*4:ichar*4) = epics_chars(ichar)
             enddo
             epics_name(tot_epics) = temp_name
 1000     continue
       endif

       return
       end
