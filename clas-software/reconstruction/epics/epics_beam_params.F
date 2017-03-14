       subroutine epics_beam_params

       implicit none
       save

#include "epics.inc"
#include "beam_param.inc"

       integer i_e

       do i_e=1,tot_epics
          if (epics_name(i_e)(:13) .eq. 'IPM2C21A.XPOS') then
             x_2c21 = epics_value(i_e)
          endif
          if (epics_name(i_e)(:13) .eq. 'IPM2C21A.YPOS') then
             y_2c21 = epics_value(i_e)
          endif
          if (epics_name(i_e)(:8) .eq. 'IPM2C21A') then
             i_2c21 = epics_value(i_e)
          endif
          if (epics_name(i_e)(:13) .eq. 'IPM2C24A.XPOS') then
             x_2c24 = epics_value(i_e)
          endif
          if (epics_name(i_e)(:13) .eq. 'IPM2C24A.YPOS') then
             y_2c24 = epics_value(i_e)
          endif
          if (epics_name(i_e)(:8) .eq. 'IPM2C24A') then
             i_2c24 = epics_value(i_e)
          endif
          if (epics_name(i_e)(:12) .eq. 'IPM2H01.XPOS') then
             x_2h01 = epics_value(i_e)
          endif
          if (epics_name(i_e)(:12) .eq. 'IPM2H01.YPOS') then
             y_2h01 = epics_value(i_e)
          endif
          if (epics_name(i_e)(:7) .eq. 'IPM2H01') then
             i_2h01 = epics_value(i_e)
          endif
          if (epics_name(i_e)(:13) .eq. 'MBSY1C_energy') then
             halla_energy = epics_value(i_e)
          endif
          if (epics_name(i_e)(:13) .eq. 'MBSY2C_energy') then
             hallb_energy = epics_value(i_e)
          endif
          if (epics_name(i_e)(:13) .eq. 'MBSY3C_energy') then
             hallc_energy = epics_value(i_e)
          endif
          if (epics_name(i_e)(:14) .eq. 'IGL1I00HALLADF') then
             halla_df = epics_value(i_e)
          endif
          if (epics_name(i_e)(:14) .eq. 'IGL1I00HALLBDF') then
             hallb_df = epics_value(i_e)
          endif
          if (epics_name(i_e)(:14) .eq. 'IGL1I00HALLCDF') then
             hallc_df = epics_value(i_e)
          endif
          if (epics_name(i_e)(:12) .eq. 'scaler_calc1') then
             fcup_current = epics_value(i_e)
          endif
       enddo

       return
       end

