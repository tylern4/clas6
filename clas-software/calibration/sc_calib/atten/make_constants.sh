#! /bin/tcsh -f

awk '{print $2}' adc_const.parm > NMIP_ADC_LEFT.dat
awk '{print $3}' adc_const.parm > NMIP_ADCu_LEFT.dat 
awk '{print $4}' adc_const.parm > NMIP_ADC_RIGHT.dat
awk '{print $5}' adc_const.parm > NMIP_ADCu_RIGHT.dat
awk '{print $6}' adc_const.parm > atten_length.dat
awk '{print $7}' adc_const.parm > atten_u.dat
