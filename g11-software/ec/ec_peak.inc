***************** Include file: ec_peak.inc **********************************
*
*   Path length for given hit & peak
*   Peak energy, time, width, position on the edge and second and third 
*   moments after attenuation corection for given hit
*
      real ec_peak(energy:dist,Maxhits,Ec_U:Ec_W,
     &whole:outer,Maxsectors)
*
      real time_we(Maxhits,Ec_U:Ec_W,whole:outer,Maxsectors)
      real peak_path(Maxhits,Ec_U:Ec_W)
      integer peak_Nhit(Maxpeaks,Ec_U:Ec_W)
      integer peak_hit(Maxhits,Maxpeaks,Ec_U:Ec_W)
*
      common /ec_peak/ ec_peak, peak_path, time_we, peak_Nhit, peak_hit
