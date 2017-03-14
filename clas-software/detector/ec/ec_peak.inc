***************** Include file: ec_peak.inc **********************************
*
*   Path length for given hit & peak
*   Peak energy, time, width, position on the edge and second and third 
*   moments after attenuation corection for given hit
*
      real ec_peak(energy:dist,Maxhits,Ec_U:Ec_W,
     &whole:cover,Maxsectors)
*
      real time_we(Maxhits,Ec_U:Ec_W,whole:cover,Maxsectors)
      real peak_path(Maxhits,Ec_U:Ec_W)
      integer peak_Nhit(Maxpeaks,Ec_U:Ec_W)
      integer peak_hit(Maxhits,Maxpeaks,Ec_U:Ec_W)
      integer MAXgroups
      parameter (MAXgroups= MAXstrips/2+1)
      real group_E(MAXgroups),strip_efr(maxgroups,MAXstrips)
      integer Ngroup,groupN(MAXgroups), strip_stat(MAXstrips)
      integer groupID( MAXstrips,MAXgroups)
*
      common /ec_peak/ Ngroup,groupN,groupID,strip_stat,
     &ec_peak, peak_path, time_we, peak_Nhit, peak_hit,strip_efr,group_E
