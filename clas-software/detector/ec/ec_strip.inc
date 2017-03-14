***************** Include file: ec_strip.inc **********************************
*
*   Strip energy and time after attenuation corection for given hit
*
      integer inner_strips(Maxstrips,Ec_U:Ec_W,Maxsectors)      
      integer outer_strips(Maxstrips,Ec_U:Ec_W,Maxsectors)      
      real Ec_stripE(Maxhits,Maxstrips,Ec_U:Ec_W,
     &whole:cover,Maxsectors)      
      real Ec_strip_E(Maxhits,Maxstrips,Ec_U:Ec_W,
     &whole:cover,Maxsectors)      
      real Ec_stripT(Maxhits,Maxstrips,Ec_U:Ec_W,
     &whole:cover,Maxsectors)
      real Ec_strip_T(Maxhits,Maxstrips,Ec_U:Ec_W,
     &whole:cover,Maxsectors)
C- stripc is 1 if strip was in that hit and -1 otherwise
      real stripc(Maxstrips,0:Maxhits,Ec_U:Ec_W,whole:cover,Maxsectors)
C- ppath is the path length from hit position to the edge
      real ppath(0:Maxhits,Ec_U:Ec_W,whole:cover,Maxsectors), esum(whole:cover)
C- nstripc is the number of strips in the peak on the edge at given hit
      integer nstripc(0:Maxhits,Ec_U:Ec_W,whole:cover,Maxsectors)
C- ntstripc is the total number of strips in the given hit
      integer ntstripc(0:Maxhits,whole:cover,Maxsectors)
      integer maxwstrips
*
      common /ec_strip/Ec_stripE,Ec_stripT,stripc,Ec_strip_E,Ec_strip_T, esum,
     +ppath,ntstripc,nstripc,inner_strips,outer_strips,maxwstrips
