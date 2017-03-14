C
C Geometry of detector planes used in SDA and results of swimming
C                                 Bogdan Niczyporuk, October 1993
C
C         
C  Detector Planes
C================= only the first 8 entries are used in the tracking=======
C sda_pln(1,ip,isec) |
C sda_pln(2,ip,isec) |--> Center curvature for DC&CC or {0,0,0} for SC&EC
C sda_pln(3,ip,isec) |
C
C sda_pln(4,ip,isec) |
C sda_pln(5,ip,isec) |--> Unit Vector perpendicular to the plane "ip"
C sda_pln(6,ip,isec) |    or (0,0,0) if plane is a cylinder
C
C sda_pln( 7,ip,isec) --> Radius of arc (DC and CC)
C sda_pln( 8,ip,isec) --> Distance to the plane (Reg.1 DC, SC and EC) 
C
C                 Results of swimming
C sda_trp(1,ip) |
C sda_trp(2,ip) |--> Track point {x,y,z} on a plane 
C sda_trp(3,ip) |
C
C sda_trp(4,ip) |
C sda_trp(5,ip) |--> Track direction at track point on a plane
C sda_trp(6,ip) |
C                 
C sda_trp(7,ip)  --> Track length from the target to a layer [cm]
C sda_trp(8:10,ip)  B field at intersection point with plane
C
C sda_tmat(5,5,ip)-> Transport matrix at track point on a plane
C
C st_Zpos(1,isec) Z-coord. of upstream edge of Start-Counter 1
C st_Zpos(2,isec) Z-coord. of downstream edge of Start-Counter 1
C st_Zpos(3,isec) Z-coord. of upstream edge of Start-Counter 2
C st_Zpos(4,isec) Z-coord. of downstream edge of Start-Counter 2
C

      INTEGER npln, nst_min, nst_max, 
     1              ndc_min, ncc_min, nsc_min, nec_min,
     2              ndc_max, ncc_max, nsc_max, nec_max,
     3              npl_st, npl_dc ,  npl_cc,  npl_sc,  npl_ec
      PARAMETER (npln =46)
      PARAMETER (npl_st =2, npl_dc =36, npl_cc =1, npl_sc =4, npl_ec =2)
      PARAMETER ( nst_min =2        , nst_max =nst_min+npl_st-1 )
      PARAMETER ( ndc_min =nst_max+1, ndc_max =ndc_min+npl_dc-1 )
      PARAMETER ( ncc_min =ndc_max+1, ncc_max =ncc_min+npl_cc-1 )
      PARAMETER ( nsc_min =ncc_max+1, nsc_max =nsc_min+npl_sc-1 )
      PARAMETER ( nec_min =nsc_max+1, nec_max =nec_min+npl_ec-1 )

      REAL sda_pln, st_Zpos, sda_trp, sda_tmat

      COMMON/sdaswim/ sda_pln(20,npln,6), st_Zpos(4,6),
     &               sda_trp(10,npln), sda_tmat(5,5,npln)

      save /sdaswim/


