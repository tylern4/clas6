c  clmate.inc
c
c  fklein (2/01): keep material ids' in an array
c     so that the array index can be used in FFREAD cards
c
c  $Id: clmate.inc,v 1.10 2009/12/22 14:57:34 natasha Exp $

      character*132 ciclmate
      data ciclmate/
     1'$Id: clmate.inc,v 1.10 2009/12/22 14:57:34 natasha Exp $'
     1/

      INTEGER imate_liq_h2
      INTEGER imate_liq_d2
      INTEGER imate_liq_he3
      INTEGER imate_liq_he4
      INTEGER imate_liq_n2
      INTEGER imate_liq_neon
      INTEGER imate_helium
      INTEGER imate_helium6
      INTEGER imate_air
      INTEGER imate_vacuum
      INTEGER imate_lithium
      INTEGER imate_beryllium
      INTEGER imate_carbon
      INTEGER imate_aluminum
      INTEGER imate_iron
      INTEGER imate_copper
      INTEGER imate_tungsten
      INTEGER imate_lead
      INTEGER imate_uranium
      INTEGER imate_titanium
      INTEGER imate_rigidgraphite
      INTEGER imate_flexgraphite
      INTEGER imate_stainless_steel
      INTEGER imate_ch2
      INTEGER imate_tio2
      INTEGER imate_polystyrene
      INTEGER imate_polyethylene
      INTEGER imate_cerex
      INTEGER imate_mylar
      INTEGER imate_kapton
      INTEGER imate_alumylar       !aluminized mylar (9:1)
      INTEGER imate_alumaseal      !mylar/aluminum/mylar/glue compound
      INTEGER imate_comp_alumaseal !kapton/alumaseal/epoxy compound
      INTEGER imate_cerexalu1      !cerex/alu compound (27:35)
      INTEGER imate_cerexalu2      !cerex/alu compound (15:14)
      INTEGER imate_cerexmyl1      !cerex/alum.mylar compound (25:22)
      INTEGER imate_lowdfoam       !low density foam for scattering chamber
      INTEGER imate_carb_fiber     !carbon fiber/epoxy
      INTEGER imate_butanol
      INTEGER imate_nh3
      INTEGER imate_nd3

      COMMON /CLMATE/    imate_liq_h2,    imate_liq_d2,    imate_liq_he3,
     1  imate_liq_he4,   imate_liq_n2,    imate_liq_neon,  imate_helium,
     2  imate_air,       imate_vacuum,    imate_lithium,   imate_beryllium, 
     4  imate_carbon,    imate_aluminum,  imate_iron,      imate_copper, 
     5  imate_tungsten,  imate_lead,      imate_uranium,   imate_stainless_steel,
     6  imate_ch2,      imate_polystyrene,imate_polyethylene, imate_cerex,
     8  imate_mylar,     imate_kapton,    imate_alumylar,     imate_alumaseal,
     9  imate_comp_alumaseal,imate_cerexalu1,imate_cerexalu2, imate_cerexmyl1,
     &  imate_lowdfoam, imate_carb_fiber, imate_butanol, imate_nh3, imate_nd3,
     &  imate_titanium, imate_rigidgraphite, imate_flexgraphite, imate_tio2,
     &  imate_helium6





