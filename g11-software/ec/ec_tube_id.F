      SUBROUTINE ec_tube_id(tube_index,strip,layer,sector)
*----------------------------------------------------------------------
*-
*-   Purpose and Methods : Translates strip,layer,sector to tube_index and 
*-                         performs the inverse transformation. The purpose 
*-                         of doing this is to have a 1-dimensional array 
*-                         representation of all forward calorimeter pmt's.
*-
*-   If tube_index is < 1, calculates tube_index from strip, layer, sector
*-   If tube_index is > 0, calculates the strip, layer, sector from tube_index
*- 
*-   Note! if one neglects to initialize tube_index to zero before calling
*-   this subroutine, it will *corrupt* the values of strip, layer, and sector!
*-
*-   Created:  August 1998, W. Brooks
*-
*----------------------------------------------------------------------

      implicit none
      integer tube_index,strip,layer,sector

      if( tube_index.lt.1 )then      ! Calculate tube_index from strip, layer, sector
         
         if(sector.lt.1.or.sector.gt.6)then
            print*,'Bad sector number in ec_tube_id!'
         endif
         if(layer.lt.1.or.layer.gt.6)then
            print*,'Bad layer number in ec_tube_id!'
         endif
         if(strip.lt.1.or.strip.gt.36)then
            print*,'Bad strip number in ec_tube_id!'
         endif

         tube_index = (sector-1)*216 + (layer-1)*36 + strip

      else                      ! Calculate the strip, layer, sector from tube_index

         sector = tube_index/216 + 1
         layer = (tube_index-(sector-1)*216)/36 + 1
         strip = tube_index-(sector-1)*216-(layer-1)*36

      endif
      
      return
      end


