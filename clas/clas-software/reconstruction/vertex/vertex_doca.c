/*
 *  Documentation for subroutine vertex_doca_
 *  Purpose:
 *  --------
 *      finds the distance of closest approach of two lines and changes the
 *	x,y,and z coordinates in each line to the closest point
 *  Input Parameters:  (Name - Type - Meaning)
 *       pline1	-	*line_t -   pointer to line 1
 *       pline2	-	*line_t -   pointer to line 2
 *  Output Parameters:  (Name - Type - Meaning)
 *       dp	-       *float	-   pointer to distance of closest approach
 *       same as above
 *  Author:   John McNabb      Created:  Fri Jul 25 16:35:48 EDT 1997
 *  Major revisions:
 *  ----------------
*/     
/*_begin_doc
 *  RCS ID string
 *  $Id: vertex_doca.c,v 1.1.1.1 1998/12/01 17:07:31 mcnabb Exp $*/
#define USE(var) static void * use_##var = (void *) &var
static char crcsid[] = 
   "$Id: vertex_doca.c,v 1.1.1.1 1998/12/01 17:07:31 mcnabb Exp $";
USE(crcsid);   /* make sure it is not optimized away */
/* END of RCS information*/
  
#include <vertex.h>
#include <math.h>
#include <kinematics.h>

void vertex_doca(line_t *pline1, line_t *pline2, float *dp, float *pR){
   float t1,t2; 		/*parametric values for line definition*/
   vector3_t diff;	/*difference between two lines*/
   float R;       	/*dot product of two directions*/
   line_t line1,line2;    /*line structures defined in vertex.h*/

   *dp=-1;                /*set the default distance between lines to -1*/
   *pR = 1;               /*default value: lines parallel*/
   line1 = *pline1;       /*set line1&2 to values pointed to by parameters*/ 
   line2 = *pline2;       /*    use as dummies for routine*/ 

   /*normalize directions if not already*/
   v3norm(line1.dir);
   v3norm(line2.dir);
   
   /*set differences between present "vertex" location*/
   diff = v3sub(line2.point , line1.point);
  
   /*calculate dot product of direction unit vectors*/
   R = v3dot(line1.dir,line2.dir);

   if(R==1){ /*parallel to beamline*/
           /*returns a negative distance so know that it was not calculated*/  
     return;
   }
 
   /*lines not parallel so continue*/
   /*calculate the parametric values of closest approach*/
   t1 = 1/(1-R*R) * v3dot(diff ,v3sub(line1.dir,v3mult(R,line2.dir)));
   t2 =-1/(1-R*R) * v3dot(diff ,v3sub(line2.dir,v3mult(R,line1.dir)));

   /*set the new "vertices" to the points of closest approach*/
   line1.point = v3add(line1.point,v3mult(t1,line1.dir)); 
   line2.point = v3add(line2.point,v3mult(t2,line2.dir)); 
 
   /*calculate the new difference vector*/
   diff = v3sub(line2.point,line1.point);
 
   /*return the "new vertices" inside the line structure passed*/
   pline1->point= line1.point;
   pline2->point= line2.point;

   /*return the distance of closest approach and 
                      the dot product of their directions*/
   *dp = v3mag(diff);
   *pR = R;
 
   return;
}








