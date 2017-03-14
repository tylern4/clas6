#if defined(__linux__) && defined(__i386__)
/******************************************************************************
* Some systems need this hack to perform the most basic checks for floating   *
* point operations (sigh).                                                    *
* Notice that linux on a pc still does not detect invalid floating point data *
* when accessing the data. Worse, this trap only works if all operands are    *
* valid. Consequently linux on a pc will still continue without trap if a     *
* program fetches invalid data and then performs some floating point          *
* operation. But, at least we fetch overflows, /0 and acos(500).              *
* HGJ, 2002-08-13                                                             *
* setting fpucw=0x0360 gives seg.abort for operations on ~10^-21<|x|<~10^22   *
******************************************************************************/

#include <fpu_control.h>

void clas_enable_fp_trap(void)
 { fpu_control_t fpucw;
    _FPU_GETCW(fpucw);
    /*****************************************
    * linux documentation                    *
    *        cf. /usr/include/fpu_control.h  *
    * _FPU_MASK_IM  invalid operand          *
    * _FPU_MASK_DM  denormalized operand     *
    * _FPU_MASK_ZM  divide by zero           *
    * _FPU_MASK_OM  overflow                 *
    * _FPU_MASK_UM  underflow                *
    ******************************************/
    
    /* note: _FPU_MASK_PM  inexact result mask has to be ignored,  *
     *                     otherwise: 0.50001**2 gives seg.abort   */

    fpucw&=~(_FPU_MASK_IM|_FPU_MASK_DM|_FPU_MASK_ZM|_FPU_MASK_OM|_FPU_MASK_UM);
    _FPU_SETCW(fpucw);
 }
#else
void clas_enable_fp_trap(void)
  {
  }
#endif

#if !defined(__VMS__) && !defined(__WIN32__)
/***********************************************************************
* Fortran/C clutch for systems without multi language calling standard *
***********************************************************************/

void clas_enable_fp_trap_(void)
  { clas_enable_fp_trap();
  }
#endif
