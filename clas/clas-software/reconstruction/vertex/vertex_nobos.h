/* vertex_nobos.h
    define a set of structures used in finding detached vertices
    No BOS routines, only some of the data structures.
*/

#include <ntypes.h>

typedef struct {
  vector3_t point;
  vector3_t dir;
}  line_t;

extern int mtv_nobos(tber_t TBtracks[], int ntrk, vector3_t *vert, float Cmat[9], float *chi2, int *inter);




