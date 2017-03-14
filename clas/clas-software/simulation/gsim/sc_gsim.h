/* 
 * This file sets up some defines for the SC part of GSIM.
 */

#define MAX_PANEL 4   /* Number of planes */
#define NUM_SCIN 57   /* Total number of strips. */

#ifndef SQ
#define SQ(x) ((x)*(x))
#endif

/* We "override" the standard SCG structure, since we prefer to use VECTORS ! */

/* ------------------------ scgg -----------------*/
typedef struct {
  int id;    /*   paddle id# */
  int panel;    /*  panel number - to which plane (panel) the paddle belongs */
  vector3_t ccw;  /* Counter clockwise end. l */
  vector3_t cw;   /* Clockwise end          h */
  float width;    /*  width (cm) (~|| CLAS theta)  */
  float thick;    /*  thickness (cm) (~|| CLAS r)  */
  float delta;    /*  the difference in length of the higher-theta slab minus the lower-theta slab divided by 4 (cm)  */
} scgg_t;

typedef struct {
	bankHeader_t bank;
	scgg_t scgg[1];
} clasSCGG_t;

/* --------------------- END scgg -----------------*/

typedef struct {      
  int ifirst;      /* First paddle id in this panel, C convention */
  int ilast;       /* Last paddle id in this panel,  C convention */
                   /* Normalized orthogonal vectors for panels. */
  vector3_t xo;    /* X direction: along widths pointing to higher paddle id. */
  vector3_t yo;    /* Y direction: along length of paddle pointing up in sec1. */
  vector3_t zo;    /* Z direction: pointing outwards. */    
  float tht[3];    /* The tht angle of xo,yo,zo (angle wrt Z) */
  float phi[3];    /* The phi angle of xo,yo,zo (polar angle) */
}SCpanel_t;

typedef struct {   /* Dimentions for one paddle */
  float width;
  float length;
  float thick;
  vector3_t  rm;  /* Vector pointing to center of paddle */  
}SCdim_t;

typedef struct {      /* Structure for passing geometry information. */
  
  SCdim_t   Dim[NUM_SCIN];
  SCpanel_t panel[MAX_PANEL];
  clasSCGG_t *SCGG;  /* Pointer to BOS bank. */
  float sc_parm[28]; /* Volume description of the SC mother volume: pgon */
  int   sc_parm_num;
}SC_geom_t;

/* Some MACROS to make access easier. */

#define ILast(i)      (Geom->panel[i].ilast)
#define IFirst(i)      (Geom->panel[i].ifirst)
#define GeomWidth(i)  (Geom->SCGG->scgg[i].width)
#define GeomThick(i)  (Geom->SCGG->scgg[i].thick)
#define GeomDelta(i)  (Geom->SCGG->scgg[i].delta)
#define GeomCWvec(i)  (Geom->SCGG->scgg[i].cw)
#define GeomCCWvec(i)  (Geom->SCGG->scgg[i].ccw)


/* Function stubs */

void geom_sc(void);
void geom_sc_init(int,SC_geom_t *);

