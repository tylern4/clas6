/*
 *  geant_shapes.c
 *
 *  define structure types for some of the standard GEANT shapes.
 *    alot easier to code and understand (w/ C) than just a pile
 *    of indices on an array.
 *
 *  remember: all angles for shape declaration in degrees
 */

/* $Id: geant_shapes.h,v 1.1 1999/07/30 19:53:53 feuerbac Exp $ */

typedef struct {
  int vol_id;
  char vol_name[5]; /* must be unique */
  char vol_type[5];
  int nparms;
  int medium;
} geant_shape_t;


/* box -- box with faces perpendicular to the axes
 */
typedef struct {
  float dx;
  float dy;
  float dz;
} geant_box_t;

typedef struct {
  geant_shape_t head;
  geant_box_t box;
} BOX_t;


/* trd1 -- trapezoid with the x dim varying along z
 */
typedef struct {
  float dx1;  /* half-length along x at the surface at -dz side */
  float dx2;  /* half-length along x at the surface at +dz side */
  float dy;   /* half-length along y */
  float dz;   /* half-length along z */
} geant_trd1_t;

typedef struct {
  geant_shape_t head;
  geant_trd1_t trd1;
} TRD1_t;


/* trd2 -- trapezoid with both x and y dimensions varying along z
 */
typedef struct {
  float dx1;  /* half-length along x at the surface at -dz side */
  float dx2;  /* half-length along x at the surface at +dz side */
  float dy1;  /* half-length along y at the surface at -dz side */
  float dy2;  /* half-length along y at the surface at +dz side */
  float dz;   /* half-length along z */
} geant_trd2_t;

typedef struct {
  geant_shape_t head;
  geant_trd2_t trd2;
} TRD2_t;


/* ctub -- cut tube. cut at the extremities with planes not neccessarily
 *         perpendicular to z.
 */
typedef struct {
  float rmin;        /* inside radius */
  float rmax;        /* outside radius */
  float dz;          /* half-length along z */
  float phi1;        /* starting angle of the segment */
  float phi2;        /* ending angle of the segment */
  vector3_t lface;   /* unit vector perpendicular to the face at -dz */
  vector3_t hface;   /* unit vector perpendicular to the face at +dz */
} geant_ctub_t;

typedef struct {
  geant_shape_t head;
  geant_ctub_t ctub;
} CTUB_t;

