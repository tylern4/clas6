/*
 *  JAM - The Simple Communications Attachment Library.
 *
 *  List processing code.
 *
 *  Revision history:
 *  JAM - 01/1996 - initial version
 *  JAM - 05/1996 - add in this banner.
 */
#include <stdio.h>
#ifdef Darwin
#include <sys/malloc.h>
#else
#include <malloc.h>
#endif

#include <string.h>
#include "lst.h"
/* -**********************************************************- */
void lst_error (wine)
char *wine ;
{
  printf ("lst_error: %s\n", wine);
}
void lst_abort (w)
char *w;
{
  printf ("lst_abort: %s\n", w);
  exit (0);
}
/* -**********************************************************- */
/* -==========================================================- */
int lst_init (jam)
CtlBlk *jam ;
{
  jam->head = 0;
  jam->tail = 0;
  jam->cursor = 0;
  jam->nItems = 0;
}
/* -==========================================================- */
PFrag * get_pfrag (name, format, nr, rows, cols, size, type, pi)
char *name;
char *format;
int  nr;
int  rows;
int  cols;
int  size;
int  type;
int *pi;
{
  PFrag *p;

  if ((p = (PFrag *)malloc (sizeof(PFrag))) == 0) {
    lst_error ("out of memory 1");
  }
  p->name = strdup (name);
  p->format = strdup (format);
  p->nr = nr;
  p->nrows = rows;
  p->ncols = cols;
  p->d_size = size;
  p->type = type;
  if (type == 0)
    p->in = pi;
  else
    p->sn = (short *)pi;
  p->next = 0;

  return p;
}
/* -==========================================================- */
int lst_add (jam, p)
CtlBlk *jam;
void *p;
{
  if (p == 0) return 0;

  if (jam->head == 0) {
    jam->head = p;
    jam->tail = p;
    jam->nItems += 1;
    return 1;                   /* add to the front. */
  } ;

  if (jam->tail == 0) 
    lst_abort ("list corrupted!") ;

  jam->tail->next = p;
  jam->tail = p;

  jam->nItems += 1;
  return 1;
}
/* -==========================================================- */
void lst_drop (jam)
CtlBlk *jam ;
{
  int i;
  PFrag *p, *t;

  if (jam->head == 0) return ;

  p = jam->head ;
  for (i=0; i<jam->nItems; i++) {
    t = p;
    p = p->next;
    free (t->name) ;
    free (t);
  }
  jam->head = 0;
  jam->tail = 0;
  jam->cursor = 0;
  jam->nItems = 0;
}
/* -==========================================================- */
int lst_count (jam)
CtlBlk *jam;
{
  int c=0;
  PFrag *p = jam->head;
  while (p) {
    c += 1;
    p = p->next;
  }
  return c;
}
/* -==========================================================- */
/*  f == 0 means reset to the front of the list 
 *         and return that one.
 *  f != 0 means return the next one on list
 */
PFrag * lst_nxt (jam, f)
CtlBlk *jam;
int f;
{
  if (f == 0) 
    jam->cursor = jam->head;
  else 
    jam->cursor = jam->cursor->next;

  return jam->cursor;
}
/* -==========================================================- */

/* -==========================================================- */
