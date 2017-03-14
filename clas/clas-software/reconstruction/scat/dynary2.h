#ifndef _DYNARRAY2_H_
#define _DYNARRAY2_H_

#include <iostream.h>
#include <stdlib.h>
#include <string.h>

//======================================================
typedef struct DyNode {
  void          *inf;
  DyNode        *next;
  DyNode        *prev;
} DyNode;
//======================================================
class DynArray {
  DyNode        *head;
  DyNode        *tail;
  long          size;
  void          error (int, char *msg);
  void          error (int, char *msg, int);
  void          warning (char *msg);
  void          warning (char *msg, int);
 public:
  DynArray ();
  ~DynArray ();
  long          add (void *);
  int           remove (long);
  int           remove (void *);
  void *        operator[](long);
  long          count();
  long          IndexOf (void *);
  DyNode *      danger (long);
};
//======================================================
void DynArray::error (int r, char *msg, int i)
{
  cout << "DynArray error: " << msg << " : " << i << endl;
  exit (r);
};
//======================================================
void DynArray::error (int r, char *msg)
{
  cout << "DynArray error: " << msg << endl;
  exit (r);
};
//======================================================
void DynArray::warning (char *msg)
{
  cout << "DynArray warning: " << msg << endl;
};
//======================================================
void DynArray::warning (char *msg, int i)
{
  cout << "DynArray warning: " << msg << " : " << i << endl;
};
//======================================================
DynArray::DynArray () {

  size = 0;
  head = NULL;
  tail = NULL;
};
//======================================================
DynArray::~DynArray () {

  DyNode *p, *t;

  for (p = head; p != NULL;) {
    t = p;
    p = p->next;
    delete t;
  }
};
//======================================================
long DynArray::add (void *new_element) {

  if ( (head == NULL) && (tail == NULL) ) {
    DyNode *j = new DyNode ;
    j->inf = new_element ;
    j->next = NULL;
    j->prev = NULL;
    head = j;
    tail = j;
    return ++size;
  }

  DyNode *j = new DyNode;

  j->inf = new_element;
  j->next = tail->next;
  j->prev = tail;
  tail->next = j;
  tail = j;

  return ++size;
};
//======================================================
int DynArray::remove (long z) {

  int cnt=0;
  DyNode *p, *n, *rp;

  for (cnt=0, rp = head; cnt<z; cnt++) {
    if (rp == NULL) break;
    rp = rp->next;
  }
    
  if (rp == 0) {
    warning ("DynArray: Bogus index");
    return 0;
  }
  size -= 1;
  if ( (rp->next == NULL) && (rp->prev == NULL) ) {
    delete rp;
    head = NULL;
    tail = NULL;
    return 1;
  }
  if ( (rp->next != NULL) && (rp->prev != NULL) ) {
    p = rp->prev;
    n = rp->next;
    p->next = rp->next;
    n->prev = rp->prev;
    delete rp;
    return 1;
  } 
  if (rp->next == NULL) {
    p = rp->prev;
    p->next = NULL;   ;
    tail = p;
    delete rp;
    return 1;
  } 
  if (rp->prev == NULL) {
    n = rp->next;
    n->prev = NULL;
    head = n;
    delete rp;
    return 1;
  }
  warning ("control at the end of sanity");
  return 0;
};
//======================================================
int DynArray::remove (void * z) {

  DyNode *rp, *p, *n;

  for (rp=head ; rp!=NULL; rp=rp->next) 
    if (rp->inf == z) break;

  if (rp==NULL)
    return 0;

  size -= 1;
  if ( (rp->next != NULL) && (rp->prev != NULL) ) {
    p = rp->prev;
    n = rp->next;
    p->next = rp->next;
    n->prev = rp->prev;
  } else
    if (rp->next == NULL) {
      if (rp->prev == NULL) {
	rp->inf = 0;       // remove the only one ;
	head = 0;
	tail = 0;
	return 1;
      }
      p = rp->prev;
      p->next = rp->next;
      tail = p;
    } else
      if (rp->prev == NULL) {
	n = rp->next;
	n->prev = NULL;
	head = n;
      }
  
  delete rp;
  return 1;
  
};
//======================================================
void * DynArray::operator[](long x)
{
  int i;
  DyNode *p;

  if ((head == 0) && (tail == 0)) return 0;

  if (x <0 || x >= size) {
    warning ("operator[] - index out of range");
    return 0;
  }
  for (i=0, p=head; i<x; i++) {
    if (p==NULL) 
      return 0;
    p=p->next;
  }
  return p->inf;
}
//======================================================
long DynArray::IndexOf (void * x)
{
  long i;
  DyNode *p;

  for (p=head, i=0; p!=NULL; i++) {
    if (p->inf == x) break;
    p = p->next ;
  }
  return i;
}
//======================================================
DyNode * DynArray::danger (long position)
{
  DyNode * rp;
  long cnt;

  for (cnt=0, rp=head; cnt < position; cnt++) {
    if (rp == NULL) return 0;
    rp = rp->next ;
  }
  return rp;
}
//======================================================
long DynArray::count ()
{
  DyNode *p = head;
  long i=0;

  while (p!=NULL) {
    i++;
    p = p->next;
  }
  return i;
}
//======================================================

#endif

 
