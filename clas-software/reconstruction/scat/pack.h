#ifndef _PACK_H_
#define _PACK_H_

/* ========================================================= */
typedef struct PFrag {
  char *name;
  int  num;
  int  sw;
  int  *in;
} PFrag ;
/* ========================================================= */
class Packer {
  int nItems ;
  int size_n_words ;
  DynArray pkeep;
 public:
  Packer   () ;
  ~Packer () ;
  Packer   (char *, int, int, int *) ;
  int      add (char *, int, int, int *) ;
  int      count () ;
  int      packed (Event *) ;
} ;
/* ========================================================= */
void makeBank (aBank *o1, PFrag *jj) 
{
  int i;

  o1->sraw.sraw_len = 0;
  o1->sraw.sraw_val = 0;

  o1->iraw.iraw_len = jj->sw;
  o1->iraw.iraw_val = (int *)malloc (jj->sw*(sizeof(int)));
  for (i=0; i<jj->sw; i++)
    o1->iraw.iraw_val[i] = jj->in[i];
  o1->name = jj->name;
  o1->number = jj->num;
  o1->size = jj->sw;
}  
/* ========================================================= */
Packer :: Packer () 
{
  nItems = 0;
  size_n_words = 0;
}
/* ========================================================= */
Packer :: Packer (char *n, int nm, int size, int *id)
{
  int l;
  PFrag *pf = new PFrag ;
  if (n == 0) {
    l = 7;
    pf->name = "unknown";
  }
  else {
    l = strlen (n);
    pf->name = strdup (n);
  }
  pf->num = nm;
  pf->sw = size;
  pf->in = id;

  nItems = 1;
  size_n_words = l+1+size;
  pkeep.add (pf);
} ;
/* ========================================================= */
Packer :: ~Packer ()
{
  for (int i=0; i<pkeep.count(); i++) {
    PFrag *p = (PFrag *)pkeep[i];
    delete [] p->name;
    delete [] p->in ;
    pkeep.remove (p);
    delete p;
  }
};
/* ========================================================= */
int Packer :: add (char *n, int nm, int size, int *pid)
{
  int l;
  PFrag *pf = new PFrag ;
  if (n == 0) {
    l = 7;
    pf->name = "unknown";
  }
  else {
    l = strlen (n);
    pf->name = strdup(n);
  }
  pf->num = nm;
  pf->sw = size;
  pf->in = pid;

  nItems += 1;
  size_n_words += l+1+size;
  pkeep.add (pf);
  return 1;
};
/* ========================================================= */
int Packer :: count ()
     // return the number of items
{
  return nItems;   
};
/* ========================================================= */
int Packer :: packed (Event *_i) 
{
  _i->b.b_len = this->pkeep.count () ;
  _i->b.b_val = new aBank [nItems] ;
  aBank * _z = _i->b.b_val ;
  for (long ii=0; ii<nItems; ii++) {
    makeBank (_z, (PFrag *)pkeep[ii]);
    _z++ ;
  }
  return _i->b.b_len ;
}
	
  
#endif
