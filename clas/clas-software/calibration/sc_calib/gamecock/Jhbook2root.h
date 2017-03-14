#ifndef __ROOTINTERFACE_H
#define __ROOTINTERFACE_H

#ifdef __cplusplus
extern "C" {
#endif

void rootopenw(const char* filename);
void rootend();
void rootbook1(int hid, const char* title, 
	       int xbins, double xmin, double xmax);
void rootbook2(int hid, const char* title, 
	       int xbins, double xmin, double xmax, 
	       int ybins, double ymin, double ymax);
void rootf1(int hid, double x, double value);
void rootf2(int hid, double x, double y, double value);
void rootopera(int h1, const char* coperator, int h2, int h3, 
	       double c1, double c2);
  /*------------------------------*/

void rootcanvas(int width, int height);
void rootdraw(int hid);
void rootupdate();
void rootrun();

  /*
void rootf1_(int* id_, double* x_, double* value_) {
  rootf1(*id_, *x_, *value_);
}

void rootbook1_(int* id_, const char* ctitle_, 
		int* ixbins_, double* xmin_, double* xmax_);

  */

#ifdef __cplusplus
}
#endif
#endif
