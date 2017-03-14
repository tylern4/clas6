#include "Inx.h"


bool operator< (Inx& i, int a) {
  return (i.index < a);
}

bool operator> (Inx& i, int a) {
  return (i.index > a);
}

bool operator<= (Inx& i, int a) {
  return (i.index <= a);
}

bool operator>= (Inx& i, int a) {
  return (i.index >= a);
}

bool operator!= (Inx& i, int a) {
  return (i.index != a);
}

bool operator== (Inx& i, int a) {
  return (i.index == a);
}

ostream& operator<< (ostream& os, Inx& i) {
  os << "channel " << i.GetFSector() << ".";
  os.width(2);
  char savec = os.fill('0');
  os << i.GetFStripe();
  os.fill(savec);
  return os;
}

