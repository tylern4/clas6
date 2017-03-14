#ifndef __ST_VECTOR_H__
#define __ST_VECTOR_H__

#include <vector>
#include "stpp_STdata.h"

typedef std::vector<STdata>::iterator dataiter_t;

/// container for intermediate results, provides a vector for each paddle
class STvector {
protected:
  std::vector<STdata> vstdata[ST_MAX];
public:
  /// constructor initializes vector
  STvector();

  /// returns index [0..23], best vector entry for given sector and segment
  int BestSegment(int sector, int segment8);

  /// returns an iterator to the first entry in STdata vector
  dataiter_t begin(int i) { return vstdata[i].begin(); }

  /// returns an iterator to the end (after last entry) of STdata vector
  dataiter_t end(int i)   { return vstdata[i].end(); }

  /// number of elements in STdata vector
  int size(int i) const   { return vstdata[i].size(); }

  /// sum of all elements in every vector
  int size();

  /// check whether TDC value is in window
  /// (CalDB Subsystems TDC_min/max)
  /// and ADC above threshold
  bool InWindow(double adc, int tdc);
};

#endif
