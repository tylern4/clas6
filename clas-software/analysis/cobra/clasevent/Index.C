#include "Index.h"
/** @file Index.C
 * @brief Index class source file.
 */
//_____________________________________________________________________________
/** @class Index
 *  @brief Utility class used by ClasEvent to handle particle/photon indicies.
 *
 *  Index is a utility class used by ClasEvent to keep track of particle and
 *  photon indicies in the data file (BOS,compressed ROOT,...). ClasEvent tells
 *  Index which particles it wants to look for using the Init function. Then,
 *  for each event, Index looks in the data file for those particles and 
 *  @a good tagged photons. The photons are considered @a good if difference
 *  in the event start time and the photon's vertex time are less than @a _dt.
 *  
 *  For each event, Index should be set using SetIndex. This function is a 
 *  template with no generic declaration...it must be specified for each 
 *  interface.
 *
 *  ClasEvent also uses Index to set its particle and photon info for each 
 *  event using the SetParticles and SetPhoton funcitons. These are also
 *  template functions with no generic declaration and must be specified for
 *  each interface.
 *
 */
//_____________________________________________________________________________

void Index::_Copy(const Index &__ind){
  /// Copy the @ind's data members to this Index object.
  int size,i,j,size2;
  _particles = __ind._particles;
  size = __ind.NumPhotons();
  _photIndex.resize(size);
  for(i = 0; i < size; i++) _photIndex[i] = __ind._photIndex[i];
  _numCharged = __ind._numCharged;
  size = (int)__ind._indexCombos.size();
  _indexCombos.resize(size);
  for(i = 0; i < size; i++){
    size2 = __ind._indexCombos[i].size();
    _indexCombos[i].resize(size2);
    for(j = 0; j < size2; j++) _indexCombos[i][j] = __ind._indexCombos[i][j];
  }
  _currentIndexPointer = __ind._currentIndexPointer;
  _currentPhotIndexPointer = __ind._currentPhotIndexPointer;
  _id_scheme = __ind._id_scheme;
  _dt = __ind._dt;
}
//_____________________________________________________________________________

void Index::Init(const std::vector<String> &__p_names){
  /// Initialize this Index object to look for @a p_names charged particles.
  
  // set up the vector of indicies
  int size = (int)__p_names.size();
  int num = 0;

  std::vector<String> names;
  std::vector<int> index;

  // add the 1st name
  names.push_back(__p_names[0]);
  index.push_back(0);

  for(int i = 1; i < size; i++){
    num = count(names.begin(),names.end(),__p_names[i]);
    names.push_back(__p_names[i]);
    index.push_back(num);
  }

  _particles = ParticleArray<int>::ParticleArray(names,index);

#ifdef __DEBUG__
  // if we're debugging...print stuff
  std::cout << "<Index::Init> __Index is looking for: ";
  int which;
  for(int i = 0; i < _particles.Size(); i++) {
    which = _particles[i];
    std::cout << _particles[i].Name() << "(" << which << ") ";
  }
  std::cout << std::endl;
#endif
}
//_____________________________________________________________________________

void Index::Reset(){
  /// Reset the Index object to get ready for the next event.
  _photIndex.clear();
  std::vector<std::vector<int> >::iterator iter = _indexCombos.begin();
  while(iter != _indexCombos.end()){
    iter->clear();
    iter++;
  }
  _indexCombos.clear();

  _numCharged = 0;
  _currentIndexPointer = 0;
  _currentPhotIndexPointer = 0;
}
//_____________________________________________________________________________

bool Index::_GetIndexCombos(const std::vector<std::vector<int> > &__p_index){
  /// Get all combos of indices from @a p_index.
  /** Returns @a true if a valid combination is found, @a false otherwise */

  int size = _particles.Size();
  int i;
  // check that each particle has at least 1 index found for each particle
  for(i = 0; i < size; i++) if((int)__p_index[i].size() == 0) return false;
      
  std::vector<int> combo(_particles.Size());
  // make 1st combo
  for(i = 0; i < size; i++) combo[i] = __p_index[i][0];

  // loop and make all possible combos
  int which_part = size - 1;
  std::vector<int> which_index(size,0);

  while(combo[0] != -1){
    this->_AddIndexCombo(combo); // add it to list of valid combos
    which_part = size - 1;
    which_index[which_part]++;
    combo[which_part] = __p_index[which_part][which_index[which_part]];
    while(which_index[which_part] >= (int)__p_index[which_part].size() && which_part >= 0){
      which_index[which_part] = 0;
      combo[which_part] = __p_index[which_part][0];
      if(which_part > 0){
	which_part--;
	which_index[which_part]++;
	combo[which_part] = __p_index[which_part][which_index[which_part]];
      }
      else{
	combo[0] = -1;
	which_part--;
      }
    }
  }
  if(this->NumEventCombos() > 0) return true;
  else return false;
}
//_____________________________________________________________________________

void Index::_AddIndexCombo(const std::vector<int> &__combo){
  /// Adds @a combo to list of valid index combos if @a combo needs to be added
  int size = (int)__combo.size();
  int i,j;

  // 1st check that none of the indicies are equal
  for(i = 0; i < size; i++){
    for(j = i+1; j < size; j++) if(__combo[i] == __combo[j]) return;
  }

  // now check that this combo hasn't allready been added to the list
  std::vector<bool> found_index(size);
  bool same_combo;

  for(i = 0; i < (int)_indexCombos.size(); i++){
    for(j = 0; j < size; j++){
      found_index[j] = false;
      for(int k = 0; k < size; k++){
	if(_indexCombos[i][k] == __combo[j]) {
	  found_index[j] = true;
	  break;
	}
      }
    }
    same_combo = true;
    for(j = 0; j < size; j++){
      if(found_index[j] == false){
	same_combo = false;
	break;
      }
    }
    if(same_combo) return; // don't add it, it's allready been added
  }

  // if we make it here it wasn't found
  _indexCombos.push_back(__combo);  
}
//_____________________________________________________________________________
