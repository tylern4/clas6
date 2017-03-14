// CLASdata interace to ClasEvent header file. -*- C++ -*-
#ifndef _CLASdata_Interface_H
#define _CLASdata_Interface_H
/** @file CLASdata_Interface.h
 * @brief ClasEvent interface with CLASdata (compressed ROOT files) header.
 */

#include <CLASdata.h>
#include "ClasEvent.h"
#include "ParticleArray.h"

// ClasEvent member functions
template<> void ClasEvent::SetClasEvent(CLASdata *__data);
template<> void ClasEvent::SetClasEvent(CLASdata *__data,int __runNumber);
template<> bool ClasEvent::GetEvent(CLASdata *__data);
template<> void ClasEvent::_GetCovMatTrack(CLASdata *__data);
template<> TVector3 ClasEvent::GetVertex(CLASdata *__data, const std::vector<int> &__inds);
template<> float ClasEvent::GetDOCA(CLASdata *__data, const std::vector<int> &__inds);

// Index member functions
template<> bool Index::SetIndex(CLASdata *__data,
				float __targOffset,float __vtime);
template<> void Index::SetParticles(CLASdata *__data,
				    ParticleArray<Particle> &__parts,
				    ParticleArray<Particle> &__ignored,
				    float __vtime);
template<> void Index::SetPhoton(CLASdata *__data,Photon &__photon,
				 float __targOffset) const;


#endif
