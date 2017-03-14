// BOS file interace to ClasEvent header file. -*- C++ -*-
#ifndef _BOS_Interface_H
#define _BOS_Interface_H
/** @file BOS_Interface.h
 * @brief ClasEvent interface with CLAS BOS files header.
 */
#include <cmath>
#include "ClasEvent.h"
#include "ParticleArray.h"
#include "Bos.h"

// ClasEvent member functions
template<> void ClasEvent::SetClasEvent(Bos *__bos);
template<> void ClasEvent::SetClasEvent(Bos *__bos,int __runNumber);
template<> bool ClasEvent::GetEvent(Bos *__bos);
template<> void ClasEvent::_GetCovMatTrack(Bos *__bos);

// Index member functions
template<> bool Index::SetIndex(Bos *__bos,
				float __targOffset,float __vtime);
template<> void Index::SetParticles(Bos *__bos,
				    ParticleArray<Particle> &__parts,
				    ParticleArray<Particle> &__ignored,
				    float __vtime);
template<> void Index::SetPhoton(Bos *__bos,Photon &__photon,
				 float __targOffset) const;


#endif
