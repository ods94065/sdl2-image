#ifndef HS_SDL2_IMAGE_WRAPPER_H_
#define HS_SDL2_IMAGE_WRAPPER_H_

/*
 * File        : sdl2_image_wrapper.h
 * Copyright   :  (c) Owen Smith 2014
 * License     :  BSD-like
 *
 * Maintainer  :  ods94043@yahoo.com
 * Stability   :  alpha
 * Portability :  portable
 *
 * This module wraps macros in the SDL2_image library, so that we can
 * call them portably from Haskell.
 */
#include "SDL_image.h"

extern void ImageVersion(SDL_version* outVersion);

#endif /* HS_SDL2_IMAGE_WRAPPER_H_ */
