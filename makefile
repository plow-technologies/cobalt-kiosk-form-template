#!/bin/bash

# This file creates a binary output of cobalt-kiosk-form-template and the C interface to be used by iOS.
# to compile this your system must have GHC-iOS.

make:
	hsc2hs src/Kiosk/Backend/Form/Rendering/CStruct.hsc
	Kiosk.Backend.Form.Rendering.CStruct
	src/Backend/Form/Rendering/foo.h

clean:
