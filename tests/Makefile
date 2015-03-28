GHC_FLAGS = -Odph -rtsopts -threaded \
-fno-liberate-case -funbox-strict-fields -funfolding-keeness-factor1000 \
-fllvm -optlo-O3

YARR_FLAGS = $(GHC_FLAGS) -fexpose-all-unfoldings -fsimpl-tick-factor=500
REPA_FLAGS = $(GHC_FLAGS) -funfolding-use-threshold1000

DDUMP_CORE = -ddump-simpl -dsuppress-all

OPENCV_FLAGS = -O2 `pkg-config --cflags --libs opencv`

ct: color-trans.hs
	ghc --make $(YARR_FLAGS) $(DDUMP_CORE) -o ct color-trans.hs > ct-core.hs

blur: blur.hs
	ghc --make $(YARR_FLAGS) $(DDUMP_CORE) -o blur blur.hs > blur-core.hs

canny: canny.hs
	ghc --make $(YARR_FLAGS) $(DDUMP_CORE) -o canny canny.hs > canny-core.hs

ub: blur-unroll-bench.hs
	ghc --make $(YARR_FLAGS) $(DDUMP_CORE) -o ub blur-unroll-bench.hs > ub-core.hs

pm: planet-motion.hs
	ghc --make $(YARR_FLAGS) $(DDUMP_CORE) -o pm planet-motion.hs > pm-core.hs

repa-canny: canny-repa.hs
	ghc --make $(REPA_FLAGS) -o repa-canny canny-repa.hs

cv-canny: canny-opencv.cpp
	g++ -o cv-canny canny-opencv.cpp $(OPENCV_FLAGS)

le: lightness-equalization.hs
	ghc --make $(YARR_FLAGS) $(DDUMP_CORE) -o le lightness-equalization.hs > le-core.hs

%.as: %
	objdump -d $< > $@

clean:
	rm *.o *.hi *.dyn_o *.dyn_hi t-* ct blur canny ub pm repa-canny cv-canny le 2> /dev/null
    
