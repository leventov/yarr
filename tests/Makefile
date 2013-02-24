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

repa-canny: repa-canny.hs
	ghc --make $(REPA_FLAGS) -o repa-canny repa-canny.hs

cv-canny: canny-opencv.cpp
	g++ -o cv-canny canny-opencv.cpp $(OPENCV_FLAGS)

le: lum-equalization.hs
	ghc --make $(YARR_FLAGS) $(DDUMP_CORE) -o le lum-equalization.hs > le-core.hs

%.as: %
	objdump -d $< > $@

%.o: %.hs
	ghc $(GHC_FLAGS) $<

%.o: %.c
	gcc -c $(GCC_FLAGS) $<

clean:
	rm *.o *.hi t-* ct blur canny ub repa-canny le
    