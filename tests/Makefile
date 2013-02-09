GHCFLAGS = -Odph -rtsopts -threaded -fno-liberate-case -funbox-strict-fields \
-fexpose-all-unfoldings -funfolding-keeness-factor1000 -fsimpl-tick-factor=500 \
-fllvm -optlo-O3

DDUMP_CORE = -ddump-simpl -dsuppress-all

GCCFLAGS = -O2

ct: color-trans.hs
	ghc --make $(GHCFLAGS) $(DDUMP_CORE) -o ct color-trans.hs > ct-core.hs

blur: blur.hs
	ghc --make $(GHCFLAGS) $(DDUMP_CORE) -o blur blur.hs > blur-core.hs

canny: canny.hs
	ghc --make $(GHCFLAGS) $(DDUMP_CORE) -o canny canny.hs > canny-core.hs

ub: blur-unroll-bench.hs
	ghc --make $(GHCFLAGS) $(DDUMP_CORE) -o ub blur-unroll-bench.hs > ub-core.hs

%.as: %
	objdump -d $< > $@

%.o: %.hs
	ghc $(GHCFLAGS) $<

%.o: %.c
	gcc -c $(GCCFLAGS) $<

clean:
	rm *.o *.hi t-* ct blur canny ub
    