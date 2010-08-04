bench-builder:
	ghc --make -O2 -fforce-recomp -main-is Builder benchmarks/Builder.hs
	./benchmarks/Builder --resamples 10000
