benchmark:
	ghc --make -O2 -fforce-recomp -main-is Builder benchmarks/Builder.hs
	./benchmarks/Builder --resamples 10000

test:
	runghc -itests tests/Tests.hs
