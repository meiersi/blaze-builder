benchmark:
	ghc --make -O2 -fforce-recomp -main-is Builder benchmarks/Builder.hs
	./benchmarks/Builder --resamples 10000

bench-chunked-write:
	ghc --make -O2 -fforce-recomp -main-is ChunkedWrite benchmarks/ChunkedWrite.hs
	./benchmarks/ChunkedWrite --resamples 10000

core-chunked-write:
	ghc-core -- --make -O2 -fforce-recomp -main-is ChunkedWrite benchmarks/ChunkedWrite.hs


test:
	runghc -itests tests/Tests.hs
