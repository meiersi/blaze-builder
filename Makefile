bench-blaze-vs-binary:
	ghc --make -O2 -fforce-recomp -main-is BlazeVsBinary benchmarks/BlazeVsBinary.hs
	./benchmarks/BlazeVsBinary --resamples 10000

bench-chunked-write:
	ghc --make -O2 -fforce-recomp -main-is ChunkedWrite benchmarks/ChunkedWrite.hs
	./benchmarks/ChunkedWrite --resamples 10000

core-chunked-write:
	ghc-core -- --make -O2 -fforce-recomp -main-is ChunkedWrite benchmarks/ChunkedWrite.hs

# throughput benchmarks: interactive development
ghci-throughput: benchmarks/Throughput/CBenchmark.o 
	ghci -O2 -fforce-recomp -ibenchmarks -main-is BenchThroughput benchmarks/Throughput/CBenchmark.o benchmarks/BenchThroughput.hs

bench-throughput: benchmarks/Throughput/CBenchmark.o
	ghc --make -O2 -fforce-recomp -fliberate-case-threshold=1000 -ibenchmarks -main-is BenchThroughput benchmarks/Throughput/CBenchmark.o benchmarks/BenchThroughput.hs
	./benchmarks/BenchThroughput 100

benchmarks/Throughput/CBenchmark.o: benchmarks/Throughput/CBenchmark.c
	gcc -O3 -c $< -o $@

bench-string-and-text:
	ghc --make -O2 -fforce-recomp -ibenchmarks -main-is StringAndText StringAndText
	./benchmarks/StringAndText --resamples 10000

bench-compression:
	ghc --make -O2 -fforce-recomp -ibenchmarks -main-is Compression Compression
	./benchmarks/Compression --resamples 10000


test:
	runghc -itests tests/Tests.hs
