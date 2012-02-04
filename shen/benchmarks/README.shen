\* 

This is the benchmark macro for Shen.  Assuming your port to Blub is in the directory Platforms/Blub; do the
following.

1. (cd "../../Benchmarks")
2. (load "README.shen")
3. (load "benchmarks.shen")

  *\

(defmacro benchmark-macro
  [benchmark Message Benchmark] -> [do [nl] [output Message] [time Benchmark]])

