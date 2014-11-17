**simple_combinatorial_enumeration_OMPmotB.f90** uses the same fundamental algorithm as simple_combinatorial_enumeration.f90 (see **Simple_Combinatorial_Enumeration/README.txt**) but attempts to do it:

- on a larger set of ICD-9 four-letter codes (see n3/n4_exapmle_output.jpg)
- using multiple threads via OMP to parallelize the computation

I continued to perform timing tests to evaluate of the efficacy of parallelization -- you can find my results in the files **"n20 plot.jpg"** and **"n25 plot.jpg"**

* The way the parallelization is done is, essentially, instead of piling up all the recursive calls on one thread, the recursive calls are split up across an even number of threads at an appropriate point in the binary tree. See binary tree images for examples:

N = 4, Serial: **binary_tree_n4_serial.jpg**

N = 4, 4 threads: **binary_tree_n4_4threads.jpg**

N = 4, 8 threads: **binary_tree_n4_8threads.jpg**

*********************************************

Some notes/guidelines if you wish to test this program:

* The program works when compiled with ifort -openmp ... compilation. Trying to compile and run it with gfortran -fopenmp ... results in Segmentation faults.

* The program is currently coded to run at n=25 and use 512 threads (which it will do as long we have export OMP_NUM_THREADS=512). The way my program is designed, this will generate 512 output files, corresponding to the output of each thread -- these output files can be combined into 1 using the unix command: **cat output* > combined_output_file**. I'm sure there's some OMP clause to do this, I just never figured it out!

** If you want to modify the # threads that it works with, you'll need to modify line 6 in my program (in addition to changing $OMP_NUM_THREADS, to match it).
integer, **PARAMETER :: threads=<number of threads>**
 Also, due to the nature of the algorithm, the # threads you choose must be a power of 2 (8,32,64,256,1024 etc.)

** If you want to modify the n, it's a bit more of a hassle. There are 3 places in my program that you need to modify:
-- line 5: integer, PARAMETER :: h=<n>
-- line 26-28: label_names(# : # ) = (/ <ICD CODES> /) 
-- line 129: character(4), dimension(<n>) :: ln

** If you'd like to inspect the !$OMP directives, the whole parallel do loop starts at line 42