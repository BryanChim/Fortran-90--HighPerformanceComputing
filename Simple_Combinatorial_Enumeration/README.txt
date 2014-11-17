**simple_combinatorial_enumeration.f90** performs a series of combinatorial enumeration trials (10 trial for each label n = 5, n = 10 and n = 15, currently) and calculates the average CPU time required to complete in each case. 

Simple combinatorial enumeration is the calculation of every possible subset of a set. 

ie. for set {A, B, C}, all possible subsets are:
{A, B, C}
{A, B}
{A, C}
{A}
{B, C}
{B}
{C}
{}

EXAMPLE: see **"subsets n 3 4 5.jpg"** for a listing of subsets for n=3,4,5

This program uses the recursive algorithm explained on this web page: http://cod3rutopia.blogspot.com/2013/07/finding-all-subsets-of-set.html

Here is my explanation, as quoted from my write-up for the HPC course (follow along with binary_tree.jpg): 

"Each rcomb(#,#) represents a subroutine call. The left number is the “low” variable, the index for which is either declared TRUE or FALSE in a boolean array. The right number is the “high” variable, the last and largest index in the array.

Starting with rcomb(1,4), we branch left if “low” (1) is set to TRUE in the boolean array. We branch right if “low” is set to FALSE in the boolean array. This mandates, depending on which branch we descend from, whether or not the label at index (1) will be included in those subsets or not. As you can see, all the outputted subsets in the left branch contain 1, while all the outputted subsets in the right branch lack 1. 

Each subroutine call, thus, leads recursively to another 2 subroutine calls – one for if the low index is TRUE and one for if the low index is FALSE.  This repeats (ie. for low = 2, 3, 4) until “low” touches every index, and is incremented beyond “high” – recursion ends and then we output our subset based on the combination of TRUE/FALSE values in the boolean array." 
