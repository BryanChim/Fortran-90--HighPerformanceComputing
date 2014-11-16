	program network_diameter

!!!!VARIABLE/ARRAY DECLARATIONS
	logical :: new_unique_node
	logical :: diameter_not_found = .true.
	character*10 :: n1,n2
	character*50 :: in_filename, out_filename
	integer :: i, j, msize, indexX, indexY	
	integer :: diameter = 1
	integer :: num_pairs = 0 
	integer :: num_unique_nodes = 0
	integer :: eof_status = 0
	integer :: array_iterator = 1
	
	PARAMETER (N=200)

!!!!MATRICES
	character*10, dimension(N) :: name_array			! holds unique node names
	integer, dimension(:,:), allocatable :: matrix		! holds the initial connectivity matrix
	integer, dimension(:,:), allocatable :: n_matrix	! holds connectivity matrix following each multiply
	logical, dimension(:,:), allocatable :: tfmatrix	! holds the logical matrix to check connections
	
!!!!FILENAME READS	
	print *, 'Enter network data input file name: '
	read *, in_filename
	print *, 'Enter analysis output file name: '
	read *, out_filename
	
	open(UNIT=10,FILE=in_filename,STATUS='OLD')
	open(UNIT=11,FILE=out_filename,STATUS='NEW')


!!!!DO WHILE LOOP - counts # node-node pairs and # unique nodes	
	DO WHILE (eof_status >= 0)
	
	read(10,*,iostat=eof_status) n1,n2
	
		IF (eof_status==0) THEN
			num_pairs = num_pairs + 1
				
			IF (ALL(name_array.ne.n1)) THEN
				num_unique_nodes = num_unique_nodes + 1
!				allocate(name_array(num_unique_nodes))
				name_array(array_iterator) = n1
				array_iterator = array_iterator + 1				
			END IF
				
			IF (ALL(name_array.ne.n2)) THEN
				num_unique_nodes = num_unique_nodes + 1
!				allocate(name_array(num_unique_nodes))		
				name_array(array_iterator) = n2
				array_iterator = array_iterator + 1

			END IF
			
!			print *, n1,' ',n2

		END IF
	
	END DO

!!!!Write # pairs and # unique nodes to output file
	write (11, *) 'Results for ', in_filename
	write (11, *) ' '	
	write (11, *) '#pairs: ', num_pairs
	write (11, *) '#unique nodes: ',num_unique_nodes
 
!	print *, name_array

!!!!Rewind input file, this time to read in the connections		
	rewind(UNIT=10)	

!!!!Allocate appropriate size memory to matrices	
	msize = num_unique_nodes
	allocate (matrix(msize,msize))
	allocate (n_matrix(msize,msize))
	allocate (tfmatrix(msize,msize))	

!!!!Initialize matrices and reset eof_status	
	matrix = 0
	tfmatrix = .false.	
	eof_status = 0

	
!!!!Read in node pairs and get their indices from the name array generated above	
	DO WHILE (eof_status >= 0)
	
	read(10,*,iostat=eof_status) n1,n2
	
		IF (eof_status==0) THEN
		
			do i=1,msize
				if (name_array(i) .eq. n1) then
					indexX = i
					exit
				endif
			end do		

			do i=1,msize
				if (name_array(i) .eq. n2) then
					indexY = i
					exit
				endif
			end do			

!!!!!!!!!!!!Set the appropriate row/column to 1, and do the same for its mirror			
			matrix(indexX, indexY) = 1
			matrix(indexY, indexX) = 1
			tfmatrix(indexX, indexY) = .true.
			tfmatrix(indexY, indexX) = .true.

			


		END IF
	END DO
	
!!!!PRINT/WRITE Statements for viewing the initial connectivity matrix and logical matrix	
	write (11, *) 'Initial Matrix'
	
	do i=1,msize
		write (11, *)  name_array(i), (matrix(j,i), j=1,num_unique_nodes)
	end do
	
!	write (11, *) ' '
	
!	do i=1,msize
!		print *, (tfmatrix(j,i), j=1,num_unique_nodes)
!	end do
	
	close (UNIT=10)
	
	n_matrix = matrix


!!!!Perform matrix multiplication and update the logical matrix until it is entirely .true.
!!!!Once the whole logical matrix is found to be true, diameter_not_found = .false., and exit
	DO WHILE (diameter_not_found) 
!	DO k=1,5
		n_matrix = matmul (n_matrix,matrix)
		
		diameter = diameter + 1
		
	
		
		do i =1, msize
			do j=1, msize
				if (n_matrix(i,j).ne.0) then
					tfmatrix(i,j)=.true.
					tfmatrix(j,i)=.true.
				end if
			end do
		end do
		
		
		if (ALL(tfmatrix)) then
			diameter_not_found = .false.
		end if
		
		
	
	END DO	
	
!!!!WRITE statement to view the final matrix after diameter has been determined
	write (11, *) 'Final Matrix'
	
	do i=1,msize
		write (11,*) name_array(i), (n_matrix(j,i), j=1,msize)
	end do		


!!!!Output the diameter, close and end.	
	write (11, *) ' '	
	write (11, *) 'Network Diameter =', diameter			

	close(UNIT=11)		
	
	end program network_diameter

