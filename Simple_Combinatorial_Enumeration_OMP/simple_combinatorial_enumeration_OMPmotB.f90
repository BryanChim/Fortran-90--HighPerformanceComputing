	program simple_combinatorial_enumeration
	use omp_lib
	implicit none
	character(4) :: a
	integer, PARAMETER :: h=25
	integer, PARAMETER :: threads=512
	integer :: counter=0,l=1, h_initi, i, j, unitnum
	real :: h_init
	real :: start_clock_timer, stop_clock_timer, total_time
	character(4), dimension(h)  :: label_names
	logical, dimension(h) :: bl
	logical, dimension(:,:), allocatable  :: bl_init	
	common label_names
	
	print *, h, 'labels - combinations computed using'
	print *, threads, 'threads'	
	
	h_init = log(real(threads))/log(2.)
	h_initi = INT(h_init)
	allocate(bl_init(h_initi,threads))
	bl_init = .false.
	bl = .false.
	
		

	label_names(1:10) = (/ 'BEXT','BINT','CALI','CBON','CCNS','CCON','CEND','CEXT','CGNU','CONG' /)
	label_names(11:20) = (/'CRSP','CRST','CSKN','DEXT','DINT','EARS','EEXT','EINT','EYES','GEXT' /)
	label_names(21:25) = (/'GINT','HEXT','HINT','IBAC','INFT'/)
	call comb_enum_init(bl,bl_init,threads,l,h_initi)
	
	!do i=1,threads
	!	print *, (bl_init(j,i),j=1,h_initi)
	!end do	

	total_time=stop_clock_timer-start_clock_timer	

	
	!open (unit=10,file='ceptest.dat')	

	call cpu_time(start_clock_timer)
	l=h_initi+1
!$OMP PARALLEL DO FIRSTPRIVATE(bl,l,h_initi), PRIVATE(unitnum)
	do i=1,threads
!$OMP CRITICAL		
		unitnum = i+6
		write(a,"(i4.4)") i
		open(unit=unitnum, file='output'//a//'.txt')	
		bl(1:h_initi) = bl_init(1:h_initi,i)
!$OMP END CRITICAL
		call comb_enum(bl,l,h,unitnum)
		close(unitnum)
	end do
!$OMP END PARALLEL DO	
			
	!close (10)
	
	call cpu_time(stop_clock_timer)
	
	total_time=stop_clock_timer-start_clock_timer
	total_time= total_time/real(threads)	

	print *, 'total time for parallel: ', total_time
	
	
	end program simple_combinatorial_enumeration

	
	    recursive subroutine comb_enum_init(bool,bool_init,thr,low,high_init)
		integer :: low, high_init, thr
		integer :: i = 1
		logical, dimension(high_init) :: bool
		integer, save :: counter = 0
		logical, dimension(high_init,thr) :: bool_init

		
	! once "low" is incremented to the point where it is > "high", that indicates a termination point in the recursion
	! this means that "low" has touched every index 
	! -- the entire bool array has been populated with a distinct set of true/false values for each index
			

			
		if (low > high_init) then
			counter = counter + 1
			
			do i=1,high_init
				bool_init(i,counter) = bool(i)
			end do
			
			!if (counter==16) then
			!	do i=1,16
			!		print *, (bool_init(j,i), j=1,8)
			!	end do		
			!	print *, counter
			!end if			
			
		
	


	! low > high FALSE. there are still indexes left in the bool array that require a value
	! virtual binary tree, branch LEFT when "low" is set to TRUE. branch RIGHT when "low" is set to FALSE.
		else
		
		! set current fixated value "low" to TRUE, then proceed with recursion with next index (low + 1)
		
			bool(low) = .true.
			call comb_enum_init(bool,bool_init,thr,low+1,high_init)
		
		! set current fixated value "low" to FALSE, then proceed with recursion with next index (low + 1)
			bool(low) = .false.			
			call comb_enum_init(bool,bool_init,thr, low+1,high_init)
		
		end if
	   
    end subroutine comb_enum_init
	

!!!!!!!! RECURSIVE SUBROUTINE COMB_ENUM
! comb_enum accepts an integer array, logical array and 3 integer values
! ln - array containing all the integer labels
! bool - array of same dimension as ln, 
!		contains boolean values that determine whether the label at a given index will be included in the subset
! low - integer that fixates on the current index, before determining if it will be true/false in the bool array
! high - integer corresponding to the highest index
! num - size of label array  	
    recursive subroutine comb_enum(bool,low,high,unum)
		integer :: low, unum, high
		integer :: i = 1
		character(4), dimension(25) :: ln
		logical, dimension(high) :: bool
		common ln
		!integer, save :: counter = 0
		
		
	! once "low" is incremented to the point where it is > "high", that indicates a termination point in the recursion
	! this means that "low" has touched every index 
	! -- the entire bool array has been populated with a distinct set of true/false values for each index
		if (low > high) then
		
			write (unum,'(A)',ADVANCE="no"), '{'
		
		! first do loop prints out indexes i in the label array that are TRUE in the bool array (subset in 1st column)
			do i=1,high
				if (bool(i)) then
					write (unum,'(A6)',advance="no"), ln(i)
				end if				
			end do
			
			write (unum,'(A)',ADVANCE="no"), ' }'
			
			write (unum,'(10x)',ADVANCE="no")
			
			write (unum,'(A)',ADVANCE="no"), '{'
		
		! second do loop prints out indexes i in the label array that are FALSE in the bool array (subset in 2nd column)
			do i=1,high
				if (.not.(bool(i))) then
					write (unum,'(A6)',advance="no"), ln(i)
				end if
				
			end do			
			
			write (unum,'(A)',ADVANCE="no"), ' }'
			
			write (unum,*), ' '
		
		! increment counter, once 2^n subsets is reached, print out to confirm
			!counter = counter + 1
			
			!if (counter==2**high) then
			!	write (10,*), '# Subsets = ', counter
			!	counter = 0
			!end if
		

	! low > high FALSE. there are still indexes left in the bool array that require a value
	! virtual binary tree, branch LEFT when "low" is set to TRUE. branch RIGHT when "low" is set to FALSE.
		else
		
		! set current fixated value "low" to TRUE, then proceed with recursion with next index (low + 1)
		
			bool(low) = .true.
			call comb_enum(bool,low+1, high,unum)
		
		! set current fixated value "low" to FALSE, then proceed with recursion with next index (low + 1)
			bool(low) = .false.			
			call comb_enum(bool,low+1,high,unum)
		
		end if
	   
    end subroutine comb_enum
