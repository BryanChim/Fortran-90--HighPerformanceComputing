	program simple_combinatorial_enumeration
	integer :: h,counter=0,l=1
	integer, dimension(:), allocatable :: label_names
	logical, dimension(:), allocatable :: bl	
	real, dimension(10) :: timer_array
	real :: start_clock_timer, stop_clock_timer, total_loop_time
	real :: total_time_10_trials = 0
	real :: mean_time_10_trials
	
	open (unit=11,file='mean_elapsed_times.dat')
	
! do loop iterates through 6 instances: n=5, n=10, n=15, n=20, n=25, n=30
	do i=5,15,5
	
	! allocate appropriate memory and indices for label array and boolean array
		allocate(label_names(i))
		allocate(bl(i))
		
	! do loop performs 10 trials of running/timing the comb_enum subroutine
	! collects elapsed times for each trial in timer_array
		do j=1,10
		
			h = i
			l = 1			
			
			
			call cpu_time(start_clock_timer)
			!print *, 'clock is starting at ',start_clock_timer
			
			do k=1,i
				label_names(k) = k;
			end do
			
			open (unit=10,status='scratch')
			
			call comb_enum(label_names,bl,l,h,i)
			
			close (10)
			
			call cpu_time(stop_clock_timer)
			!print *, 'clock is finishing at ',stop_clock_timer
			total_loop_time=stop_clock_timer-start_clock_timer
			!print *, 'My cpu used ',total_loop_time,'seconds'
			!print *, ''
			
			timer_array(j) = total_loop_time
			
	
		
		end do
	
	! deallocate arrays in preparation for next round of trials
		deallocate(label_names)
		deallocate(bl)	
		
	! sum up total time from all 10 trials, and print
		do m=1,10
			total_time_10_trials = total_time_10_trials + timer_array(m)
			!print *, total_time_10_trials
		end do 
		print *, 'For n=', i
		print *, 'Total Time:', total_time_10_trials
	
	! calculate and print average elapsed time
		mean_time_10_trials = real(total_time_10_trials)/real(10.)
		print *, 'Average Time:', mean_time_10_trials
		print *, ''
	
	! write to mean_elapsed_times.dat, the mean time for n=i
		write (11,'(A25, i2, A4)', advance='no'), 'Mean elapsed time for n= ', i, 'is' 
		write (11,*), mean_time_10_trials

	! reset total time to 0
		total_time_10_trials = 0.0
		
	end do
	
	close (11)
	
	end program simple_combinatorial_enumeration

	

!!!!!!!! RECURSIVE SUBROUTINE COMB_ENUM
! comb_enum accepts an integer array, logical array and 3 integer values
! ln - array containing all the integer labels
! bool - array of same dimension as ln, 
!		contains boolean values that determine whether the label at a given index will be included in the subset
! low - integer that fixates on the current index, before determining if it will be true/false in the bool array
! high - integer corresponding to the highest index
! num - size of label array  	
    recursive subroutine comb_enum(ln,bool,low,high,num)
		integer :: low, high, num
		integer :: i = 1
		integer :: out_iterator = 1
		integer, dimension(num) :: ln
		logical, dimension(num) :: bool
		integer, save :: counter = 0
		
		
	! once "low" is incremented to the point where it is > "high", that indicates a termination point in the recursion
	! this means that "low" has touched every index 
	! -- the entire bool array has been populated with a distinct set of true/false values for each index
		if (low > high) then
		
			write (10,'(A)',ADVANCE="no"), '{'
		
		! first do loop prints out indexes i in the label array that are TRUE in the bool array (subset in 1st column)
			do i=1,num
				if (bool(i)) then
					write (10,'(I3)',advance="no"), ln(i)
				end if				
			end do
			
			write (10,'(A)',ADVANCE="no"), ' }'
			
			write (10,'(10x)',ADVANCE="no")
			
			write (10,'(A)',ADVANCE="no"), '{'
		
		! second do loop prints out indexes i in the label array that are FALSE in the bool array (subset in 2nd column)
			do i=1,num
				if (.not.(bool(i))) then
					write (10,'(I3)',advance="no"), ln(i)
				end if
				
			end do			
			
			write (10,'(A)',ADVANCE="no"), ' }'
			
			write (10,*), ' '
		
		! increment counter, once 2^n subsets is reached, print out to confirm
			!counter = counter + 1
			
			!if (counter==2**num) then
				!write (10,*), '# Subsets = ', counter
				!counter = 0
			!end if
		

	! low > high FALSE. there are still indexes left in the bool array that require a value
	! virtual binary tree, branch LEFT when "low" is set to TRUE. branch RIGHT when "low" is set to FALSE.
		else
		
		! set current fixated value "low" to TRUE, then proceed with recursion with next index (low + 1)
		
			bool(low) = .true.
			call comb_enum(ln, bool, low+1, high, num)
		
		! set current fixated value "low" to FALSE, then proceed with recursion with next index (low + 1)
			bool(low) = .false.			
			call comb_enum(ln,bool,low+1,high,num)
		
		end if
	   
    end subroutine comb_enum