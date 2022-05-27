program gauss_siedel

      implicit none

      real*8, allocatable, dimension(:,:) :: a
      real*8, allocatable, dimension(:)   :: x,y,z,q,r
      integer                           :: n
      integer                           :: i,j,k,p,s,l, iter
      real, parameter                   :: error = 0.0000005
      integer, parameter                 :: iter_max = 100
      real*8                               :: temp
      logical                            :: check
      real                               :: calc
      
      p = 3*iter_max
      Write(*,*)"Write the dimension(n) of matrix:"
      read(*,*) n

      allocate(a(n,n+1),x(n),y(p),z(n),q(n),r(n))

      open(unit= 2, file = "gauss_siedel.dat", status = "old")
       open(unit = 3, file = "out.txt", status = "new")
      
      write(*,*)"The matrix form of the equations is:"
      do i = 1,n
      
      read(2,*)(a(i,j),j= 1,n+1)
     ! 100  format(I2)
      end do  
      
      
      
      do i = 1,n
      do j = 1,n+1
      write(*,'(f15.3,t3)',advance = "no")a(i,j)
      end do
      write(*,*)
      end do
      
      
      do i = 1,n
      	do j = 1,n
      			temp = 0
      			do k = 1,n
			temp = temp + ABS(a(i,k))
			!write(*,*) temp
			end do
			temp = temp - ABS(a(i,i))
			!write(*,*)temp
       end do
           if(ABS(a(i,i)) > ABS(temp)) then
      		check = .true.
      	   else 
      	        check = .false.
      	        exit 
      end if		
      end do 
      
      if (check .eqv. .true.) then
      	write(*,*)"The matrix is diagonally dominant"
      else 
      	write(*,*)"The matrix is not diagonally dominant"
      	end if  
      			
      	
     x = 0
     y = 0    
     k = 1
     l = 4
    
      do iter = 1,iter_max
      
      	do i = 1,n
     
      		x(i) = a(i,n+1)
      			do j = 1,n
      				if(i .ne. j) then
      				        x(i) = x(i) - a(i,j) * x(j)  
             
      				end if
      			end do  
      		x(i) = x(i)/a(i,i)
      		y(k) = x(i)
      		k = k+1
      		
      	end do
        
      	if(iter>1) then
      			        
      			calc = ABS(y(l-3) - y(l))	        
      			if (calc>error) then
      				write(*,*)"iterations:",iter
      		
      				do s = 1,n
      					write(*,*)x(s)
      				end do
      			else 
      				exit 
      			end if
			l = l+3
      	               
      	                end do
        end if
       
        write(3,*)iter,x
        
     end do
  
end program gauss_siedel
