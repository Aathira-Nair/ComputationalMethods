program gauss_elimination

      implicit none

      real, allocatable, dimension(:,:)   :: a        !coefficients in matrix form
      integer                             :: i,j,k,n  !i,j,k - looping index, n - dimension of matrix
      real, allocatable,dimension(:)      :: x        !variables in equations
      real                                :: ratio     
      
      !Read the number of equations
      Write(*,*)"Enter the dimension (n) of matrix:"
      read(*,*)n
      
      allocate(a(n,n+1),x(n))
      open(unit=3, file = "gauss_elimination.dat", status = "old")
      
      !Write the coefficients in matrix form
      do i = 1,n
      do j = 1,n+1
      read(3,*)a(i,j)
      write(*,'(f15.3,t5)',advance = "no")a(i,j)
      end do
      write(*,*)
      end do
      
      !Calculations
      do k = 1,n-1
      do i = k+1,n

      ratio = a(i,k)/a(k,k)

      do j = 1,n+1

      a(i,j) = a(i,j) - ratio*a(k,j)

      end do 
      end do
      end do

      x(n) = a(n,n+1)/a(n,n)

      do k = n-1,1,-1
      x(k) = a(k,n+1)
      do j = k+1,3
      x(k) = x(k) - a(k,j)* x(j)
      end do
      x(k) = x(k)/a(k,k)
      end do 
      
      !Writing the values of x1,x2,x3....
      do i = 1,n
      write(*,"('x(',I2,'):',f12.6)")i,x(i)
      end do
      end program gauss_elimination 
      
