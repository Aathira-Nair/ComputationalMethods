program differentiation
      implicit none


      integer                       :: i,m        !looping index and iterations
      real,allocatable,dimension(:) :: x,f,g,s    !x- variable, f- function, g - first differentiation, s- second differentiation
      
      !Read the number of iterations and the data
      write(*,*)"Enter the number of iterations"
      read(*,*) m

      allocate(x(m),f(m),g(m),s(m))
    
      open(unit=4,file = "new4.dat",status = "old")
      read(4,*)x,f      
    
     
      write(*,'(A4,15x,A4,15x,A4,15x,A4)')"x(i)","f(i)","g(i)","s(i)"
      do i = 0,m-1
      g(i) = (f(i+1) - f(i))/(x(i+1) - x(i))

      s(i) = (f(i-1) - 2*f(i) + f(i+1))/(x(i+1) - x(i))**2
      write(*,*)x(i),f(i),g(i),s(i)
      end do

      close(4)
      end program differentiation






