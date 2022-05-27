program trapezoidal
      implicit none

      real, allocatable, dimension(:) :: x                         !iterative variable x for trapezoidal method
      real :: temp,sums                                            !temp : temporary variable for trapezoidal method, sum:
                                                                   !total integeration value
      real :: a,b                                                  !a,b - [a,b]
      real :: h                                                    !h = step size
      integer :: m,i                                               !m = no of iterations, i = loop index
      real*8, external :: f
      
      !Read the lower limit, upper limit and the no. of intervals
      Write(*,*)"Enter the lower limit"
      read(*,*) a

      write(*,*)"Enter the upper limit"
      read(*,*)b
      
      write(*,*)"Enter number of intervals"
      read(*,*)m
      
      !Calculate the stepsize
      h = (b-a)/m
      sums = 0.

      allocate(x(m))
      
      !Apply trapezoidal rule
      x(1) = a
      x(m) = b
      sums = f(a) + f(b)
      do i = 1,m-1
           x(i) = a + i*h
           temp = x(i)
           sums = sums + 2*f(temp)
      end do
      sums = h/2 * sums
      write(*,*) sums
      end program trapezoidal

     real*8 function f(x)
             implicit none

             real, intent(in) :: x

              f = 1/(1+x)

             end function f


