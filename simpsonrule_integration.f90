program simpson
      implicit none

      real*8, external  :: f                              
      integer           :: m,i                         !no of intervals,looping index
      real              :: h                           !stepsize
      real              :: p,q                         !intervals [p,q]
      real              :: temp_s, sums_s              !temp_s, temp_z - temporary variables, sums_s - total integration
                                                       !value
      real, allocatable, dimension(:) :: y,z           !iterative variables y,z
      
       !Solving using Simpson's rule
      Write(*,*)"Enter the lower limit"
      read(*,*) p

      write(*,*)"Enter the upper limit"
      read(*,*)q
      
      write(*,*)"Enter number of intervals"
      read(*,*)m
      
      !Calculating stepsize
      h = (q-p)/m
      allocate(y(m),z(m))

      sums_s = f(p) + f(q) 

      do i = 1, m-1
      temp_s = p + i*h
      if(mod(i,2) == 0) then
              sums_s = sums_s+2*f(temp_s)
      else 
              sums_s = sums_s + 4*f(temp_s)
      end if
      end do
    
      sums_s = h/3 * sums_s
       

      write(*,*) sums_s
end program simpson

  real*8 function f(x)
             implicit none

             real, intent(in) :: x

             f = log(x)*log(2.71828)

             end function f

