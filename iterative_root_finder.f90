program root
      implicit none
      
      !Data Dictionary
      real*8 :: f,x,i,temp,y       !f= iterative function, i = initial value, temp = temporary variable, x, y = input variable
      integer :: k                 !no of iterations
 
      !Getting the initial value from user         
      write(*,*)"Enter the value of x1:"
      read(*,*)i

      !Calculating the iterative formula
      k = 1
      2 x = f(i)
      temp = x
      write(*,"('k:',I5,5x,'x_',I3,':',F15.8,5X,'f(x):',f15.8)")k+1,k+1,x,f(x)
      
      !Setting a stopping condition
      if(abs(x-i) >= 0.000005) then
              k = k+1
              i = temp
              goto 2
      end if
      
      !Telling the user
      write(*,*)"The root of the equation is x =",x
      stop
     
      end program root

!function to find the iterative function
      real*8 function f(y)
      real*8 :: y 
     
      f = (1 - 3*y + 3*y**2)**(1./5.)
       end function

