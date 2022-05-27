program karlpearson_correlation
implicit none

       !Data Dictionary

       integer, parameter :: max_size = 10000                            !max size of data
       real, dimension(max_size) :: x,y                                  !2 different sets of variables
       integer :: i,s,n,j                                                !i = loop index, s = I/O status; 0 for success
       real    ::  r                                                     !Karl pearson coefficient of correlation
       real    ::  sumx,term_x, sumy, term_y,num,sumx_sqrt,sumy_sqrt     !sum of x variables, sum of each x variable minus it's
                                                                         !mean, sum of y variables,sum of y variables minues their
                                                                         !mean, covariance,sum of squares of term_x, sum of squares
                                                                         !of term_y respectively 
       real    ::  tempx, tempy                                                  !temporary variable for reading
       real    ::  meanx, meany                                          !mean of x variables, mean of y variables
       logical :: exceed = .false.                                       !logical indicating array limits are exceeded

       !initialising values
       n = 0
       j = 0
       sumx = 0
       
       sumy = 0
       
       num = 0
       sumx_sqrt =0
       sumy_sqrt =0

       !Getting x variables for correlation
      open(unit = 9, file = "a.dat", status = "old",IOSTAT = s)

       do 
       read(9,*,IOSTAT = s) tempx
       if(s/=0) exit
       n = n+1
       if(n <= max_size) then
       x(n) = tempx
       
       else 
               exceed = .true.
       end if 
       end do 

       !Getting y variables for correlation
       open(unit = 7, file = "b.dat", status = "old",IOSTAT = s)

       do 
       read(7,*,IOSTAT = s) tempy
       if(s/=0) exit
       j = j+1
       if(j <= max_size) then
       y(j) = tempy
       
       else 
               exceed = .true.
       end if 
       end do
       
       !Calculating sum of x and y variables
       do i = 1,n
       sumx = sumx + x(i)
       sumy = sumy + y(i)

       end do 
       
       !Finding mean of x and y variables
       meanx = sumx/real(n)
       meany = sumy/real(n)
       
       !finding covariance and sum of input values squared
       do i = 1,n
       
       term_x =  x(i) - meanx
       term_y =  y(i) - meany
       num = num + term_x * term_y
       sumx_sqrt = sumx_sqrt + term_x**2
       sumy_sqrt = sumy_sqrt + term_y**2
       end do 
       
       !Calculating Karl Pearson's coefficient of correlation
       r = num/sqrt(sumx_sqrt*sumy_sqrt)
       
       !Telling the user the value of r
       write(*,*)"The Karl pearson's correlation is given as:",r
       
       close(9)
       close(7)
       end program karlpearson_correlation
