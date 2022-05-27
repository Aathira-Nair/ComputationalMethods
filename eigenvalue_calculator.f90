program eigen
      implicit none
     
      real,dimension(3,3)   :: mat            !given matrix
      real,dimension(3)     :: x,y            !eigen vectors
      integer               :: k,i,j          !looping index
      real                  :: iter           !iterations
      real                  :: lambda         !eigenvalue

      iter = 0 
      !Read the matrix from input file
      open(unit=5, file = "new5.dat", status = "old")
      read(5,*)mat

      do i = 1,3
      do j = 1,3
      write(*,'(f15.3,t5)',advance = "no")mat(i,j)
      end do
      write(*,*)
      end do
      
      !Calculations
      do i = 1,3
      x(i) = 1
      end do
     
     2 iter = iter + 1
     do i = 1,3
      y(i) = 0
      do j = 1,3

      y(i) = y(i) + mat(i,j) * x(j)

      end do 
      end do
       !finding the largest eigen value and eigen vector
       lambda = maxval(y)
       do i = 1,3
       y(i) = y(i)/lambda

       end do
       
       do k = 1,3
       if(abs(x(k) - y(k)) > 0.00005) then
               write(*,*)
               write(*,*)"iteration:", iter
               write(*,*)"Eigenvalue:",lambda
               write(*,*)
               write(*,*)"Eigenvector:"
               do i = 1,3
               x(i) = y(i)
               write(*,'(f12.6)')y(i)
               end do 
               
               goto 2
       end if
       end do 
       close(5)
       end program eigen
