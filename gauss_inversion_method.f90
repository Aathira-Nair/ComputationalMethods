program gauss_inversion
      implicit none

      real, dimension(3,3) :: a,b      !a- matrix to be inverted, b - identity matrix
      integer              :: k,i,j    !looping index
      real*8               :: ratio
      
      !Read the matrix from input file and the identity matrix
      open(unit = 2, file = "gauss_inversion.dat", status = "old")
     
      do i = 1,3
      do j = 1,3
      read(2,*) a(i,j)
      b(i,j) = 0
      end do
      b(i,i) = 1
      end do

      Write(*,*)"The matrix given is:"
      do i = 1,3
      do j = 1,3
      write(*,'(f15.3,t5)',advance = 'no') a(i,j)
      end do 
      write(*,*)
      end do 
     
      write(*,*)"The identity matrix is:"
      do i = 1,3
      do j = 1,3
      write(*,'(f15.3,t5)',advance = 'no') b(i,j)
      end do 
      write(*,*)
      end do 

      !Calculations
      do k = 1,3
      do i = 1,3

      if(i == k) then 
              goto 1
      end if   
              ratio = a(i,k)/a(k,k)
              do j = 1,3
              a(i,j) = a(i,j) - ratio* a(k,j)
              b(i,j) = b(i,j) - ratio * b(k,j)
              end do 
     1        end do
              end do

              do i = 1,3
              do j = 1,3
              b(i,j) = b(i,j)/a(i,i)
              end do
              end do
              
              write(*,*)"The inverted matrix is:"
              do i = 1,3
              do j = 1,3

              write(*,'(f15.3,t5)',advance = 'no')b(i,j)
              end do
              write(*,*)
              end do

              close(2)

              end program gauss_inversion
      


