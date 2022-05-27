program pivotal_condense_method

      implicit none
      
      !Data dictionary
      integer ::  i, j,k                                   !loop index
      real*8, allocatable, dimension(:,:) :: mat,ratio     !matrix1
      real :: det                                          !determinant of the matrix

      integer :: m                                         !dimension of matrix1
      
      write(*,*)"Enter the dimension of matrix:"
      read(*,*)m

      allocate(mat(m,m),ratio(m,m))

      !Getting values from input file
      open(unit = 1, file = "input.dat", status = "old")
      read(1,*) mat
  
      !creating an upper triangular matrix
      do k = 1,m-1
      do i = k+1,m

      ratio(k,k) = mat(i,k)/mat(k,k)

      do j = 1,m

      mat(i,j) = mat(i,j) - ratio(k,k) * mat(k,j)

      end do
      end do
      end do
      
      !calculating value of determinant
      det = 1

      do i = 1,m
      det = det* mat(i,i)

      end do

      !Telling the user values of modified upper triangular matrix1 and determinant value
      write(*,*)"Matrix A is:"
      do i = 1,m
      do j = 1,m
     write(*,'(f15.3,t5)',advance = 'no')mat(i,j)
      end do
      write(*,*)
      end do

      write(*,*)"The determinant is given as:",det
      close(1)
      end program pivotal_condense_method

