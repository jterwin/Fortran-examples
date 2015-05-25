program hello_world
  
  implicit none

  integer :: i

  
  do i = 1,10

     write(*,'(A,I0,A)') "Hello world ", i, "!"

  end do


  

end program hello_world
