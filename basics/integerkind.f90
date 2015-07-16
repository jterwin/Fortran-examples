program integerkinds
    use iso_fortran_env
    implicit none

    integer :: i
    integer(kind=int8)  :: i8
    integer(kind=int16) :: i16
    integer(kind=int32) :: i32
    integer(kind=int64) :: i64

    integer(kind=selected_int_kind(6)) :: j6
    integer(kind=selected_int_kind(15)):: j15

    print *,'Default:'
    print *, huge(i)
    print *,'Int8:'
    print *, huge(i8)
    print *,'Int16:'
    print *, huge(i16)
    print *,'Int32:'
    print *, huge(i32)
    print *,'Int64:'
    print *, huge(i64)

    print *,''

    print *,'Selected Integer Kind 6:'
    print *, huge(j6)

    print *,'Selected Integer Kind 15:'
    print *, huge(j15)

    do i=1,30
       write(*,*) "i=",i," gives ",selected_int_kind(i)
    enddo

end program integerkinds
