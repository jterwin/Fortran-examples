PROGRAM test3
  IMPLICIT NONE

  INTEGER :: i, j, k, numrows, numcols
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: a
  CHARACTER(LEN=30) :: rowfmt
  INTEGER :: txtclock, binclock
  REAL    :: txttime, bintime

  numrows=5001
  numcols=762
  ALLOCATE(a(numrows,numcols))
  k=1
  DO i=1,SIZE(a,1)
    DO j=1,SIZE(a,2)
      a(i,j)=k
      k=k+1
    END DO
  END DO

  CALL tick(txtclock)
  WRITE(rowfmt,'(A,I4,A)') '(',numcols,'(1X,I6))'
  OPEN(UNIT=12, FILE="aoutput.txt", ACTION="write", STATUS="replace", &
       RECL=(7*numcols+10))
  DO i=1,numrows
    WRITE(12,FMT=rowfmt) (a(i,j), j=1,numcols)
  END DO
  CLOSE(UNIT=12)
  txttime = tock(txtclock)

  CALL tick(binclock)
  OPEN(UNIT=13, FILE="boutput.dat", ACTION="write", STATUS="replace", &
       FORM="unformatted")
  WRITE(13) a
  CLOSE(UNIT=13)
  bintime = tock(binclock)

  PRINT *, 'ASCII  time = ', txttime
  PRINT *, 'Binary time = ', bintime

CONTAINS

    SUBROUTINE tick(t)
        INTEGER, INTENT(OUT) :: t

        CALL system_clock(t)
    END SUBROUTINE tick

    ! returns time in seconds from now to time described by t
    REAL FUNCTION tock(t)
        INTEGER, INTENT(IN) :: t
        INTEGER :: now, clock_rate

        call system_clock(now,clock_rate)

        tock = real(now - t)/real(clock_rate)
    END FUNCTION tock
END PROGRAM test3
