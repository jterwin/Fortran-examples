PROGRAM fortran_paths

  CHARACTER (LEN=128) :: F, FN


  F = "/test.txt"
  FN = FULLNAME(F)

  WRITE(*,'(A)') ""
  WRITE(*,'(3(2x,A))') TRIM(F), "-->", TRIM(FN)


  F = "~/test.txt"
  FN = FULLNAME(F)

  WRITE(*,'(A)') ""
  WRITE(*,'(3(2x,A))') TRIM(F), "-->", TRIM(FN)


  F = "test.txt"
  FN = FULLNAME(F)

  WRITE(*,'(A)') ""
  WRITE(*,'(3(2x,A))') TRIM(F), "-->", TRIM(FN)

  F = "../test.txt"
  FN = FULLNAME(F)

  WRITE(*,'(A)') ""
  WRITE(*,'(3(2x,A))') TRIM(F), "-->", TRIM(FN)

CONTAINS


  !          This assumes C shell.
  !           Leave absolute path names unchanged.
  !           If name starts with ’~/’, replace tilde with home
  !           directory; otherwise prefix relative path name with
  !           path to current directory.
  CHARACTER (LEN=128) FUNCTION FULLNAME( NAME )
    CHARACTER (LEN=*) :: NAME
    CHARACTER (LEN=128) :: PREFIX

    IF ( NAME(1:1) .EQ. '/' ) THEN
       FULLNAME = NAME
    ELSE IF ( NAME(1:2) .EQ. '~/' ) THEN
       CALL GETENV( 'HOME', PREFIX )
       FULLNAME = TRIM(PREFIX) // NAME(2:LEN_TRIM(NAME))
    ELSE
       CALL GETCWD( PREFIX )
       FULLNAME = TRIM(PREFIX) // '/' // TRIM(NAME)
    ENDIF

    RETURN
  END FUNCTION FULLNAME


END PROGRAM fortran_paths
