!Main program demonstrating facade usage
program facade_main
  use facade_module
  implicit none

  type(home_theater_facade) :: theater

  print '(A)', 'Getting ready to watch a movie...'
  call theater%watch_movie('The Fortran Adventure')
  print '(A)', 'Movie finished, shutting everything down.'
  call theater%end_movie()

end program facade_main
