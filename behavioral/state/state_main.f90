!-----------------------------------------------------------------------
!Main program test_car
!-----------------------------------------------------------------------
program    test_car
   use car_state_module
   implicit none
   type(car) :: mycar

   call mycar % init() ! set to off
   write(*,*)"------------------------------------------------------------"
   call mycar % off() ! try to off
   call mycar % open_window()
   call mycar % open_door()
   write(*,*)"------------------------------------------------------------"
   call mycar % on() ! try to on
   call mycar % open_window()
   call mycar % open_door()
   write(*,*)"------------------------------------------------------------"
   call mycar % start() ! try to move
   call mycar % open_window()
   call mycar % open_door()
   write(*,*)"------------------------------------------------------------"
   call mycar % on() !  try to on -> set to broken
   call mycar % off()
   call mycar % start()
   call mycar % open_window()
   call mycar % open_door()

end program test_car
