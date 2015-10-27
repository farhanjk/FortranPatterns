!Copyright (c) 2015 M. Zapukhlyak
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!-----------------------------------------------------------------------
!Module car_state_module implements car example of state design pattern
!-----------------------------------------------------------------------
module car_state_module
   implicit none
   private ! all by default
   public :: abstract_car_state, state_car_is_off, state_car_is_on, state_car_is_moving
   public :: abstract_car, car

   ! abstract car
   ! no private data, used only to declare properly procedures passing reference to different class object
   type, abstract :: abstract_car
   contains
      procedure (abstract_car_procedure), deferred, pass :: set_state
   end type abstract_car

   ! abstract car state
   type, abstract :: abstract_car_state
   contains
      procedure (abstract_car_state_procedure), deferred, pass :: open_door
      procedure (abstract_car_state_procedure), deferred, pass :: open_window
      procedure (abstract_car_state_procedure), deferred, pass :: off
      procedure (abstract_car_state_procedure), deferred, pass :: on
      procedure (abstract_car_state_procedure), deferred, pass :: start
   end type abstract_car_state

   ! interface to set_state in abstract car
   abstract interface
      subroutine abstract_car_procedure(this, state)
         import  :: abstract_car_state
         import  :: abstract_car
         class(abstract_car), intent(inout) :: this
         class(abstract_car_state), intent(in) :: state
      end subroutine abstract_car_procedure
   end interface

   ! interface to operate in abstract state
   abstract interface
      subroutine abstract_car_state_procedure(this, car)
         import  :: abstract_car_state
         import  :: abstract_car
         class(abstract_car_state), intent(inout) :: this
         class(abstract_car), intent(inout) :: car
      end subroutine abstract_car_state_procedure
   end interface

   ! concrete states
   type, extends(abstract_car_state) :: state_car_is_off
   contains
      procedure, pass :: open_door => try_to_open_door_car_is_off
      procedure, pass :: open_window => try_to_open_window_car_is_off
      procedure, pass :: off => try_to_off_car_is_off
      procedure, pass :: on => try_to_on_car_is_off
      procedure, pass :: start => try_to_start_car_is_off
   end type state_car_is_off

   type, extends(abstract_car_state) :: state_car_is_on
   contains
      procedure, pass :: open_door => try_to_open_door_car_is_on
      procedure, pass :: open_window => try_to_open_window_car_is_on
      procedure, pass :: off => try_to_off_car_is_on
      procedure, pass :: on => try_to_on_car_is_on
      procedure, pass :: start => try_to_start_car_is_on
   end type state_car_is_on

   type, extends(abstract_car_state) :: state_car_is_moving
   contains
      procedure, pass :: open_door => try_to_open_door_car_is_moving
      procedure, pass :: open_window => try_to_open_window_car_is_moving
      procedure, pass :: off => try_to_off_car_is_moving
      procedure, pass :: on => try_to_on_car_is_moving
      procedure, pass :: start => try_to_start_car_is_moving
   end type state_car_is_moving

   type, extends(abstract_car_state) :: state_car_is_broken
   contains
      procedure, pass :: open_door => try_to_open_door_car_is_broken
      procedure, pass :: open_window => try_to_open_window_car_is_broken
      procedure, pass :: off => try_to_off_car_is_broken
      procedure, pass :: on => try_to_on_car_is_broken
      procedure, pass :: start => try_to_start_car_is_broken
   end type state_car_is_broken

   ! concrete car
   type, extends(abstract_car)  :: car
      class(abstract_car_state), allocatable, private :: current !< --- current state
   contains
      procedure, pass :: init => car_init
      procedure, pass :: on => car_on
      procedure, pass :: off => car_off
      procedure, pass :: start => car_start
      procedure, pass :: open_door => car_open_door
      procedure, pass :: open_window => car_open_window
      procedure, pass :: set_state => car_set_state
   end type car

contains
   !-----------------------------------------------------------------------
   !Subroutines operate_state_car_is_off
   !-----------------------------------------------------------------------
   subroutine  try_to_open_door_car_is_off(this, car)
      implicit none
      class(state_car_is_off), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      write(*,*) "The door is being opened ..."
      ! state doesn't change
   end subroutine try_to_open_door_car_is_off

   subroutine  try_to_open_window_car_is_off(this, car)
      implicit none
      class(state_car_is_off), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      write(*,*) "Can't open the window! Switch the car on!"
      ! state doesn't change
   end subroutine try_to_open_window_car_is_off

   subroutine  try_to_off_car_is_off(this, car)
      implicit none
      class(state_car_is_off), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      write(*,*) "Can't off the car! Switch the car on!"
      ! state doesn't change
   end subroutine try_to_off_car_is_off

   subroutine  try_to_on_car_is_off(this, car)
      implicit none
      class(state_car_is_off), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      class(abstract_car_state), allocatable :: state
      write(*,*) "The car is on!"
      ! do something and change state
      allocate(state, source=state_car_is_on())
      call car % set_state(state)
   end subroutine try_to_on_car_is_off

   subroutine  try_to_start_car_is_off(this, car)
      implicit none
      class(state_car_is_off), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      write(*,*) "Can't start the car! Switch the car on!"
      ! state doesn't change
   end subroutine try_to_start_car_is_off

   !-----------------------------------------------------------------------
   !Subroutines operate_state_car_is_on
   !-----------------------------------------------------------------------
   subroutine  try_to_open_door_car_is_on(this, car)
      implicit none
      class(state_car_is_on), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      write(*,*) "The door is being opened ..."
      ! state doesn't change
   end subroutine try_to_open_door_car_is_on

   subroutine  try_to_open_window_car_is_on(this, car)
      implicit none
      class(state_car_is_on), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      write(*,*) "The window is being opened ..."
      ! state doesn't change
   end subroutine try_to_open_window_car_is_on

   subroutine  try_to_off_car_is_on(this, car)
      implicit none
      class(state_car_is_on), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      class(abstract_car_state), allocatable :: state
      write(*,*) "The car is off!"
      ! do something and change state
      allocate(state, source=state_car_is_off())
      call car % set_state(state)
   end subroutine try_to_off_car_is_on

   subroutine  try_to_on_car_is_on(this, car)
      implicit none
      class(state_car_is_on), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      write(*,*) "The car is on already!"
      ! state doesn't change
   end subroutine try_to_on_car_is_on

   subroutine  try_to_start_car_is_on(this, car)
      implicit none
      class(state_car_is_on), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      class(abstract_car_state), allocatable :: state
      write(*,*) "The car is moving!"
      ! do something and change state
      allocate(state, source=state_car_is_moving())
      call car % set_state(state)
   end subroutine try_to_start_car_is_on

   !-----------------------------------------------------------------------
   !Subroutines operate_state_car_is_moving
   !-----------------------------------------------------------------------
   subroutine  try_to_open_door_car_is_moving(this, car)
      implicit none
      class(state_car_is_moving), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      write(*,*) "Can't open the door while moving!"
      ! state doesn't change
   end subroutine try_to_open_door_car_is_moving

   subroutine  try_to_open_window_car_is_moving(this, car)
      implicit none
      class(state_car_is_moving), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      write(*,*) "The window is being opened ..."
      ! state doesn't change
   end subroutine try_to_open_window_car_is_moving

   subroutine  try_to_off_car_is_moving(this, car)
      implicit none
      class(state_car_is_moving), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      class(abstract_car_state), allocatable :: state
      write(*,*) "The car is off!"
      ! do something and change state
      allocate(state, source=state_car_is_off())
      call car % set_state(state)
   end subroutine try_to_off_car_is_moving

   subroutine  try_to_on_car_is_moving(this, car)
      implicit none
      class(state_car_is_moving), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      class(abstract_car_state), allocatable :: state
      write(*,*) "Starter failure. The car is broken now!"
      ! do something and change state
      allocate(state, source=state_car_is_broken())
      call car % set_state(state)
   end subroutine try_to_on_car_is_moving

   subroutine  try_to_start_car_is_moving(this, car)
      implicit none
      class(state_car_is_moving), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      write(*,*) "The car is moving already!"
      ! state doesn't change
   end subroutine try_to_start_car_is_moving

   !-----------------------------------------------------------------------
   !Subroutines operate_state_car_is_broken
   !-----------------------------------------------------------------------
   subroutine  try_to_open_door_car_is_broken(this, car)
      implicit none
      class(state_car_is_broken), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      write(*,*) "The door is being opened ..."
      ! state doesn't change
   end subroutine try_to_open_door_car_is_broken

   subroutine  try_to_open_window_car_is_broken(this, car)
      implicit none
      class(state_car_is_broken), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      write(*,*) "The window is being opened ..."
      ! state doesn't change
   end subroutine try_to_open_window_car_is_broken

   subroutine  try_to_off_car_is_broken(this, car)
      implicit none
      class(state_car_is_broken), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      write(*,*) "The car is broken!"
      ! state doesn't change
   end subroutine try_to_off_car_is_broken

   subroutine  try_to_on_car_is_broken(this, car)
      implicit none
      class(state_car_is_broken), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      write(*,*) "The car is broken!"
      ! state doesn't change
   end subroutine try_to_on_car_is_broken

   subroutine  try_to_start_car_is_broken(this, car)
      implicit none
      class(state_car_is_broken), intent(inout) :: this
      class(abstract_car), intent(inout) :: car
      write(*,*) "The car is broken!"
      ! state doesn't change
   end subroutine try_to_start_car_is_broken

   !-----------------------------------------------------------------------
   !Subroutine car_init
   !-----------------------------------------------------------------------
   subroutine  car_init(this)
      implicit none
      class(car), intent(inout) :: this
      ! prepare and set default current state to state a
      write(*,*) "The car is off!"
      if(allocated(this % current)) deallocate(this % current)
      allocate(this % current, source=state_car_is_off())
   end subroutine car_init

   !-----------------------------------------------------------------------
   !Subroutine car_on
   !-----------------------------------------------------------------------
   subroutine  car_on(this)
      implicit none
      class(car), target, intent(inout) :: this
      call this % current % on(this)

   end subroutine car_on

   !-----------------------------------------------------------------------
   !Subroutine car_off
   !-----------------------------------------------------------------------
   subroutine  car_off(this)
      implicit none
      class(car), target, intent(inout) :: this
      call this % current % off(this)
   end subroutine car_off

   !-----------------------------------------------------------------------
   !Subroutine car_start
   !-----------------------------------------------------------------------
   subroutine  car_start(this)
      implicit none
      class(car), target, intent(inout) :: this
      call this % current % start(this)
   end subroutine car_start

   !-----------------------------------------------------------------------
   !Subroutine car_open_door
   !-----------------------------------------------------------------------
   subroutine  car_open_door(this)
      implicit none
      class(car), target, intent(inout) :: this
      call this % current % open_door(this)
   end subroutine car_open_door

   !-----------------------------------------------------------------------
   !Subroutine car_open_window
   !-----------------------------------------------------------------------
   subroutine  car_open_window(this)
      implicit none
      class(car), target, intent(inout) :: this
      call this % current % open_window(this)
   end subroutine car_open_window

   !-----------------------------------------------------------------------
   !Subroutine car_set_state
   !-----------------------------------------------------------------------
   subroutine  car_set_state(this, state)
      implicit none
      class(car), intent(inout) :: this
      class(abstract_car_state), intent(in) :: state
      ! detection if(this % current /= state) else do nothing!
      if(.not. same_type_as(state,this % current)) then
         ! this % current = state ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
         ! workaround
         if(allocated(this % current)) deallocate(this % current)
         allocate (this % current, source=state)
      endif
   end subroutine car_set_state

end module car_state_module


