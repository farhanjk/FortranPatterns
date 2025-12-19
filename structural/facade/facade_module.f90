!Copyright (c) 2024 OpenAI
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Module demonstrating the Facade pattern
module facade_module
  implicit none

  type :: amplifier
  contains
    procedure :: on => amplifier_on
    procedure :: off => amplifier_off
    procedure :: set_volume => amplifier_set_volume
  end type amplifier

  type :: dvd_player
  contains
    procedure :: on => dvd_on
    procedure :: off => dvd_off
    procedure :: play => dvd_play
  end type dvd_player

  type :: popcorn_maker
  contains
    procedure :: on => popcorn_on
    procedure :: off => popcorn_off
    procedure :: pop => popcorn_pop
  end type popcorn_maker

  ! Facade that coordinates the subsystems
  type :: home_theater_facade
     type(amplifier) :: amp
     type(dvd_player) :: dvd
     type(popcorn_maker) :: popcorn
  contains
     procedure :: watch_movie
     procedure :: end_movie
  end type home_theater_facade

contains

  subroutine amplifier_on(this)
    class(amplifier), intent(in) :: this
    print '(A)', 'Turning on the amplifier'
  end subroutine amplifier_on

  subroutine amplifier_off(this)
    class(amplifier), intent(in) :: this
    print '(A)', 'Turning off the amplifier'
  end subroutine amplifier_off

  subroutine amplifier_set_volume(this, level)
    class(amplifier), intent(in) :: this
    integer, intent(in) :: level
    write (*, '(A,I0)') 'Setting amplifier volume to ', level
  end subroutine amplifier_set_volume

  subroutine dvd_on(this)
    class(dvd_player), intent(in) :: this
    print '(A)', 'Starting the DVD player'
  end subroutine dvd_on

  subroutine dvd_off(this)
    class(dvd_player), intent(in) :: this
    print '(A)', 'Stopping the DVD player'
  end subroutine dvd_off

  subroutine dvd_play(this, movie)
    class(dvd_player), intent(in) :: this
    character(len=*), intent(in) :: movie
    write (*, '(A,A)') 'Playing movie: ', trim(movie)
  end subroutine dvd_play

  subroutine popcorn_on(this)
    class(popcorn_maker), intent(in) :: this
    print '(A)', 'Heating up the popcorn maker'
  end subroutine popcorn_on

  subroutine popcorn_off(this)
    class(popcorn_maker), intent(in) :: this
    print '(A)', 'Cooling down the popcorn maker'
  end subroutine popcorn_off

  subroutine popcorn_pop(this)
    class(popcorn_maker), intent(in) :: this
    print '(A)', 'Popping popcorn'
  end subroutine popcorn_pop

  subroutine watch_movie(this, movie)
    class(home_theater_facade), intent(in) :: this
    character(len=*), intent(in) :: movie

    call this%popcorn%on()
    call this%popcorn%pop()
    call this%amp%on()
    call this%amp%set_volume(7)
    call this%dvd%on()
    call this%dvd%play(movie)
  end subroutine watch_movie

  subroutine end_movie(this)
    class(home_theater_facade), intent(in) :: this
    call this%dvd%off()
    call this%amp%off()
    call this%popcorn%off()
  end subroutine end_movie

end module facade_module
