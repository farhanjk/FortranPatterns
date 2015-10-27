!Copyright (c) 2015 zmiimz
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!-----------------------------------------------------------------------
!Module sort_strategy_module
!-----------------------------------------------------------------------

module sort_strategy_module

   public :: strategy, quicksort_strategy, shellsort_strategy


   type, abstract :: strategy
   contains
      procedure (strategy_procedure), deferred, pass :: sort
   end type strategy

   abstract interface
      subroutine strategy_procedure(this, data)
         import  :: strategy
         class(Strategy), intent(inout) :: this
         integer, dimension(:), intent(inout) :: data
      end subroutine strategy_procedure
   end interface

   type, extends(strategy) :: quicksort_strategy
   contains
      procedure, pass :: sort=> quicksort
   end type quicksort_strategy

   type, extends(strategy) :: shellsort_strategy
   contains
      procedure, pass :: sort=> shellsort
   end type shellsort_strategy


contains

   subroutine quicksort(this, data)
      class(quicksort_strategy), intent(inout) :: this
      integer, dimension(:), intent(inout) :: data
      write(*,*) "Implementation of quicksort"
   end subroutine quicksort


   subroutine shellsort(this, data)
      class(shellsort_strategy), intent(inout) :: this
      integer, dimension(:), intent(inout) :: data
      write(*,*) "Implementation of shellsort"
   end subroutine shellsort

end module sort_strategy_module


!-----------------------------------------------------------------------
!Module UserStrategy_module
!-----------------------------------------------------------------------
module user_strategy_module
   use sort_strategy_module
   implicit none
   private ! all by default
   public :: user_strategy

   type user_strategy
      class(strategy), allocatable :: sorter
      integer, dimension(:), allocatable :: user_data
   contains
      procedure, pass :: init
      procedure, pass :: change_strategy
      procedure, pass :: sort
   end type user_strategy


contains

   !-----------------------------------------------------------------------
   !Subroutine init
   !-----------------------------------------------------------------------
   subroutine  init(this, s, d)
      implicit none

      class(user_strategy), intent(inout) :: this
      class(strategy), intent(in) :: s
      integer, dimension(:), allocatable :: d

      allocate(this % sorter, source = s)

      ! allocate(this % user_data, source = d) ! bug in gfortran 5.2.1 "Array specification required in ALLOCATE statement"
      allocate(this % user_data(size(d)), source = d) ! workaround
   end subroutine init

   !-----------------------------------------------------------------------
   !Subroutine change_strategy
   !-----------------------------------------------------------------------
   subroutine  change_strategy(this, s)
      implicit none
      class(user_strategy), intent(inout) :: this
      class(strategy), intent(in) :: s
      ! overloaded assignment?
      ! Assignment to an allocatable polymorphic variable at (1) is not yet supported
      ! not implemented yet [F2008] gfortran 5.2.1, ifort 16.0
      ! this % sorter = s

      ! workaround?
      deallocate(this % sorter)
      allocate(this % sorter, source = s)
   end subroutine change_strategy

   !-----------------------------------------------------------------------
   !Subroutine sort
   !-----------------------------------------------------------------------
   subroutine  sort(this)
      implicit none
      class(user_strategy), intent(inout) :: this
      call this % sorter % sort(this % user_data)
   end subroutine sort

end module user_strategy_module

