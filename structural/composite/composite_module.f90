!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Composite pattern implementation
module composite_module
    implicit none
        
    !employee contains list of employees
    type employee
        character(30) :: name
        type(employee), pointer :: next => null()
        type(employee), pointer :: list=>null(), current=>null(), previous=>null()
        integer :: N = 0
        contains
            procedure :: print_employee_name => print_employee_name_impl
            procedure :: add_subordinate => add_subordinate_impl
            procedure :: remove_last_subordinate => remove_last_subordinate_impl
            procedure :: print_subordinates => print_subordinates_impl
    end type employee
    
    !Constructor for initializing the employee
    interface employee
         module procedure init_employee
    end interface
    
contains
    
    type(employee) function init_employee(this)
        class(employee), intent(inout) :: this
        nullify(init_employee%list)   ! Initialize list to point to no target.
    end function
    
    subroutine print_employee_name_impl(this)
        class(employee), intent(in) :: this
        print *, this%name
    end subroutine print_employee_name_impl
    
    subroutine add_subordinate_impl(this, new_employee)
        class(employee), intent(inout) :: this
        type(employee), intent(in) :: new_employee

        ! Add the 1st element as a special case.
       if (.not. associated(this%list)) then
           allocate(this%list)
          this%list = new_employee
          nullify(this%list%next)
          this%current => this%list
      else
          allocate(this%current%next)
         this%current%next = new_employee
         nullify(this%current%next%next)
         this%current => this%current%next
     end if
       
    end subroutine add_subordinate_impl
    
    subroutine remove_last_subordinate_impl(this)
        class(employee), intent(inout) :: this
        this%current = this%current%previous
    end subroutine remove_last_subordinate_impl
    
    subroutine print_subordinates_impl(this)
        class(employee), intent(in) :: this
        ! Output the list

           type(employee), pointer :: temp_cur
          type(employee), pointer :: temp_prev

              print *, 'List of subordinates:'
           temp_cur => this%list
           do while ( associated(temp_cur) )
              print *, temp_cur%name
              temp_prev => temp_cur
              temp_cur => temp_cur%next
           end do
     end subroutine print_subordinates_impl
    
end module composite_module