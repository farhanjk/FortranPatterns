!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Provides singleton functionality
module singleton
	implicit none
	public :: get_singleton_value, construct_singleton, dispose_singleton, set_singleton_value
	private
	
	type singleton_type
		private
		integer :: value
	end type singleton_type
	
	type(singleton_type), pointer :: singleton_instance => null()
contains
	subroutine construct_singleton()
		if (.not. associated(singleton_instance)) then
			allocate(singleton_instance)
			singleton_instance%value=0
		end if
	end subroutine construct_singleton
	
	subroutine dispose_singleton()
		deallocate(singleton_instance)	
	end subroutine  dispose_singleton
	
	integer function get_singleton_value()
		if (associated(singleton_instance)) then
			get_singleton_value = singleton_instance%value
		endif
	end function get_singleton_value
	
	subroutine set_singleton_value(v)
		integer, intent(in) :: v
		if (associated(singleton_instance)) then
			singleton_instance%value = v
		endif
	end subroutine set_singleton_value
	
end module singleton