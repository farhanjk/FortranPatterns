!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!The shape factory
module factory
	
	!shape (base type), rectangle & circle (extends shape), fromShapeFactoryGetShape (main factory interface)
	public :: shape, rectangle, circle, fromShapeFactoryGetShape

	private

	!base type
	type shape
		character(100) :: value
	contains
		procedure :: printShape => drawShape
	end type

	type, extends(shape) :: rectangle 
	end type
	
	type, extends(shape) :: circle
	end type

contains
	!prints value of the shape
	subroutine drawShape(v)
		class(shape), intent(in) :: v
		print *, v%value
	end subroutine
	
	!factory interface
	subroutine fromShapeFactoryGetShape(v,e)
		character(*), intent(in) :: v
		class(shape), allocatable, intent(out) :: e
		
		if (v .EQ. 'Rectangle') then
			allocate(e, source=rectangle("This shape is rectangle"))
		else
			allocate(e, source=circle("This shape is circle"))
		endif
	end subroutine fromShapeFactoryGetShape
	
end module factory
