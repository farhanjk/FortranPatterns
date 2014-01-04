!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Module for defining decorator
module decorator_module
	implicit none
	
	type, abstract :: shape
	contains
		procedure(drawInterface), deferred :: draw
	end type shape
	abstract interface
		subroutine drawInterface(this)
			import shape
			class(shape), intent(in) :: this
		end subroutine 
	end interface
	
	type, extends(shape) :: rectangle
	contains
		procedure :: draw => drawRectangle
	end type rectangle
	
	type, extends(shape) :: circle
	contains
		procedure :: draw => drawCircle
	end type circle

	type, extends(shape), abstract :: shapeDecorator
	end type shapeDecorator
	
	type, extends(shapeDecorator) :: redShapeDecorator
		class(shape), pointer :: decoratedShape
	contains
		procedure :: draw => drawWithRedShapeDecorator
	end type redShapeDecorator
	
contains
	
	subroutine drawRectangle(this)
		class(rectangle), intent(in) :: this
		print *, "Draw rectangle"
	end subroutine 
	
	subroutine drawCircle(this)
		class(circle), intent(in) :: this
		print *, "Draw circle"
	end subroutine 

	subroutine drawWithRedShapeDecorator(this)
		class(redShapeDecorator), intent(in) :: this
		call this%decoratedShape%draw
		print *, "Red Border"
	end subroutine 

end module decorator_module