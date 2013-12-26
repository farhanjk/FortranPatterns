!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

module abstractfactory
	implicit none
	public shape, color, theabstractfactory, factoryProducer
	private
	!Interface for shapes
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
	
	!Concrete type rectange implements shape
	type, extends(shape) :: rectangle
	contains
		procedure :: draw=>drawRectangle
	end type

	!Concrete type circle implements shape
	type, extends(shape) :: circle
	contains
		procedure :: draw=>drawCircle
	end type

	!Interface for colors
	type, abstract :: color
	contains
		procedure(fillInterface), deferred :: fill
	end type color
	abstract interface
		subroutine fillInterface(this)
			import color
			class(color), intent(in) :: this
		end subroutine
	end interface
	
	!Concrete class red implementing color
	type, extends(color) :: red
	contains
		procedure :: fill=>fillRed
	end type

	!Concrete class blue implementing color
	type, extends(color) :: blue
	contains
		procedure :: fill=>fillBlue
	end type
	
	!Abstract class to get shape and color
	type, abstract :: theAbstractFactory
	contains
		procedure(getColorInterface), deferred :: getColor
		procedure(getShapeInterface), deferred :: getShape
	end type
	abstract interface
		subroutine getColorInterface(this, c, r)
			import theAbstractFactory
			import color
			class(theAbstractFactory), intent(in) :: this
			character(*), intent(in) :: c
			class(color), allocatable, intent(out) :: r
		end subroutine
		subroutine getShapeInterface(this, s, r)
			import theAbstractFactory
			import shape
			class(theAbstractFactory), intent(in) :: this
			character(*), intent(in) :: s
			class(shape), allocatable, intent(out) :: r
		end subroutine
	end interface
	
	!Concrete implementation of theAbstractFactory 
	type, extends(theAbstractFactory) :: shapefactory
	contains
		procedure :: getShape => getShapeImpl
		procedure :: getColor => getColorEmpty
	end type

	type, extends(theAbstractFactory) :: colorfactory
	contains
		procedure :: getShape => getShapeEmpty
		procedure :: getColor => getColorImpl
	end type
	
	!Factory producer
	type factoryProducer
	contains
		procedure :: getFactory => getFactoryImpl
	end type
contains
	subroutine drawRectangle(this)
		class(rectangle), intent(in) :: this
		print *, "This is a rectangle"
	end subroutine drawRectangle
	
	subroutine drawCircle(this)
		class(circle), intent(in) :: this
		print *, "This is a circle"
	end subroutine drawCircle

	subroutine fillRed(this)
		class(red), intent(in) :: this
		print *, "Filling red color"
	end subroutine fillRed

	subroutine fillBlue(this)
		class(blue), intent(in) :: this
		print *, "Filling blue color"
	end subroutine fillBlue
	
	subroutine getShapeImpl(this, s, r)
		class(shapeFactory), intent(in) :: this
		character(*), intent(in) :: s
		class(shape), allocatable, intent(out) :: r
		if (s .EQ. 'Rectangle') then
			allocate(r, source=rectangle())
		else
			allocate(r, source=circle())
		endif
	end subroutine getShapeImpl

	subroutine getColorImpl(this, c, r)
		class(colorFactory), intent(in) :: this
		character(*), intent(in) :: c
		class(color), allocatable, intent(out) :: r
		if (c .EQ. 'Blue') then
			allocate(r, source=blue())
		else
			allocate(r, source=red())
		endif
	end subroutine getColorImpl
	
	subroutine getShapeEmpty(this, s, r)
		class(colorFactory), intent(in) :: this
		character(*), intent(in) :: s
		class(shape), allocatable, intent(out) :: r
	end subroutine getShapeEmpty
	
	subroutine getColorEmpty(this, c, r)
		class(shapeFactory), intent(in) :: this
		character(*), intent(in) :: c
		class(color), allocatable, intent(out) :: r
	end subroutine getColorEmpty

	subroutine getFactoryImpl(this, s, r)
		class(factoryProducer), intent(in) :: this
		character(*), intent(in) :: s
		class(theAbstractFactory), allocatable, intent(out) :: r
		if (s .EQ. 'Shape') then
			allocate(r, source=shapeFactory())
		else
			allocate(r, source=colorFactory())
		endif
	end subroutine getFactoryImpl

end module abstractfactory