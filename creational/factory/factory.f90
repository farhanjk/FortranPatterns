!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!The shape factory
module factory
    
    !shape (base type), rectangle & circle (extends shape), fromShapeFactoryGetShape (main factory interface)
    public :: shape, fromShapeFactoryGetShape

    private

    !base type
    type, abstract :: shape
        character(100) :: value
    contains
        procedure(drawShape), deferred :: printShape
    end type
    !Defines interface for printShape
    abstract interface
        subroutine drawShape(this)
            import shape
            class(shape), intent(in) :: this
        end subroutine
    end interface

    !Concrete shape, implements printShape
    type, extends(shape) :: rectangle 
    contains
        procedure :: printShape => printRectangle
    end type
    
    !Concrete shape, implements printShape
    type, extends(shape) :: circle
    contains
        procedure :: printShape => printCircle
    end type

contains
    !prints value of the rectangle
    subroutine printRectangle(this)
        class(rectangle), intent(in) :: this
        print *, "This is a " // this%value
    end subroutine
    
    !prints value of the circle
    subroutine printCircle(this)
        class(circle), intent(in) :: this
        print *, "This is a " // this%value
    end subroutine

    !factory interface
    subroutine fromShapeFactoryGetShape(v,e)
        character(*), intent(in) :: v
        class(shape), allocatable, intent(out) :: e
        
        if (v .EQ. 'Rectangle') then
            allocate(e, source=rectangle("Rectangle"))
        else
            allocate(e, source=circle("Circle"))
        endif
    end subroutine fromShapeFactoryGetShape
    
end module factory
