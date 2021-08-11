!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Module for defining bridge
module bridge_module
    implicit none
    
    !Bridge
    type, abstract :: drawAPI
    contains
        procedure(drawCircleInterface), deferred :: drawCircle
    end type drawAPI
    abstract interface
        subroutine drawCircleInterface(this)
            import drawAPI
            class(drawAPI), intent(in) :: this
        end subroutine drawCircleInterface
    end interface
    
    !Bridge concrete classes
    type, extends(drawAPI) :: redCircleAPI
    contains
        procedure :: drawCircle => drawRedCircleImpl
    end type redCircleAPI

    type, extends(drawAPI) :: blueCircleAPI
    contains
        procedure :: drawCircle => drawBlueCircleImpl
    end type blueCircleAPI

    !Classes using the bridge
    type, abstract :: shape 
        class(drawAPI), allocatable :: d
    contains
        procedure(drawInterface), deferred :: draw
    end type shape
    abstract interface
        subroutine drawInterface(this)
            import shape
            class(shape), intent(in) :: this
        end subroutine drawInterface
    end interface
    
    type, extends(shape) :: circleShape
    contains
        procedure :: draw => drawCircleShape
    end type circleShape
    
    !Constructor for initializing
    interface circleShape
         module procedure init_circleShape
    end interface
    
contains
    
    type(circleShape) function init_circleShape(c)
        class(drawAPI), intent(in) :: c
        allocate(init_circleShape%d, source = c)
     end function
    
    subroutine drawRedCircleImpl(this)
        class(redCircleAPI), intent(in) :: this
        print *, "Draw red circle"
    end subroutine drawRedCircleImpl

    subroutine drawBlueCircleImpl(this)
        class(blueCircleAPI), intent(in) :: this
        print *, "Draw blue circle"
    end subroutine drawBlueCircleImpl
    
    subroutine drawCircleShape(this)
        class(circleShape), intent(in) :: this
        call this%d%drawCircle
    end subroutine drawCircleShape
end module bridge_module