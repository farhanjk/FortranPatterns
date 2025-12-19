!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Main Program
program bridge_main
    use bridge_module
    implicit none

    !shape to draw
    class(shape), allocatable :: s1
    !Bridge implementation instances
    type(redCircleAPI) :: redCircle
    type(blueCircleAPI) :: blueCircle

    !passing the drawable custom api
    allocate(s1, source=circleShape(redCircle))
    call s1%draw
    deallocate(s1)
    allocate(s1, source=circleShape(blueCircle))
    call s1%draw
    deallocate(s1)
end program bridge_main