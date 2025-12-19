!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Main Program
program decorator_main
    use decorator_module
    implicit none
    
     class(shape), allocatable :: circle_shape
    class(shape), allocatable :: rectangle_shape
     class(shape), allocatable :: redCircle_shape
     class(shape), allocatable :: redRectangle_shape
    type(circle) :: theCircle
     
     allocate(circle_shape, source=theCircle)
     print *, 'Circle with normal border:'
     call circle_shape%draw
     allocate(redCircle_shape, source=redShapeDecorator(circle_shape))
    print *, 'Circle with red border:'
    call redCircle_shape%draw
    allocate(rectangle_shape, source=rectangle())
    allocate(redRectangle_shape, source = redShapeDecorator(rectangle_shape))
    print *, 'Rectangle with red border:'
    call redRectangle_shape%draw
    
     deallocate(circle_shape)
     deallocate(redCircle_shape)
     deallocate(redRectangle_shape)
    
end program decorator_main