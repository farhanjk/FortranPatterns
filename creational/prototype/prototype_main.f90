!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Main program
program prototype_main
    use prototype_module
    implicit none
    !Create first object
    type(shape) :: shape1
    class(shape), allocatable :: shape2
    !Load resources
    shape1%not_expensive = 40
    shape1%expensive = 80
    !Confirm the loaded values
    print *, shape1%expensive
    print *, shape1%not_expensive
    !Clone shape1 to shape2
    allocate(shape2, source=shape1%clone())
    !Confirm that the expensive resource is referenced
    print *, shape2%expensive
    print *, shape2%not_expensive
    deallocate(shape2)
end program prototype_main