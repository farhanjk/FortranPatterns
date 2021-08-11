!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Module providing implementation of prototype pattern
module prototype_module
    implicit none
    
    public :: shape
    
    type shape
        !Assuming not_expensive value can be set in no time!
        integer :: not_expensive = 0
        !Assuming "expensive" needs a lot of time to set!
        integer :: expensive = 0
    contains
        procedure :: clone => cloneImpl
    end type shape
contains
    function cloneImpl(this) result(out)
        class(shape), intent(in) :: this
        type(shape) :: out
        out%expensive = this%expensive
    end function cloneImpl
end module prototype_module