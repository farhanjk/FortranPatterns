!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Main Program
program composite_main
    use composite_module
    implicit none
    
    !Employees
    type(employee) :: manager
    type(employee) :: assistant_manager1
    type(employee) :: assistant_manager2
    type(employee) :: regular_employee

    manager = employee('Khan')
    assistant_manager1 = employee('Jamal')
    assistant_manager2 = employee('Farhan')
    regular_employee = employee('Zinnia')

    !Add subordinates and validate by printing them
    call manager%add_subordinate(assistant_manager1)	
    call manager%add_subordinate(assistant_manager2)	
    call manager%print_subordinates
    call assistant_manager1%add_subordinate(regular_employee)
    call assistant_manager1%print_subordinates
end program composite_main