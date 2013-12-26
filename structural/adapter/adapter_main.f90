!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Main Program
program adapter_main
	use adapter_module
	
	class(employee), allocatable :: e1
	class(employee), allocatable :: e2
	class(employee), allocatable :: e3
	class(consultant), allocatable :: c1
	type(consultantAdapter) :: ca
	
	!2 employees and 1 consultant	
	allocate(e1, source = concreteEmployee(10))
	allocate(e2, source = concreteEmployee(20))
	allocate(c1, source = concreteConsultant(30))
	
	allocate(e3, source = e1)
	call e3%showHappiness()
	deallocate(e3)

	allocate(e3, source = e2)
	call e3%showHappiness()
	deallocate(e3)

	!consultant adapter acts as an employee
	ca = consultantAdapter(c1)
	allocate(e3, source =ca)
	call e3%showHappiness()
	deallocate(e3)
	  
end program adapter_main