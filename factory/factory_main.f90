!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Main Program
!To compile:
!$ gfortran -c factory.f90
!$ gfortran factory_main.f90 factory.o -o factory_main
program factory_main
	!Shape Factory module
	use factory
	!Alocatable shape address
	class(shape), allocatable :: shapeInstance

	!!! allocate(shapeInstance, source=shape("shape")) !!!
	!!! Error: Can't construct ABSTRACT type 'shape' !!!
	
	!Get a rectangle and verify by printing
	call fromShapeFactoryGetShape("Rectangle", shapeInstance)
	call shapeInstance%printShape
	deallocate(shapeInstance)
	!Get a circle and verify by printing
	call fromShapeFactoryGetShape("Circle", shapeInstance)
	call shapeInstance%printShape
	deallocate(shapeInstance)
end program factory_main