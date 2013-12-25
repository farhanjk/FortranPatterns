!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Main program
program abstractfactory_main
	use abstractfactory
	!Allocatable for the shape and color factory
	class(theAbstractFactory), allocatable :: theFactory
	!Allocatable for shape instance
	class(shape), allocatable :: shapeInstance
	!Allocatable for color instance
	class(color), allocatable :: colorInstance

	!The factory producer
	type(factoryProducer) :: factoryProducerInstance

	!Get shape factory
	call factoryProducerInstance%getFactory('Shape', theFactory)
	!Get rectangle from the factory
	call theFactory%getShape('Rectangle', shapeInstance)
	!draw the rectangle
	call shapeInstance%draw
	deallocate(shapeInstance)

	!Get color factory
	call factoryProducerInstance%getFactory('Color', theFactory)
	!Get blue from the color factory
	call theFactory%getColor('Blue', colorInstance)
	!Fill the blue color
	call colorInstance%fill
	deallocate(colorInstance)

	deallocate(theFactory)
end program abstractfactory_main