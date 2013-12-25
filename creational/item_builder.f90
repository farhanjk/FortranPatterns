!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Contains definition of item and builder to build the item
module item_builder
	implicit none

	!client will have access to item but cannot instantiate it
	type, public, abstract :: item
		integer, private :: value
	contains
		procedure(getValueInterface), deferred :: getValue
	end type 
	abstract interface 
		integer function getValueInterface(this)
			import item
			class(item), intent(in) :: this
		end function
	end interface
	
	!Client does not know about any implementation of item
	type, private, extends(item) :: concreteItem
	contains
		procedure :: getValue => getValueImpl
	end type

contains
	!returns item's value
	integer function getValueImpl(this)
		class(concreteitem), intent(in) :: this
		getValueImpl = this%value
	end function
		
	!builds the item
	type(concreteItem) function buildItem(value)
		integer, intent(in) :: value
		type(concreteItem) :: itemInstance
		itemInstance%value = value
		buildItem = itemInstance
	end function
end module item_builder