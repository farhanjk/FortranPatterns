!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Module for defining adapter
module adapter_module
    implicit none
    !An employee
    type, abstract :: employee
        integer :: happiness = 0
    contains
        procedure(showHappinessInterface), deferred :: showHappiness
    end type employee
    abstract interface
        subroutine showHappinessInterface(this)
            import employee
            class(employee), intent(in) :: this
        end subroutine showHappinessInterface
    end interface
    
    !Employee implementation
    type, extends(employee) :: concreteEmployee
    contains
        procedure :: showHappiness => showHappinessImpl
    end type concreteEmployee
    
    !Consultant (not an employee)
    type, abstract :: consultant
        integer :: smiles = 0
    contains
        procedure(showSmilesInterface), deferred :: showSmiles
    end type consultant
    abstract interface
        subroutine showSmilesInterface(this)
            import consultant
            class(consultant), intent(in) :: this
        end subroutine
    end interface

    !Consultant implementation
    type, extends(consultant) :: concreteConsultant
    contains
        procedure :: showSmiles => showSmilesImpl
    end type concreteConsultant
    
    !Consultant adapter paves path between consultant and employee
    type, extends(employee), abstract :: consultantAdapterIntermediary
    end type consultantAdapterIntermediary
    
    !Constructor for initializing the adapter
    interface consultantAdapter
         module procedure init_consultantAdapter
    end interface
    
    !Consultant adapter paves path between consultant and employee
    type, extends(consultantAdapterIntermediary) :: consultantAdapter
        class(consultant), allocatable :: c
    contains
        procedure :: showHappiness => showHappinessConsultantAdapterImpl
        procedure :: showSmiles => showSmilesConsultantAdapterImpl
    end type consultantAdapter
contains
        
    type(consultantAdapter) function init_consultantAdapter(c)
        class(consultant), intent(in) :: c
        allocate(init_consultantAdapter%c, source = c)
     end function
        
    subroutine showHappinessImpl(this)
        class(concreteEmployee), intent(in) :: this
        print *, this%happiness
    end subroutine showHappinessImpl
    
    subroutine showSmilesImpl(this)
        class(concreteConsultant), intent(in) :: this
        print *, this%smiles
    end subroutine
    
    subroutine showHappinessConsultantAdapterImpl(this)
        class(consultantAdapter), intent(in) :: this
        print *, this%c%smiles	
    end subroutine

    
    subroutine showSmilesConsultantAdapterImpl(this)
        class(consultantAdapter), intent(in) :: this
        print *, this%c%smiles	
    end subroutine
end module adapter_module