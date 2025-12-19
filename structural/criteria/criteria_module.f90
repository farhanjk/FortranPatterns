!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Module providing implementation of criteria pattern
module criteria_module
    implicit none
    
    !Person type
    type, public :: person
        character(10) :: name
        character(1) :: gender
        character(1) :: maritalstatus
    end type person
    
    !Criteria type with meetCriteria interface
    type, abstract :: criteria
    contains
        procedure(meetCriteriaInterface), deferred :: meetCriteria
    end type
    !Interface defination for meetCriteria
    abstract interface
        subroutine meetCriteriaInterface(this, personArray, outPersonArray)
            import criteria
            import person
            class(criteria), intent(in) :: this
            type(person), dimension (:), allocatable, intent(inout) :: personArray(:)
            type(person), dimension (:), allocatable, intent(inout) :: outPersonArray(:)
        end subroutine meetCriteriaInterface
    end interface
    
    type, extends(criteria) :: maleCriteria
    contains
        procedure :: meetCriteria => meetMaleCriteria
    end type maleCriteria
    
    type, extends(criteria) :: femaleCriteria
    contains
        procedure :: meetCriteria => meetFemaleCriteria
    end type femaleCriteria

    type, extends(criteria) :: singleCriteria
    contains
        procedure :: meetCriteria => meetSingleCriteria
    end type singleCriteria

    type, extends(criteria) :: AndCriteria
        class(criteria), allocatable :: c1
        class(criteria), allocatable :: c2
    contains
        procedure :: meetCriteria => meetAndCriteria
    end type AndCriteria

    type, extends(criteria) :: OrCriteria
        class(criteria), allocatable :: c1
        class(criteria), allocatable :: c2
    contains
        procedure :: meetCriteria => meetOrCriteria
    end type OrCriteria

contains

    !Genralized method to dynamically increase the size of array and put value at pos.
    subroutine putInArray(array, pos, value)
        type(person), dimension (:), allocatable, intent(inout) :: array(:)
        integer :: pos
        type(person), intent(in) :: value
        type(person), dimension (:), allocatable :: tempArray(:)
        
        allocate(tempArray(pos))
        tempArray(1:pos) = array
        deallocate(array)
        call move_alloc(tempArray, array)
        array(pos) = value
    end subroutine putInArray
    
    subroutine meetMaleCriteria(this, personArray, outPersonArray)
        class(maleCriteria), intent(in) :: this
        type(person), dimension (:), allocatable, intent(inout) :: personArray(:)
        type(person), dimension (:), allocatable, intent(inout) :: outPersonArray(:)
        integer :: j, k
        !initializing outPersonArray to avoid segmentation fault
        if (.not. allocated(outPersonArray)) then
            allocate(outPersonArray(1))
        endif
        k = 0
        do j=1,size(personArray)
            if (personArray(j)%gender .EQ. 'm') then
                k = k + 1
                call putInArray(outPersonArray, k, personArray(j))
            end if
        enddo
    end subroutine meetMaleCriteria
    
    subroutine meetFemaleCriteria(this, personArray, outPersonArray)
        class(femaleCriteria), intent(in) :: this
        type(person), dimension (:), allocatable, intent(inout) :: personArray(:)
        type(person), dimension (:), allocatable, intent(inout) :: outPersonArray(:)
        integer :: j, k

        if (.not. allocated(outPersonArray)) then
            allocate(outPersonArray(1))
        endif
        k = 0
        do j=1,size(personArray)
            if (personArray(j)%gender .EQ. 'f') then
                k = k + 1
                call putInArray(outPersonArray, k, personArray(j))
            end if
        enddo
    end subroutine meetFemaleCriteria

    subroutine meetSingleCriteria(this, personArray, outPersonArray)
        class(singleCriteria), intent(in) :: this
        type(person), dimension (:), allocatable, intent(inout) :: personArray(:)
        type(person), dimension (:), allocatable, intent(inout) :: outPersonArray(:)
        integer :: j, k

        if (.not. allocated(outPersonArray)) then
            allocate(outPersonArray(1))
        endif
        k = 0
        do j=1,size(personArray)
            if (personArray(j)%maritalstatus .EQ. 's') then
                k = k + 1
                call putInArray(outPersonArray, k, personArray(j))
            end if
        enddo
    end subroutine meetSingleCriteria

    subroutine meetAndCriteria(this, personArray, outPersonArray)
        class(AndCriteria), intent(in) :: this
        type(person), dimension (:), allocatable, intent(inout) :: personArray(:)
        type(person), dimension (:), allocatable, intent(inout) :: outPersonArray(:)
        type(person), dimension (:), allocatable :: tempPersonArray(:)
        allocate(tempPersonArray(1))
        !First criteria (output in tempPersonArray)
        call this%c1%meetCriteria(personArray, tempPersonArray)
        if (.not. allocated(outPersonArray)) then
            allocate(outPersonArray(1))
        endif
        !Apply second criteria to tempPersonArray
        call this%c2%meetCriteria(temppersonArray, outPersonArray)
    end subroutine meetAndCriteria

    subroutine meetOrCriteria(this, personArray, outPersonArray)
        class(OrCriteria), intent(in) :: this
        type(person), dimension (:), allocatable, intent(inout) :: personArray(:)
        type(person), dimension (:), allocatable, intent(inout) :: outPersonArray(:)
        type(person), dimension (:), allocatable :: tempPersonArray1(:)
        type(person), dimension (:), allocatable :: tempPersonArray2(:)
        integer :: i, j
        logical :: found = .false.

        allocate(tempPersonArray1(1))
        allocate(tempPersonArray2(1))
        
        !Apply the two criterias separately
        call this%c1%meetCriteria(personArray, tempPersonArray1)
        call this%c2%meetCriteria(personArray, tempPersonArray2)
        
        if (.not. allocated(outPersonArray)) then
            allocate(outPersonArray(1))
        endif

        do i = 1, size(tempPersonArray1), 1
            call putInArray(outPersonArray, i, tempPersonArray1(i))
        end do

        do i = 1, size(tempPersonArray2), 1
            found = .false.
            do j=1, size(outPersonArray), 1
                if (outPersonArray(j)%name==tempPersonArray2(i)%name) then
                    found = .true.
                    exit
                endif
            end do
            if (found .eqv. .false.) then
                call putInArray(outPersonArray, size(outpersonArray)+1, tempPersonArray2(i))
            endif
        end do
    end subroutine meetOrCriteria
    
    !Generalized method to print a person array
    subroutine printPersonArray(personArray)
        type(person), dimension (:), allocatable, intent(in) :: personArray(:)
        integer :: i
        do i = 1, size(personArray), 1
            if (.not. personArray(i)%name .eq. '') then
                print *, 'Name: '//personArray(i)%name//', Gender: '//personArray(i)%gender//&
                         ', Marital Status: '//personArray(i)%maritalstatus
            end if
        end do
    end subroutine printPersonArray

end module criteria_module