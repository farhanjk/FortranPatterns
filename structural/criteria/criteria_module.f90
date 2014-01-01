!TODO: this needs to be complete and is not in usable format for now

module criteria_module
	implicit none
	
	type, public :: person
		character(20) :: name
		character(1) :: gender
		character(1) :: maritalstatus
	end type person
	
	type, abstract :: criteria
	contains
		procedure(meetCriteriaInterface), deferred :: meetCriteria
	end type
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

	subroutine meetMaleCriteria(this, personArray, outPersonArray)
		class(maleCriteria), intent(in) :: this
		type(person), dimension (:), allocatable, intent(inout) :: personArray(:)
		type(person), dimension (:), allocatable, intent(inout) :: outPersonArray(:)
		integer :: j
		do j=1,size(personArray)
			if (personArray(j)%gender .EQ. 'm') then
				outPersonArray(j) = personArray(j)
			end if
		enddo
	end subroutine meetMaleCriteria
	
	subroutine meetFemaleCriteria(this, personArray, outPersonArray)
		class(femaleCriteria), intent(in) :: this
		type(person), dimension (:), allocatable, intent(inout) :: personArray(:)
		type(person), dimension (:), allocatable, intent(inout) :: outPersonArray(:)
		integer :: j
		do j=1,size(personArray)
			if (personArray(j)%gender .EQ. 'f') then
				outPersonArray(j) = personArray(j)
			end if
		enddo
	end subroutine meetFemaleCriteria

	subroutine meetSingleCriteria(this, personArray, outPersonArray)
		class(singleCriteria), intent(in) :: this
		type(person), dimension (:), allocatable, intent(inout) :: personArray(:)
		type(person), dimension (:), allocatable, intent(inout) :: outPersonArray(:)
		integer :: j
		do j=1,size(personArray)
			if (personArray(j)%maritalstatus .EQ. 's') then
				outPersonArray(j) = personArray(j)
			end if
		enddo
	end subroutine meetSingleCriteria

	subroutine meetAndCriteria(this, personArray, outPersonArray)
		class(AndCriteria), intent(in) :: this
		type(person), dimension (:), allocatable, intent(inout) :: personArray(:)
		type(person), dimension (:), allocatable, intent(inout) :: outPersonArray(:)
		type(person), dimension (:), allocatable :: tempPersonArray(:)
		call this%c1%meetCriteria(personArray, tempPersonArray)
		call this%c1%meetCriteria(temppersonArray, outPersonArray)
	end subroutine meetAndCriteria

	subroutine meetOrCriteria(this, personArray, outPersonArray)
		class(OrCriteria), intent(in) :: this
		type(person), dimension (:), allocatable, intent(inout) :: personArray(:)
		type(person), dimension (:), allocatable, intent(inout) :: outPersonArray(:)
		type(person), dimension (:), allocatable :: tempPersonArray1(:)
		type(person), dimension (:), allocatable :: tempPersonArray2(:)
		integer :: i, j
		logical :: found = .false.
		call this%c1%meetCriteria(personArray, tempPersonArray1)
		call this%c1%meetCriteria(personArray, tempPersonArray2)
		
		do i = 1, size(tempPersonArray1), 1
			outPersonArray(i) = tempPersonArray1(i)
		end do

		do i = 1, size(tempPersonArray2), 1
			found = .false.
			do j=1, size(outPersonArray), 1
				if (outPersonArray(j)%name==tempPersonArray2(i)%name) then
					found = .true.
					exit
				endif
			end do
			if (found .eqv. .true.) then
				outpersonArray(size(outpersonArray)+1) = tempPersonArray2(i)
			endif
		end do
	end subroutine meetOrCriteria

end module criteria_module