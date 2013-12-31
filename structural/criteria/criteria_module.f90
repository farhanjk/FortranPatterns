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
	
end module criteria_module