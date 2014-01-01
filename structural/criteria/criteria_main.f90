program criteria_main
	use criteria_module
	implicit none
	type(person), allocatable :: persons(:)
	class(criteria), allocatable :: cmale	
	class(criteria), allocatable :: cfemale	
	class(criteria), allocatable :: csingle
	class(criteria), allocatable :: cand
	class(criteria), allocatable :: cor	
		
	allocate(persons(6))
	persons(1) = person('Robert', 'm', 's')
	persons(2) = person('John', 'm', 'm')
	persons(3) = person('Laura', 'f', 'm')	
	persons(4) = person('Diana', 'f', 's')	
	persons(5) = person('Mike', 'm', 's')	
	persons(6) = person('Bobby', 'm', 's')	
	
	allocate(cmale, source=malecriteria())
	allocate(cfemale, source=femalecriteria())
	allocate(csingle, source=singlecriteria())
	allocate(cand, source=andcriteria(cmale, null()))
!	allocate(cor, source=orcriteria(csingle, cfemale))
	
!	deallocate(persons)
!	deallocate(cmale)
!	deallocate(cfemale)
!	deallocate(csingle)
!	deallocate(cand)
!	deallocate(cor)
	
end program criteria_main