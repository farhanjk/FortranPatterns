!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Main Program
program criteria_main
	use criteria_module
	implicit none
	type(person), allocatable :: persons(:)
	type(person), allocatable :: outPersons(:)
	class(criteria), allocatable :: cmale	
	class(criteria), allocatable :: cfemale	
	class(criteria), allocatable :: csingle
	class(criteria), allocatable :: cand
	class(criteria), allocatable :: cor	
	type(andcriteria) :: and_criteria
	type(orcriteria) :: or_criteria
	
	!Initialize array with list of persons
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
	
	!All males
	call cmale%meetcriteria(persons, outPersons)
	print *, 'Males:'
	call printPersonArray(outPersons)
	deallocate(outPersons)

	!All females
	call cfemale%meetcriteria(persons, outPersons)
	print *, 'Females:'
	call printPersonArray(outPersons)
	deallocate(outPersons)

	!Male and Single
	allocate(and_criteria%c1, source = malecriteria())
	allocate(and_criteria%c2, source = singlecriteria())
	allocate(cand, source=and_criteria)
	call cand%meetCriteria(persons, outPersons)
	print *, 'Single Males:'
	call printPersonArray(outPersons)
	deallocate(outPersons)

	!Single or Female
	allocate(or_criteria%c1, source = singlecriteria())
	allocate(or_criteria%c2, source = femalecriteria())
	allocate(cor, source=or_criteria)
	call cor%meetCriteria(persons, outPersons)
	print *, 'Single Or Females:'
	call printPersonArray(outPersons)
	deallocate(outPersons)

	deallocate(persons)
	deallocate(cmale)
	deallocate(cfemale)
	deallocate(csingle)
	deallocate(cand)
	deallocate(cor)
	
end program criteria_main