!Main Program
!To compile:
!$ gfortran -c factory.f90
!$ gfortran factory_main.f90 factory.o -o factory_main
program factory_main
	!Shape Factory module
	use factory
	!Alocatable shape address
	class(shape), allocatable :: shapeInstance
	!Get a rectangle and verify by printing
	call fromShapeFactoryGetShape("Rectangle", shapeInstance)
	call shapeInstance%printShape
	deallocate(shapeInstance)
	!Get a circle and verify by printing
	call fromShapeFactoryGetShape("Circle", shapeInstance)
	call shapeInstance%printShape
	deallocate(shapeInstance)
end program factory_main