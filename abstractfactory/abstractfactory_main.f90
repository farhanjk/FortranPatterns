program abstractfactory_main
	use abstractfactory
	class(theAbstractFactory), allocatable :: theFactory
	class(shape), allocatable :: shapeInstance
	class(color), allocatable :: colorInstance

	type(factoryProducer) :: factoryProducerInstance

	call factoryProducerInstance%getFactory('Shape', theFactory)
	call theFactory%getShape('Rectangle', shapeInstance)
	call shapeInstance%draw
	deallocate(shapeInstance)

	call factoryProducerInstance%getFactory('Color', theFactory)
	call theFactory%getColor('Blue', colorInstance)
	call colorInstance%fill
	deallocate(colorInstance)

	deallocate(theFactory)
end program abstractfactory_main