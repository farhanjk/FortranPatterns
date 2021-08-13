!> Reference: https://refactoring.guru/design-patterns/visitor/go/example
module visitor_pattern

    implicit none
    private

    public :: square, circle, rectangle, area_calculator, middle_coordinates

    !> Two abstract classes

    type, abstract :: shape
    contains
        procedure(get_type_procedure), deferred :: get_type
        procedure(accept_procedure), deferred :: accept
    end type shape

    type, abstract :: visitor
    contains
        procedure(visit_procedure), deferred :: visit
    end type visitor

    abstract interface
        function get_type_procedure(self) result(result)
            import shape
            class(shape), intent(inout) :: self
            character(:), allocatable :: result
        end function get_type_procedure
        subroutine accept_procedure(self, v)
            import shape, visitor
            class(shape), intent(inout) :: self
            class(visitor), intent(inout) :: v
        end subroutine accept_procedure
        subroutine visit_procedure(self, s)
            import visitor, shape
            class(visitor), intent(inout) :: self
            class(shape), intent(inout) :: s
        end subroutine visit_procedure
    end interface

    !> Specific shapes

    type, extends(shape) :: square
        integer :: side
    contains
        procedure :: get_type => square_get_type
        procedure :: accept => square_accept
    end type square

    type, extends(shape) :: circle
        integer :: radius
    contains
        procedure :: get_type => circle_get_type
        procedure :: accept => circle_accept
    end type circle

    type, extends(shape) :: rectangle
        integer :: l
        integer :: b
    contains
        procedure :: get_type => rectangle_get_type
        procedure :: accept => rectangle_accept
    end type rectangle

    !> Specific visitors

    type, extends(visitor) :: area_calculator
        integer :: area
    contains
        procedure :: visit => area_calculator_visit
    end type area_calculator

    type, extends(visitor) :: middle_coordinates
        integer :: x, y
    contains
        procedure :: visit => middle_coordinates_visit
    end type middle_coordinates

contains

    function square_get_type(self) result(result)
        class(square), intent(inout) :: self
        character(:), allocatable :: result
        result = "Square"
    end function square_get_type

    function circle_get_type(self) result(result)
        class(circle), intent(inout) :: self
        character(:), allocatable :: result
        result = "Circle"
    end function circle_get_type

    function rectangle_get_type(self) result(result)
        class(rectangle), intent(inout) :: self
        character(:), allocatable :: result
        result = "Rectangle"
    end function rectangle_get_type

    subroutine square_accept(self, v)
        class(square), intent(inout) :: self
        class(visitor), intent(inout) :: v
        call v%visit(self)
    end subroutine square_accept

    subroutine circle_accept(self, v)
        class(circle), intent(inout) :: self
        class(visitor), intent(inout) :: v
        call v%visit(self)
    end subroutine circle_accept

    subroutine rectangle_accept(self, v)
        class(rectangle), intent(inout) :: self
        class(visitor), intent(inout) :: v
        call v%visit(self)
    end subroutine rectangle_accept

    subroutine area_calculator_visit(self, s)
        class(area_calculator), intent(inout) :: self
        class(shape), intent(inout) :: s
        select type (s)
        type is (square)
            print *, "Calculating area for square.🔥"
        type is (circle)
            print *, "Calculating area for circle.🔥"
        type is (rectangle)
            print *, "Calculating area for rectangle.🔥"
        end select
    end subroutine area_calculator_visit

    subroutine middle_coordinates_visit(self, s)
        class(middle_coordinates), intent(inout) :: self
        class(shape), intent(inout) :: s
        select type (s)
        type is (square)
            print *, "Calculating middle point coordinates for square.💠"
        type is (circle)
            print *, "Calculating middle point coordinates for circle.💠"
        type is (rectangle)
            print *, "Calculating middle point coordinates for rectangle.💠"
        end select
    end subroutine middle_coordinates_visit

end module visitor_pattern
