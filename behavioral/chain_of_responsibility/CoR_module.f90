!> CoR: Hospital departments
module hospital_CoR

    implicit none
    private

    public :: patient, department, reception, doctor, medical, cashier

    type patient
        character(:), allocatable :: name
        logical :: registration_done
        logical :: doctor_check_up_done
        logical :: medicine_done
        logical :: payment_done
    end type patient

    type, abstract :: department
    contains
        procedure(execute_procedure), deferred :: execute
        procedure(set_next_procedure), deferred :: set_next
    end type department

    abstract interface
        subroutine execute_procedure(self, p)
            import department, patient
            class(department), intent(inout) :: self
            type(patient), intent(inout) :: p
        end subroutine execute_procedure
        subroutine set_next_procedure(self, next)
            import department
            class(department), intent(inout) :: self
            class(department), intent(inout) :: next
        end subroutine set_next_procedure
    end interface

    type, extends(department) :: reception
        class(department), pointer :: next
    contains
        procedure :: execute => reception_execute
        procedure :: set_next => reception_set_next
    end type reception

    type, extends(department) :: doctor
        class(department), pointer :: next
    contains
        procedure :: execute => doctor_execute
        procedure :: set_next => doctor_set_next
    end type doctor

    type, extends(department) :: medical
        class(department), pointer :: next
    contains
        procedure :: execute => medicine_execute
        procedure :: set_next => medicine_set_next
    end type medical

    type, extends(department) :: cashier
        class(department), pointer :: next
    contains
        procedure :: execute => cashier_execute
        procedure :: set_next => cashier_set_next
    end type cashier

contains

    subroutine reception_execute(self, p)
        class(reception), intent(inout) :: self
        type(patient), intent(inout) :: p

        if (p%registration_done) then
            print *, "Patient registration already done.✔️"
            call self%next%execute(p)
            return
        end if

        print *, "Reception registering patient."
        p%registration_done = .true.
        call self%next%execute(p)

    end subroutine reception_execute

    subroutine reception_set_next(self, next)
        class(reception), intent(inout) :: self
        class(department), intent(inout) :: next

        allocate(self%next, source=next)

    end subroutine reception_set_next   

    subroutine doctor_execute(self, p)
        class(doctor), intent(inout) :: self
        type(patient), intent(inout) :: p

        if (p%doctor_check_up_done) then
            print *, "Doctor checkup already done.✔️"
            call self%next%execute(p)
            return
        end if

        print *, "Doctor checking patient."
        p%doctor_check_up_done = .true.
        call self%next%execute(p)

    end subroutine doctor_execute

    subroutine doctor_set_next(self, next)
        class(doctor), intent(inout) :: self
        class(department), intent(inout) :: next

        allocate(self%next, source=next)

    end subroutine doctor_set_next   

    subroutine medicine_execute(self, p)
        class(medical), intent(inout) :: self
        type(patient), intent(inout) :: p

        if (p%medicine_done) then
            print *, "Medicine already given to patient.✔️"
            call self%next%execute(p)
            return
        end if

        print *, "Medical giving medicine to patient."
        p%medicine_done = .true.
        call self%next%execute(p)

    end subroutine medicine_execute

    subroutine medicine_set_next(self, next)
        class(medical), intent(inout) :: self
        class(department), intent(inout) :: next

        allocate(self%next, source=next)

    end subroutine medicine_set_next   

    subroutine cashier_execute(self, p)
        class(cashier), intent(inout) :: self
        type(patient), intent(inout) :: p

        if (p%payment_done) then
            print *, "Payment Done.✔️"
            return
        end if

        print *, "Cashier getting money from patient."
        p%payment_done = .true.

    end subroutine cashier_execute

    subroutine cashier_set_next(self, next)
        class(cashier), intent(inout) :: self
        class(department), intent(inout) :: next

        allocate(self%next, source=next)

    end subroutine cashier_set_next   

end module hospital_CoR