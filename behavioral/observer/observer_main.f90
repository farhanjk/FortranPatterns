!> Reference: https://refactoring.guru/design-patterns/observer/go/example
program test_observer

    use observer_pattern, only: item, customer, new_item
    type(item) :: shirt_item
    type(customer) :: observer_first, observer_second, observer_third

    !> A shirt item
    shirt_item = new_item("A Shirt")

    !> Some customers
    observer_first = customer(ID="abc@gmail.com")
    observer_second = customer(ID="def@gmail.com")
    observer_third = customer(ID="xyz@foxmail.com")

    !> Scene 1
    call shirt_item%register(observer_first)
    call shirt_item%register(observer_second)
    call shirt_item%update_availability()

    !> Scene 2
    call shirt_item%deregister(observer_first)
    call shirt_item%register(observer_third)
    call shirt_item%update_availability()

end program test_observer