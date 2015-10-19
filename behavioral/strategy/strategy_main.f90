!Copyright (c) 2015 zmiimz
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!-----------------------------------------------------------------------
!Main strategy_main
!-----------------------------------------------------------------------
program    strategy_main
   use sort_strategy_module
   use user_strategy_module
   implicit none

   type(shellsort_strategy) :: ssort
   type(quicksort_strategy) :: qsort
   type(user_strategy) :: us
   integer, dimension(:), allocatable :: data

   allocate(data(100))

   call us % init(ssort, data)

   call us % sort()

   call us % change_strategy(qsort)
   call us % sort()

   call us % change_strategy(ssort)
   call us % sort()

   call us % change_strategy(qsort)
   call us % sort()

end program strategy_main
