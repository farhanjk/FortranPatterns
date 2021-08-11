!Copyright (c) 2013 Farhan J. Khan
!THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
!FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
!COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
!IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

!Main Program
program singleton_main
    !Module providing singleton functionality
    use singleton
    !This will create singleton object
    call construct_singleton()
    !Print the value (0)
    print *, get_singleton_value()
    !set the value for singleton
    call set_singleton_value(89)
    !Print the value of singleton  (89)
    print *, get_singleton_value()
    !This will ignore creation as the instance is already available
    call construct_singleton()
    !Print the value (still 89!)
    print *, get_singleton_value()
    !Dispose off the singleton instance
    call dispose_singleton()
    !This will create singleton object
    call construct_singleton()
    !Print the value (0)
    print *, get_singleton_value()
end program singleton_main