program test_pdf_info_trailer_reference_1552
   use, intrinsic :: iso_fortran_env, only: int64
   use fortplot
   use test_output_helpers, only: ensure_test_output_dir, assert_pdf_file_valid
   implicit none

   character(len=:), allocatable :: output_dir
   character(len=:), allocatable :: out_pdf
   integer :: unit
   integer :: ios
   integer(int64) :: fsize
   character(len=1), allocatable :: data(:)
   logical :: has_info_ref

   call ensure_test_output_dir('pdf_info_trailer_reference_1552', output_dir)
   out_pdf = trim(output_dir)//'info_trailer_reference.pdf'

   call figure()
   call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp])
   call title('PDF trailer includes /Info reference')
   call savefig(out_pdf)

   call assert_pdf_file_valid(out_pdf)

   open (newunit=unit, file=out_pdf, access='stream', form='unformatted', &
         status='old', action='read', iostat=ios)
   if (ios /= 0) then
      print *, 'FAIL: cannot open ', trim(out_pdf)
      stop 1
   end if

   inquire (unit=unit, size=fsize)
   if (fsize <= 0_int64) then
      print *, 'FAIL: zero-size PDF'
      close (unit)
      stop 1
   end if

   allocate (character(len=1) :: data(fsize))
   read (unit, iostat=ios) data
   close (unit)
   if (ios /= 0) then
      print *, 'FAIL: cannot read PDF data'
      stop 1
   end if

   has_info_ref = bytes_contains(data, fsize, '/Info 1 0 R')
   if (.not. has_info_ref) then
      print *, 'FAIL: PDF trailer missing /Info 1 0 R'
      stop 1
   end if

   print *, 'PASS: PDF trailer includes /Info reference'
contains
   logical function bytes_contains(arr, n, pat) result(found)
      character(len=1), intent(in) :: arr(n)
      integer(int64), intent(in) :: n
      character(len=*), intent(in) :: pat
      integer :: i
      integer :: j
      integer :: m

      found = .false.
      m = len_trim(pat)
      if (m <= 0) return
      do i = 1, int(n) - m + 1
         do j = 1, m
            if (arr(i + j - 1) /= pat(j:j)) exit
            if (j == m) then
               found = .true.
               return
            end if
         end do
      end do
   end function bytes_contains
end program test_pdf_info_trailer_reference_1552
