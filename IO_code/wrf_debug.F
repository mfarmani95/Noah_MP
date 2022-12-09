SUBROUTINE set_wrf_debug_level ( level ) 
  USE module_wrf_error 
  IMPLICIT NONE 
  INTEGER , INTENT(IN) :: level 
  wrf_debug_level = level 
  RETURN 
END SUBROUTINE set_wrf_debug_level 
 
SUBROUTINE get_wrf_debug_level ( level ) 
  USE module_wrf_error 
  IMPLICIT NONE 
  INTEGER , INTENT(OUT) :: level 
  level = wrf_debug_level 
  RETURN 
END SUBROUTINE get_wrf_debug_level 
 
SUBROUTINE wrf_debug( level , str ) 
  USE module_wrf_error 
  IMPLICIT NONE 
  CHARACTER*(*) str 
  INTEGER , INTENT (IN) :: level 
  INTEGER :: debug_level 
  CHARACTER (LEN=256) :: time_str 
  CHARACTER (LEN=256) :: grid_str 
  CHARACTER (LEN=512) :: out_str 
  if(silence/=0) return
  CALL get_wrf_debug_level( debug_level ) 
  IF ( level .LE. debug_level ) THEN 
#ifdef _OPENMP 
  ! old behavior 
  CALL wrf_message( str ) 
#else 
  ! TBH: This fails on pgf90 6.1-4 when built with OpenMP and using more 
  ! TBH: than one thread. It works fine multi-threaded on AIX with xlf 
  ! TBH: 10.1.0.0 . Hence the cpp nastiness. 
  ! new behavior: include domain name and time-stamp 
  !niu CALL get_current_time_string( time_str ) 
  !niu CALL get_current_grid_name( grid_str ) 
  !niu out_str = TRIM(grid_str)//' '//TRIM(time_str)//' '//TRIM(str) 
  !niu CALL wrf_message( TRIM(out_str) ) 
#endif 
  ENDIF 
  RETURN 
END SUBROUTINE wrf_debug
      
subroutine WRF_trace(fpath,line,msg,lunit)      
   implicit none
      
   character(len=*), intent(in) :: fpath
   integer, intent(in) :: line
   character(len=*), intent(in), optional :: msg
   integer, intent(in), optional :: lunit

   integer :: lunit_
   character(len=:), allocatable :: str
   character(len=:), allocatable :: msg_
   msg_ = ""
   lunit_ = 6
   if (present (msg)) msg_ = msg
   if (present (lunit)) lunit_ = lunit
   str = fname(fpath)
   write(unit=lunit_,fmt='(a,1x,i4,1x,a)')'[DEBUG] '//str,line,msg_
   flush(lunit_)

contains
      
   function fname(fpath_)
      character(len=*), intent(in) :: fpath_
      character(:), allocatable :: fname
      character(len=64) :: ok='._ABCDEFGHIJKLMNOPQRSTUVWXYZ'//&
                              'abcdefghijklmnopqrstuvwxyz'//&
                              '0123456789'
      integer l1,l2
      fname = fpath_
      l2 = len(fpath_)
      l1 = l2
      do
         if (index(ok,fpath_(l1:l1)) > 0) then 
            l1 = l1 - 1
         else
            exit
         end if
      end do
      fname = fpath_(l1+1:l2)
   end function fname

end subroutine WRF_trace
