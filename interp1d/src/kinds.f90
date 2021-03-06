module kinds
  use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
  private

! default precision
  integer, parameter :: rp = real64
  integer, parameter :: ip = int32

  public ::  rp, ip, int8, int16, int32, int64, real32, real64, real128
end module kinds

