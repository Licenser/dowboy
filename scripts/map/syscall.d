syscall:::entry
{
  self->syscall_entry_ts[probefunc] = vtimestamp;
}
syscall:::return
/self->syscall_entry_ts[probefunc]/
{
  @time[probefunc] = lquantize((vtimestamp - self->syscall_entry_ts[probefunc] ) / 1000, 0, 63, 2);
  self->syscall_entry_ts[probefunc] = 0;
}

