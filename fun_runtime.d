erlang*:::function-entry
{
  self->funcall_entry_ts[copyinstr(arg1)] = vtimestamp;
}
erlang*:::function-return
{
  @time[copyinstr(arg1)] = lquantize((vtimestamp - self->syscall_entry_ts[copyinstr(arg1)] ) / 1000, 0, 63, 2);
}
