/*
 * Measures the execution time of erlang functions.
 * this can make pretty pictures!
 */
erlang*:::global-function-entry
{
  self->funcall_entry_ts[copyinstr(arg1)] = vtimestamp;
}
erlang*:::function-return
/self->funcall_entry_ts[copyinstr(arg1)]/
{
  @time[copyinstr(arg1)] = lquantize((vtimestamp - self->funcall_entry_ts[copyinstr(arg1)] ) / 1000, 0, 63, 2);
  self->funcall_entry_ts[copyinstr(arg1)] = 0;
}
