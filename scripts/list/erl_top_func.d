/*
 * Measures how long erlang spends in each function.
 * This is kind of like profiling.
 */
erlang*:::global-function-entry
{
  self->funcall_entry_ts[copyinstr(arg1)] = vtimestamp;
}
erlang*:::function-return
{
  @time[copyinstr(arg1)] = sum((vtimestamp - self->funcall_entry_ts[copyinstr(arg1)] ) / 1000);
}
