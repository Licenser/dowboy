/*
 * Measures how long erlang spends in each function and grouped by PID.
 * This is kind of like profiling.
 */
erlang*:::global-function-entry
{
  self->funcall_entry_ts[copyinstr(arg1), copyinstr(arg0)] = vtimestamp;
}
erlang*:::function-return
{
  @time[copyinstr(arg1), copyinstr(arg0)] = sum((vtimestamp - self->funcall_entry_ts[copyinstr(arg0), copyinstr(arg1)] ) / 1000);
}
