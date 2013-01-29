/*
 * How long processes are scheduled in erlang
 */

erlang*:::process-scheduled
{
  self->sched_ts[copyinstr(arg0)] = vtimestamp;
}

erlang*:::process-scheduled
/self->sched_ts[copyinstr(arg0)]/
{
  @time[copyinstr(arg0)] = lquantize((vtimestamp - self->sched_ts[copyinstr(arg0)]), 0, 63, 2);
  self->sched_ts[copyinstr(arg0)] = 0;
}
