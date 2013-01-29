erlang*:::process-scheduled
{
  self->sched_ts[copyinstr(arg0)] = vtimestamp;
}

erlang*:::process-scheduled
{
  @time[copyinstr(arg0)] = lquantize((vtimestamp - self->sched_ts[copyinstr(arg0)] ) / 1000, 0, 63, 2);
}
