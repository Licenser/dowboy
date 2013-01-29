/*
 * Counts how much each function was called within each pid,
 * pids can be filtered by adding
 * / copyinstr(arg0) == "<0.107.0>"/
 */
erlang*:::global-function-entry
{
  @[copyinstr(arg1), copyinstr(arg0)] = count();
}
