/*
 * Counts the number each function is called.
 */
erlang*:::global-function-entry
{
  @[copyinstr(arg1)] = count();
}
