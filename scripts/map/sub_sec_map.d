/*
 * Sub second offset CPU utilisation.
 * To limit to one executable you can add:
 *   / execname == "beam" /
 */
profile:::profile-997
{
  @[execname] = lquantize((timestamp % 1000)/15,0, 63, 2);
}
