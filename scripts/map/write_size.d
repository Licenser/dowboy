/*
 * Write size
 */
fsinfo:::write {
  @[args[0]->fi_mount] = lquantize(arg1/1024, 0, 63, 2)
}
