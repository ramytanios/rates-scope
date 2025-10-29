package lib
package dtos

enum OptionType(val sign: Int):
  case Call extends OptionType(1)
  case Put extends OptionType(-1)
