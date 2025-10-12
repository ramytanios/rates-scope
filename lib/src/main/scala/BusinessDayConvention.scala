package lib

enum BusinessDayConvention:

  /** Take the next business day if payment day is a holiday */
  case Following

  /** Take the prev business day if payment day is a holiday */
  case Preceding

  /** As `Following` but reverts to `Preceding` if day falls on the next month */
  case ModifiedFollowing

  /** As `Preceding` but reverts to `Following` if day falls on the next month */
  case ModifiedPreceding
