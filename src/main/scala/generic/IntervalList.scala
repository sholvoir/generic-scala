/**
  * package com.micinfotech.generic


  * Created by sovar on 1/3/17.

class Interval[A: Ordering](start: A)(end: A)(endpoint: (Boolean, Boolean) = (True, True)) {
  var prev: Interval[A] = _
  var next: Interval[A] = _

  def contains(item: A)(implicit cmp: Ordering[A]): Boolean = {
    val x = cmp.compare(item, start)
    val y = cmp.compare(end, item)
    ((x > 0) && (y > 0)) || ((x == 0) && endpoint._1) || (y == 0) && endpoint._2
  }
}

class IntInterval(val start: Int, val end: Int) {
  var prev: IntInterval = _
  var next: IntInterval = _
  def contains(item: Int): Boolean = item >= start && item <= end
}

class IntervalList {
  var head: IntInterval = _
  var last: IntInterval = _
  def insert(item: IntInterval): Unit = {
    if (last == null) {
      last = item
      head = item
    } else {
      var current = last
      while (current != null && current.start >= item.start) current = current.prev
      if (current == null) {
        item.next = head
        head.prev = item
        head = item
      } else {
        item.next = current.next
        current.next.prev = item
        current.next = item
        item.prev = current
      }
    }
    val mitem = if (item.prev.contains(item.start)) {

    } else item
    //merge(mitem)
  }
  private def merge(item: IntInterval): Unit = {

  }
}
  */