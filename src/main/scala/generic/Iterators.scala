package cn.micit.generic

/**
  * some functions for Iterator.
  */
object Iterators {

  /**
    * concatenate some Iterators(sorted) => one Iterator(sorted)
    * @param iterators some iterators
    * @param cmp implicit compare method for T
    * @tparam T type
    * @return a total iterator
    */
  def combine[T](iterators: Seq[Iterator[T]])(implicit cmp: Ordering[T]): Iterator[T] = new Iterator[T] {
    private val iters = iterators.map(_.buffered)

    def hasNext = iters.exists(_.hasNext)

    def next() = {
      val t = iters.filter(_.hasNext)
      if (t.isEmpty) throw new UnsupportedOperationException("Cannot call next on an exhausted iterator!")
      t.map(x => (x.head, x)).minBy(_._1)(cmp)._2.next()
    }
  }

  /**
    * a enhanced filter
    * @param iterator the source iterator
    * @param filter T => index => boolean filter function
    * @tparam T type
    * @return result iterator
    */
  def filter[T](iterator: Iterator[T])(filter: T => Int => Boolean): Iterator[T] = {
    var index = -1
    iterator.filter { t => index += 1; filter(t)(index) }
  }

  /**
    * parallel some Iterators => one Iterator
    * zip many iterators to one whose length is the shortest one.
    * @param iterators the source iterators
    * @tparam T type
    * @return result iterator
    */
  def zip[T](iterators: Seq[Iterator[T]]): Iterator[Seq[T]]  = new Iterator[Seq[T]] {
    def hasNext: Boolean = iterators.forall(_.hasNext)
    def next(): Seq[T] = if (hasNext) {
      iterators.map(_.next())
    } else {
      throw new UnsupportedOperationException("Cannot call next on an exhausted iterator!")
    }
  }

  /**
    * get a intersect zip iterator from two iterators
    * @param a type A iterator
    * @param b type B iterator
    * @param cmp compare function between A and B (-1 mean less than, 0 mean equal， 1 mean great than)
    * @tparam A type A
    * @tparam B type B
    * @return result iterator
    */
  def intersectZip[A, B](a: Iterator[A])(b: Iterator[B])(cmp: (A, B) => Int): Iterator[(A, B)] = new Iterator[(A, B)] {
    private val a1 = a.buffered
    private val b1 = b.buffered

    def hasNext = a1.hasNext && b1.hasNext

    private def gotoNext(): Unit = {
      var x = true
      while (hasNext && x) {
        val c = cmp(a1.head, b1.head)
        if (c < 0) a1.next()
        else if (c > 0) b1.next()
        else x = false
      }
    }

    gotoNext()

    def next() = if (hasNext) {
      val a2 = a1.next()
      val b2 = b1.next()
      gotoNext()
      (a2, b2)
    } else throw new UnsupportedOperationException("Cannot call next on an exhausted iterator!")
  }

  /**
    * get a union zip iterator from two iterators
    * @param a type A iterator
    * @param b type B iterator
    * @param defaultA the default value of type A
    * @param defaultB the default value of type B
    * @param cmp compare function between A and B (-1 mean less than, 0 mean equal， 1 mean great than)
    * @tparam A type A
    * @tparam B type B
    * @return result iterator
    */
  def unionZip[A, B](a: Iterator[A])(b: Iterator[B])(defaultA: A)(defaultB: B)(cmp: (A, B) => Int): Iterator[(A, B)] = new Iterator[(A, B)] {
    private val a1 = a.buffered
    private val b1 = b.buffered

    def hasNext = a1.hasNext || b1.hasNext

    def next() = {
      if (a1.hasNext && b1.hasNext) {
        val c = cmp(a1.head, b1.head)
        if (c < 0) (a1.next(), defaultB)
        else if (c > 0) (defaultA, b1.next())
        else (a1.next(), b1.next())
      } else if (a1.hasNext) {
        (a1.next(), defaultB)
      } else if (b1.hasNext) {
        (defaultA, b1.next())
      } else throw new UnsupportedOperationException("Cannot call next on an exhausted iterator!")
    }
  }
}