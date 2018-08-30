package cn.micit.generic

/**
  * Created by sovar on 6/8/16.
  * You can get once from a BufferedIterator which have been sorted by id.
  * @param as the BufferedIterator
  * @param getid A -> id(int) function
  * @tparam A type
  */
class GetOnce[A](as: BufferedIterator[A])(getid: A => Int) {

  /**
    * Get data method
    * @param id data's identifier
    * @return data or null
    */
  def get(id: Int): Option[A] = {
    while (as.hasNext && getid(as.head) < id) as.next()
    if (as.hasNext && id == getid(as.head)) Some(as.head) else null
  }
}
