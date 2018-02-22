package week02

/**
  * Created by Yuqi Li on 2018/2/6.
  */
object Practice {
  type Set = Int => Boolean

  def contains(s:Set,elem:Int) : Boolean = s(elem)

  //x 是 Set接收的那个Int值
  def singletonSet(elem:Int):Set = x => x==elem

  def union(s:Set,t:Set):Set = x => s(x) || t(x)

  def intersect(s:Set,t:Set):Set = x => s(x) && t(x)

  def diff(s:Set,t:Set):Set = x => s(x) && (!t(x))

  def filter(s:Set, p:Int => Boolean):Set = x => s(x) && p(x)

  val bound = 1000

  def forall(s:Set,p:Int => Boolean):Boolean = {
    def iter(a:Int):Boolean = {
      //如果能到a = -bound -1,那说明 -1000到1000都满足p
      if(a == -bound - 1) true
      else if(!(s(a) && p(a))) false
      else iter(a-1)
    }
    iter(bound)
  }

  //可以把forall改一下，让它判断出set里所有都不符合p的情况
  //在这样的情况下，如果返回真，那么就不存在任何一个满足p的情况
  //如果返回假，那么就存在有至少一个元素满足p
  def exists(s:Set,p: Int => Boolean):Boolean = ! forall(s, x => !p(x))

  //需要判断s中的元素经过f映射后，是否有和输入整型相等的即可
  def map(s:Set,f:Int => Int): Set = x => exists(s,elem => x == f(elem))

}
