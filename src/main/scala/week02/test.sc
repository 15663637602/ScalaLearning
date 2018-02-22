object test{
  type Set = Int => Boolean

  def contains(s:Set,elem:Int):Boolean = s(elem)

  def singletonSet(elem:Int):Set = x => x == elem

  def union(s:Set,t:Set):Set = x => s(x) || t(x)
  def diff(s:Set,t:Set):Set = x => s(x) && (!t(x))
  def filter(s:Set,p:Int => Boolean):Set = x => s(x) && p(x)
  val bound = 1000
  def forall(s:Set,p:Int => Boolean):Boolean = {
    def iter(a:Int): Boolean = {
      if(s == -bound - 1) true
      else if(s(x) && (!p(x))) false
      else iter(a-1)
    }
    iter(bound)
  }

  def exists(s:Set,p:Int => Boolean):Boolean = ! forall(s,x => !p(x))

  def map(s:Set,f:Int => Int):Set = x => exists(s,elem => x == f(elem))
}