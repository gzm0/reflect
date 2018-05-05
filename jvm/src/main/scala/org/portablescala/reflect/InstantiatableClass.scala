package org.portablescala.reflect

final class InstantiatableClass private[reflect] (clazz: Class[_]) {
  def runtimeClass: Class[_] = clazz

  def declaredConstructors: List[InvokableConstructor] =
    clazz.getConstructors().map(new InvokableConstructor(_)).toList

  def newInstance(): Any = clazz.newInstance

  def getConstructor(parameterTypes: Class[_]*): Option[InvokableConstructor] = {
    try {
      Some(new InvokableConstructor(clazz.getConstructor(parameterTypes: _*)))
    } catch {
      case _: NoSuchMethodException => None
    }
  }
}
