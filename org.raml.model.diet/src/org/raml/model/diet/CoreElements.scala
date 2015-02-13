package org.raml.model.diet;

abstract class BasicElement[ParentType <: CanHaveChild, ChildType <: IModelElement[_, _]](
  private var nm: String = null,
  private var ch: Seq[ChildType] = List(), private var desc: String = "") extends Object
  with IModelElement[ParentType, ChildType] {

  def children(): Seq[ChildType] = ch;

  def name: String = nm;

  def _name(n: String) = nm = n;

  def description: String = desc

  def _description(n: String) = desc = n

  def +=(child: ChildType) {
    if (child.parent != null) {
      val pr: CanHaveChild = child.parent.asInstanceOf[CanHaveChild];
      pr -= child;
    }
    ch = ch :+ child;
    if (child.isInstanceOf[BasicElement[_, _]]) {
      child.asInstanceOf[BasicElement[BasicElement[_, _], _]].onAdd(this);
    }
  }
  protected def onAdd(p: ParentType) = {}
  def -=(child: Any) {
    ch = ch.filter { x => x != child };
  }
}
abstract class ParentedElement[ParentType <: CanHaveChild, ChildType <: IModelElement[_, _]](name: String, children: Seq[ChildType] = List()) extends BasicElement[ParentType, ChildType](name, children) {
  var prt: ParentType = null.asInstanceOf[ParentType];
  def parent(): ParentType = prt;

  protected override def onAdd(p: ParentType) { prt = p }
}