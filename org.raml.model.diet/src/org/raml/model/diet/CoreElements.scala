package org.raml.model.diet;

abstract class BasicElement[ ChildType <: IModelElement[ _]](
  private var nm: String = null,
  private var ch: Seq[ChildType] = List(), private var desc: String = "") extends Object
  with IModelElement[ ChildType] {

  def children(): Seq[ChildType] = ch;

  def name: String = nm;

  def _name(n: String) = nm = n;

  def description: String = desc

  def _description(n: String) = desc = n

  def +=(child: ChildType) {
    if (child.parent != null) {
      child.parent-=child;
    }
    ch = ch :+ child;
    if (child.isInstanceOf[BasicElement[ _]]) {
      child.asInstanceOf[BasicElement[_]].onAdd(this);
    }
  }
  protected def onAdd(p: IModelElement[_]) = {}
  def -=(child: Any) {
    ch = ch.filter { x => x != child };
  }
  
  def deepCopy():BasicElement[ChildType]={
    var t:BasicElement[ChildType]=clone.asInstanceOf[BasicElement[ChildType]];
    t.ch=List();
    for(c <- children){
      t+=c.deepCopy.asInstanceOf[ChildType];
    }
    return t;
  }
}
abstract class ParentedElement[ChildType <: IModelElement[_]](name: String, children: Seq[ChildType] = List()) extends BasicElement[ChildType](name, children) {
  var prt: IModelElement[_] = null;
  def parent(): IModelElement[_] = prt;

  override def deepCopy():BasicElement[ChildType]={
    var t=super.deepCopy().asInstanceOf[ParentedElement[ChildType]];
    t.prt=null;
    return t;
  }
  
  protected override def onAdd(p: IModelElement[_]) { prt = p }
}