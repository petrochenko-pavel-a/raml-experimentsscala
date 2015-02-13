package org.raml.model.diet;

trait CanHaveChild{
  def -=(child: Any) ;
  def hasChild(child:Any):Boolean;
}

trait IModelElement[ParentType<:CanHaveChild, ChildType <: IModelElement[_, _]] extends CanHaveChild{

  def name: String
  def _name(n: String)

  def description: String
  def _description(n: String)

  def parent(): ParentType;

  def key(): String = if (parent != null) { parent + "/" + name } else { name };

  def children(): Seq[ChildType]
  
  def hasChild(child:Any)=children.contains(child)

  def children[T >: ChildType](t: Class[T]): Seq[T] = {
    children().filter { p => t.isInstance(p) }
  }

  def allChildren[T <: IModelElement[_, _]](t: Class[T]): Seq[T] = {
    var x = List[T]()
    visit {
      p =>
        if (t.isInstance(p)) {
          x = x :+ p.asInstanceOf[T]
        }
    };
    return x;
  }
  def getAncestorOfKind[T](t: Class[T]): T = {
    if (t.isInstance(this)) {
      return this.asInstanceOf[T];
    }
    if (parent != null && parent.isInstanceOf[IModelElement[_, _]]) {
      return parent.asInstanceOf[IModelElement[_, _]].getAncestorOfKind(t);
    }
    return null.asInstanceOf[T];
  }

  def visit(x: Function1[IModelElement[_, _], Unit]): Unit = {
    children().foreach { z => x(z); z.visit(x) }
  }
  override def toString:String=key()
  
}