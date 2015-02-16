package org.raml.model.diet;

trait IModelElement[ChildType <: IModelElement[ _]] extends Cloneable with Serializable{

  implicit def ResourceTwoList[T<:IModelElement[_]](r:T):List[T]=List(r);
  
  def name: String
  def _name(n: String)

  def description: String
  def _description(n: String)

  def parent(): IModelElement[_];

  def key(): String = if (parent != null) { parent.key + "/" + name } else { name };

  def children(): Seq[ChildType]
  
  def hasChild(child:Any)=children.contains(child)

  def children[T >: ChildType](t: Class[T]): Seq[T] = {
    children().filter { p => t.isInstance(p) }
  }

  def allChildren[T <: IModelElement[ _]](t: Class[T]): Seq[T] = {
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
    
    if (parent != null && parent.isInstanceOf[IModelElement[ _]]) {
      return parent.asInstanceOf[IModelElement[ _]].getAncestorOfKind(t);
    }
    return null.asInstanceOf[T];
  }

  def visit(x: Function1[IModelElement[_], Unit]): Unit = {
    children().foreach { z => x(z); z.visit(x) }
  }
  override def toString:String=key;
  
  def +=(child: ChildType);
  def -=(child: Any) ;
  
  
  def merge(toMerge:IModelElement[ChildType]){
    if (!canMerge(toMerge)){
      throw new IllegalArgumentException();
    }
    for (c<- toMerge.children()){
      var ch=сhild(c.name);
      if(ch.isDefined&&ch.get.canMerge(c)){
        var v:IModelElement[IModelElement[_]]=c.asInstanceOf[IModelElement[IModelElement[_]]];
        var z:IModelElement[IModelElement[_]]=ch.get.asInstanceOf[IModelElement[IModelElement[_]]];
        z.merge(v);
      }
      else{
        this+=c;
      }
    }
  }
  override def clone():IModelElement[ChildType]={super.clone().asInstanceOf[IModelElement[ChildType]]}
  def сhild(name:String):Option[ChildType]=children.find { x => x.name==name }
  def ->(name:String):ChildType=children.find { x => x.name==name }.get;
 
  def deepCopy():IModelElement[ChildType];
  
  def canMerge(toMerge:IModelElement[_]):Boolean=toMerge!=null&&this.getClass==toMerge.getClass
}