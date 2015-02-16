package org.raml.model.diet;

abstract class BasicElement[ ChildType <: IModelElement[ _]]() extends Object
  with IModelElement[ ChildType] {

  def this( members:Seq[ChildType])={
    this();
    this.members=List();
    for (r<-members){
      this+=r;
    }
  }
  var members:Seq[ChildType];
  var diet:Boolean;
  var name:String;
  var desc:String="";
  
  def _name(n:String){
    this.name=n;
  }
  
  def children(): Seq[ChildType] = members;

  def description: String = desc

  def _description(n: String) = desc = n

  def +=(child: ChildType) {
    if (child.parent != null) {
      child.parent-=child;
    }
    members = members :+ child;
    if (child.isInstanceOf[BasicElement[ _]]) {
      child.asInstanceOf[BasicElement[_]].onAdd(this);
    }
  }
  protected def onAdd(p: IModelElement[_]) = {}
  def -=(child: Any) {
    members = members.filter { x => x != child };
  }
  
  def deepCopy():BasicElement[ChildType]={
    var t:BasicElement[ChildType]=clone.asInstanceOf[BasicElement[ChildType]];
    t.members=List();
    for(c <- members){
      t+=c.deepCopy.asInstanceOf[ChildType];
    }
    return t;
  }
}
abstract class ParentedElement[ChildType <: IModelElement[_]]( members: Seq[ChildType] = List()) extends BasicElement[ChildType]( members) {
  var prt: IModelElement[_] = null;
  def parent(): IModelElement[_] = prt;

  override def deepCopy():BasicElement[ChildType]={
    var t=super.deepCopy().asInstanceOf[ParentedElement[ChildType]];
    t.prt=null;
    return t;
  }
  
  protected override def onAdd(p: IModelElement[_]) { prt = p }
}