package org.raml.metamodel;
import scala.util.matching.Regex
/**
 * Basic resource 
 */
trait Entity[T>:Null]{
  
  def description(description:String):T=null;
  def annotate(annotation:Any):T=null;
}

trait Embedded;

trait PropertyMapping{
  
}
class ModelMapping[A<:MetaType[_],B<:MetaType[_]](val from:A,val to:B){
  
   def mappingAssertion(cm:PropertyMapping)={};
}

trait Type extends Entity[Type]{
  def superType():Type
  def name():String
  def actualType():Value=null
  
  def validation():Condition=otherwise;
}


trait Property[RangeType<:Type,DomainType<:Type] extends Entity[Property[RangeType,DomainType]]{
   def range():RangeType
   def domain():DomainType
   def $=new Value(this);
}

case class Prop[R<:Type,D<:Type](val domain:D,val range:R)extends Property[R,D];

abstract class WrappedProp [R<:Type,D<:Type](val kp:Property[R,D]) extends Property[R,D]{
  def range()=kp.range;
  def domain()=kp.domain;
}
case class req[R<:Type,D<:Type](override val kp:Property[R,D]) extends WrappedProp[R,D](kp);
case class key[R<:Type,D<:Type](override val kp:Property[R,D]) extends WrappedProp[R,D](kp);
case class multivalue[R<:Type,D<:Type](override val kp:Property[R,D]) extends WrappedProp[R,D](kp);

class RegExpContrained(var regexp:Regex) extends MetaType("",Str){}

class MetaType[B<:Type](private val desc:String,private val sTypes:B=null)extends Type{
  def name()=getClass.getName();
  def superType=sTypes;
  
  override def toString()=name();
  def str()=new Prop(this,Str);
  def bool()=new Prop(this,BooleanType);
  def prop[T<:Type](t:T)=new Prop(this,t);
  def strEnum(v:Any)=new Prop(this,new EnumType(Str){values(v)});
  
  def mutuallyExclusive[D<:Type](t:Property[_,D]*):Property[_,D]=null;
  def require(c:Condition*):Entity[_]=null;
  def unionOf[D<:Type](t:Property[_,D]*):Property[_,D]=null;
}
class Child[T<:Type](private val desc:String,val parent:T,superType:Type=null)
  extends MetaType(desc,superType){  
}



class Value(p:Property[_,_]){
  def === (q:Any):Condition = Condition(this,equality,q);
  def <=> (q:Any):OneToOneMapping = OneToOneMapping(this,equality,q);//REWORK IT
  def < (q:Any):Condition = Condition(this,lessThen,q);
  def > (q:Any):Condition = Condition(this,lessThen,q);
  def instanceOf (q:Type):Condition = Condition(this,lessThen,q);
  def conforms(c:Condition)=Condition(this,conformsTo,c);
  def where(c:Condition)=FilteredValue(this,c);
}
case class FilteredValue(v:Value,c:Condition){
  
}
sealed class Op{
  
} 
object conformsTo extends Op;
object equality extends Op;
object lessThen extends Op;
object moreThen extends Op;
object and extends Op;
object follows extends Op
case class Assertion[T](c:Condition,v:T){
  
}
case class OneToOneMapping(f:Any,op:Op,s:Any) extends Entity[OneToOneMapping] with PropertyMapping{
  
   
}
case class Condition(f:Any,op:Op,s:Any) extends Entity[Condition]{
  def ->[S<:Condition](q:Condition)=Condition(this,follows,q);
  def ->[T](q:T)=Assertion[T](this,q);
  
  def <=> (q:Condition):OneToOneMapping = OneToOneMapping(this,equality,q);//REWORK IT
  def && (q:Condition):Condition = Condition(this,and,q);
  def || (q:Condition):Condition = Condition(this,and,q);
}
object otherwise extends Condition(null,equality,null){
}
object $ extends Value(null){
  
}

class OneOfTypes[CommonInterface<:Type](private val alternativeTypes:CommonInterface*)extends Type {
  def descriminator(assertions:Assertion[Type]*):Unit={};
  def name()="";
  def superType=null;
  /**
   * returns special object proxy which automatically wraps descriminator assertions 
   * simplest implementation runtime proxy 
   */
  def selectActual():CommonInterface with Value=this.asInstanceOf[CommonInterface with Value];
}

class EnumType( private val of:Type) extends Type{
  
  def name()=getClass.getName();

  def superType()=null;
  
  private var vals:Seq[Any]=List();
  
  protected def values(t:Any*){vals=t}
  
  def values()=vals;
}

object Str extends MetaType("String from a real world");
object PositiveInt extends MetaType("String from a real world");
object NumberType extends MetaType("Number from a real world");
object RegExp extends MetaType("Reg exp",Str)
object BooleanType extends MetaType("Boolean")
object Enum extends MetaType("Comma separated values",Str)