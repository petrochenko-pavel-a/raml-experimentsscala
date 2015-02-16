import org.raml.model.diet.UriParameter
import org.raml.model.diet.QueryParameter

trait TypeModel[+B] {
}

object StringType extends TypeModel[String];

object RefType extends TypeModel[Seq[_]];

trait Named {
  def name(): String
}
trait Property[+B] extends Named {

}

case class ChildElementProperty[B](val name: String) extends Property[Seq[B]] {
  def |(v: ChildElementProperty[_]): ChildElementProperty[B] = null;
}

case class StringProperty(val name: String) extends Property[String] {
  def range = StringType;
}

case class TypeProperty(val name: String) extends Property[String] {
  def range = StringType;
}

object Api {
  val title = StringProperty("title");
  val version = StringProperty("version");
  val baseUrl = StringProperty("baseUrl");
  val mediaType = StringProperty("mediaType");
}
object Method {

}
object Resource {
  val displayName = StringProperty("displayName");
  val description = StringProperty("description");
  val typeProperty = TypeProperty("type");

  val child = ChildElementProperty[Resource.type]("resources") | 
              ChildElementProperty[Method.type]("actions")| 
              ChildElementProperty[UriParameter.type]("uriParams")|
              ChildElementProperty[QueryParameter.type]("queryParams")              
}

trait Api {
  def title: StringProperty
  def version: StringProperty
  def baseUrl: StringProperty
  def mediaType: StringProperty
}