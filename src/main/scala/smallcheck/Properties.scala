package smallcheck
import scala.collection.mutable.ListBuffer

/**
 * A collection of properties, which itself is a property.
 */
class Properties(val name: String) extends Property {
  import Property._
  
  private val props = new ListBuffer[(String, Property)]
  
  private def oneProperty: Property = all((props.map(_._2)):_*)
  
  def apply(d: Int): Seq[TestCase] = oneProperty(d)
  
  def properties: Seq[(String, Property)] = props
  
  def include(ps: Properties) = for ((n, p) <- ps.properties) property(n) = p
  
  class PropertySpecifier() {
    def update(propName: String, p: Property) = props += ((name+"."+propName, p))
  }
  
  lazy val property = new PropertySpecifier()
}
