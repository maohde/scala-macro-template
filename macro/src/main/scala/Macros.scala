import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

class fieldNames extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro fieldNamesMacro.impl
}

object fieldNamesMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import termNames.CONSTRUCTOR
    import typeNames.EMPTY

    val inputs = annottees.map(_.tree).toList
    val (expanded, objName, accessorFields) = inputs match {
      case (param: ClassDef) :: (tail)  => {
        val constructAccessor = param.collect {
          case (values: ValDef)
            if values.mods.hasFlag(Flag.PARAMACCESSOR) &&
              !values.mods.hasFlag(Flag.PRIVATE) &&
              !values.mods.hasFlag(Flag.CASEACCESSOR) => {
            values
          }
        }
        val stringDef = constructAccessor.map { field ⇒
          val name = field.name.toTermName
          val decoded = name.decodedName.toString
          q"val $name: String = $decoded"
        }
        (inputs, Some(param.name.decodedName.toString), Some(stringDef))
      }
      case (_) => (inputs, None, None)
    }

    val companionObject = for {
      accessorFieldsStringValues <- accessorFields
      if accessorFieldsStringValues.length > 0
    } yield {
      val constructorBody = Block(List(Apply(Select(Super(This(EMPTY), EMPTY), CONSTRUCTOR), List())), Literal(Constant(())))
      val constructor = DefDef(NoMods, CONSTRUCTOR, List(), List(List()), TypeTree(), constructorBody)
      val objInheritance = List(Ident(TypeName("AnyRef")))
      ModuleDef(NoMods, TermName(objName.getOrElse("Zombie")),
        Template(objInheritance, noSelfType, constructor :: accessorFieldsStringValues))
    }

    val completeBlock = companionObject match {
      case Some(x) => Block(expanded :+ x,Literal(Constant(())))
      case _ => Block(expanded, Literal(Constant(())))
    }
    c.Expr[Any](completeBlock)
  }
}

