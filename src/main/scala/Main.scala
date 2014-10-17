object Main extends App {

  @fieldNames
  case class Person(name: String, age: Int)

  @fieldNames
  class Address(street: List[String])

  println(s"${Person.name} ${Person.age} ${Address.street}")
}
