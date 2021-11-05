package exercises.dataprocessing

object JsonExercises {

  sealed trait Json

  case class JsonNumber(number: Double) extends Json

  case class JsonString(text: String) extends Json

  case class JsonObject(obj: Map[String, Json]) extends Json

  def trimAll(json: Json): Json =
    json match {
      case _: JsonBoolean | _: JsonNumber | JsonNull => json
      case JsonArray(array) =>
        val newArray = array.map(trimAll)
        JsonArray(newArray)
      case JsonString(str) => JsonString(str.trim)
      case JsonObject(obj) =>
        val newObj = obj.map {
          case (key, value) => (key, trimAll(value))
        }
        JsonObject(newObj)
    }

  // a. Implement `anonymize`, a method which keeps the structure of the JSON document
  // but removes all data such as:
  // * all `JsonString` are replaced by `***`.
  // * all `JsonNumbers` are replaced by 0
  // For example:
  // {                                          {
  //  "name": "John Doe",                         "name": "***",
  //  "age": 25,                                  "age": 0,
  //  "address": {                                "address": {
  //    "street": {             anonymize           "street": {
  //      "number" : 12,           ==>                "number" : 0,
  //      "name" : "Cody road"                        "name" : "***"
  //    },                                          },
  //    "country": "UK",                            "country": "***",
  //  }                                           }
  //}                                           }
  def anonymize(json: Json): Json =
    json match {
      case JsonBoolean(_) => JsonBoolean(false)
      case JsonNull => json
      case JsonArray(array) => val newArray = array.map(anonymize)
        JsonArray(newArray)
      case JsonNumber(_) => JsonNumber(0)
      case JsonString(_) => JsonString("***")
      case JsonObject(obj: Map[String, Json]) =>
        val newObj = obj.map {
          case (key, value) => (key, anonymize(value))
        }
        JsonObject(newObj)
    }

  // b. Implement `search`, a method that checks if a JSON document contains a text.
  // Note: `search` doesn't look inside of the keys of a `JsonObject`, only the values.
  // For example:
  // * search({ }, "ll") == false
  // * search(5, "ll") == false
  // * search("Hello", "ll") == true
  // * search({ "message" : "hello" }, "ll") == true
  // * search({ "message" : "hi" }, "ll") == false
  def search(json: Json, text: String): Boolean =
    json match {
      case JsonBoolean(_) => false
      case JsonNumber(_) => false
      case JsonNull => false
      case JsonArray(array) => array.exists(x => search(x, text))
      case JsonString(str) => str.contains(text)
      case JsonObject(obj) => obj.values.exists(x => search(x, text))
    }

  // c. Implement `depth`, a method that calculates the maximum level of nesting of a JSON document.
  // For example:
  // * { }, 5 or "hello" have depth 0
  // * { "name" : "john" } has depth 1
  // * { "name" : "john", "address" : { "postcode" : "E16 4SR" } } has depth 2
  def depth(json: Json): Int = {
    def loop(input: Json, acc: Int = 0): Int = {
      input match {
        case _: JsonString | _: JsonBoolean | _: JsonNumber | JsonNull => acc
        case JsonArray(array) => array
          .map(x => loop(x, acc + 1))
          .maxOption match {
          case Some(maxDepth) => maxDepth
          case None => 0
        }
        case JsonObject(obj) =>
          obj
            .values
            .map(x => loop(x, acc + 1))
            .maxOption match {
            case Some(maxDepth) => maxDepth
            case None => 0
          }
      }
    }

    loop(json)
  }

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // d. Add the missing cases to JSON enumeration: JsonBoolean, JsonArray and JsonNull.
  case class JsonBoolean(bool: Boolean) extends Json

  case object JsonNull extends Json

  case class JsonArray(array: List[Json]) extends Json


  // e. add an extra parameter to search so that it limits the depth of the search.
  // such as search({ "user" : { "name" : "John" } }, "o", 2) == true
  // but     search({ "user" : { "name" : "John" } }, "o", 1) == false because "John" is at depth 2

  def search(json: Json, searchText: String, maxDepth: Int): Boolean = {
    def loop(input: Json, depth: Int = maxDepth): Boolean =
      input match {
        case _: JsonBoolean | _: JsonNumber | JsonNull => false
        case JsonString(text) => text.contains(searchText)
        case JsonObject(obj) => if (depth <= 0) false else obj.values.exists(x => loop(x, depth - 1))
        case JsonArray(array) => if (depth <= 0) false else array.exists(x => loop(x, depth - 1))

      }

    loop(json)
  }
}