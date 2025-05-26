
import io.circe._, io.circe.generic.auto._
import com.github.plokhotnyuk.jsoniter_scala.circe.CirceCodecs.* 

import com.github.plokhotnyuk.jsoniter_scala.circe._

val jsonStr = """{"name":"Peter","age":25}"""

// Use Jsoniter's parser but still get a Circe AST
val parsed: Either[ParsingFailure, Json] = io.circe.parser.parse(jsonStr)

val parsedJson: Json = parsed.getOrElse(Json.Null)

import io.circe.parser._

// string => circe AST(io.circe.Json)
val input = """{"name":"Alice","age":30}"""
val json: Either[ParsingFailure, io.circe.Json] = parse(input)

import io.circe.generic.auto._
import io.circe.syntax._

case class Person(name: String, age: Int)

// circe AST(io.circe.Json) => case class
val decoded: Either[io.circe.Error, Person] = json.flatMap(_.as[Person])
//Uses implicit Decoder[Person], which is derived automatically in this case.



val person = Person("Alice", 30)
val jsonFromPerson: io.circe.Json = person.asJson

import io.circe.Printer

val jsonString: String = Printer.noSpaces.print(jsonFromPerson)

jsonFromPerson.noSpaces

import com.github.plokhotnyuk.jsoniter_scala.circe.JsoniterScalaCodec
import com.github.plokhotnyuk.jsoniter_scala.circe.CirceCodecs.zonedDateTimeC3C

import io.circe.generic.auto.*
import io.circe.{HCursor, DecodingFailure, Decoder, Json}
import io.circe.parser.*
import com.github.plokhotnyuk.jsoniter_scala.circe.JsoniterScalaCodec._
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import com.github.plokhotnyuk.jsoniter_scala.core.*
import scala.util.{Try, Success, Failure}

object Bot {

  enum Type(val value: String) {
    case Rock extends Type("Rock")
    case Assassin extends Type("Assassin")
    case Scout extends Type("Scout")
    case Tank extends Type("Tank")
    case Base extends Type("Base")
  }

  given decodeType: Decoder[Type] = Decoder.decodeString.emap { s =>
    Type.values.find(_.value == s).toRight(s"Unexpected value for Type $s")
  }

  case class Coord(x: Int, y: Int)

  enum Team(val value: String) {
    case Player1 extends Team("player1")
    case Player2 extends Team("player2")
  }

  given Decoder[Team] = Decoder.decodeString.emap { s =>
    Team.values.find(_.value == s).toRight(s"Unexpected value for Team $s")
  }

  enum MapEntry {
    case Rock(position: Coord)
    case Assassin(health: Int, team: Team, position: Coord)
    case Tank(health: Int, team: Team, position: Coord)
    case Scout(health: Int, team: Team, position: Coord)
    case Base(position: Coord)
  }

  given Decoder[MapEntry] with {
    def apply(c: HCursor) = c.get[Type]("type").flatMap {
      case Type.Rock => summon[Decoder[MapEntry.Rock]](c)
      case Type.Assassin => summon[Decoder[MapEntry.Assassin]](c)
      case Type.Scout => summon[Decoder[MapEntry.Scout]](c)
      case Type.Tank => summon[Decoder[MapEntry.Tank]](c)
      case Type.Base => summon[Decoder[MapEntry.Base]](c)
    }
  }

  case class Player(
    team: Team,
    money: Int,
    base: Coord,
    health: Int
  )

  case class State(
    map: List[MapEntry],
    you: Team,
    player1: Player,
    player2: Player
  )

  enum ReadingError {
    case Error(err: Throwable)
    case InvalidJson(err: Throwable)
    case DecodeError(err: DecodingFailure)
  }

  def states: LazyList[Either[ReadingError, State]] = {
    val next = Try(readFromStream[Json](
      System.in,
      ReaderConfig.withCheckForEndOfInput(false)
    )) match {
      case Success(json) => json.as[State].left.map(ReadingError.DecodeError(_))
      case Failure(e: JsonReaderException) => Left(ReadingError.InvalidJson(e))
      case Failure(e) => Left(ReadingError.Error(e))
    }

    next #:: states
  }

  def main(args: Array[String]): Unit = {
    println("Ready")
    Try(states.foreach {
      case Left(err) => System.err.println(err)
      case Right(state) =>
        val (me, enemy) = if state.you == Team.Player1 then (state.player1, state.player2)
          else (state.player2, state.player1)
        if me.money >= 10 then {
          val myBaseCoord = me.base
          val enemyBaseCoord = enemy.base
          println(s"Build Scout (${myBaseCoord.x}, ${myBaseCoord.y + 1}) (${enemyBaseCoord.x}, ${enemyBaseCoord.y}) Down")
        } else println("No-op")
    }).recover {
      case err => System.err.println(err)
    }
  }

}