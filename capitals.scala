class Player(val name: String, val pos: Position, val number: Int):
    override def toString = s"#$number $name $pos"

val players = Set(
    Player("Anthony Beauvillier", W, 72),
    Player("Nic Dowd", C, 26),
    Player("Pierre-Luc Dubois", W, 80),
    Player("Brandon Duhaime", W, 22),
    Player("Ethen Frank", C, 53),
    Player("Hendrix Lapierre", C, 29),
    Player("Ryan Leonard", W, 9),
    Player("Connor McMichael", C, 24),
    Player("Sonny Milano", W, 15),
    Player("Alex Ovechkin", W, 8),
    Player("Aliaksei Protas", C, 21),
    Player("Justin Sourdif", W, 34),
    Player("Dylan Strome", C, 17),
    Player("Tom Wilson", W, 43),
    Player("John Carlson", D, 74),
    Player("Declan Chisholm", D, 47),
    Player("Jakob Chychrun", D, 6),
    Player("Martin Fehervary", D, 42),
    Player("Dylan McIllwrath", D, 52),
    Player("Matt Roy", D, 3),
    Player("Rasmus Sandin", D, 38),
    Player("Trevor van Riemsdyk", D, 57),
    Player("Charlie Lindgren", G, 79),
    Player("Logan Thompson", G, 48)
)

import util.Try
enum Position: 
    case C, W, D, G
import Position.* 
enum Mode(val execute: () => Unit, val description: String = "", val hidden: Boolean=false):
    case Number extends Mode(runNumbers, "given a player's number provide their name and position")
    case Name extends Mode(runNames, "given a player's name provide their number and position")
    case Unknown extends Mode(runUnknown, hidden=true)
object Mode:
    def valueOfOption(str: String): Option[Mode] =
        try Some(Mode.valueOf(str.capitalize))
        catch 
            case ex: IllegalArgumentException => None

enum Command(val execute: (() => Unit) => Unit, val info: String):
    case Help extends Command(help, "lists all available commands")
    case Modes extends Command(listModes, "lists all playable game modes")
    case Quit extends Command(e => (), "exits the program (same as exit)")
    case Exit extends Command(e => (), "quits the program (same as quit)")
    case New extends Command(e => {println(); Mode.Unknown.execute()}, "change modes")
    case List extends Command(e => listPlayers(), "quits current mode and lists all players on roster")

val maxWordsForName = players.map(_.name.split(" ").length).foldLeft(0)(Math.max)
val minWordsForName = players.map(_.name.split(" ").length).foldLeft(maxWordsForName)(Math.min)

def pullRandom: Player = players.iterator.drop(util.Random.nextInt(players.size)).next
def readInput: String = io.StdIn.readLine.toLowerCase

def help(execute: () => Unit): Unit =
    println("\n~~List of Commands~~")
    Command.values.map(c => s"${c.toString.toLowerCase} - ${c.info}").sorted.foreach(println)
    println()
    execute()

def listModes(execute: () => Unit): Unit =
    println("\ncommand 'new' allows you to change modes")
    println("~~List of Modes~~")
    Mode.values.filter(!_.hidden).map(m => s"${m.toString.toLowerCase} - ${m.description}").sorted.foreach(println)
    println()
    execute()

def listPlayers(): Unit =
    def printPositionHeader(pos: String) = println("-"*15 + pos + "-"*15)
    println("All Players on the Washington Capitals")
    val playersByPos = players.groupMap(_.pos)(_.toString)
    printPositionHeader("Forwards")
    playersByPos(C) union playersByPos(W) foreach println
    printPositionHeader("Defensemen")
    playersByPos(D) foreach println
    printPositionHeader("Goalies")
    playersByPos(G) foreach println
    println()
    Mode.Unknown.execute()

def parseInputForCommands(execute: () => Unit)(block: String => Unit): Unit =
    val input = readInput
    try 
        val command = Command.valueOf(input.toLowerCase.capitalize)
        command.execute(execute)
    catch
        case ex: IllegalArgumentException => block(input)

def runUnknown(): Unit =
    println("Input Mode: (" + Mode.values.filter(!_.hidden).map(_.toString.toLowerCase).mkString(", ") + ")")
    parseInputForCommands(runUnknown): input =>
        Mode.valueOfOption(input) match
            case Some(mode) => 
                println()
                mode.execute()
            case None =>
                println("Unrecognized mode...\n")
                runUnknown()

def runNumbers(): Unit =
    def runNumbersGivenPlayer(player: Player): Unit =
        println(s"Number: ${player.number}")
        parseInputForCommands(() => runNumbersGivenPlayer(player)): input =>
            input.split(" ").toList.reverse match
                case position :: nameReversed if Try(Position.valueOf(position.toUpperCase)).isSuccess
                        && minWordsForName <= nameReversed.size && nameReversed.size <= maxWordsForName  =>
                    val correct = player.pos == Position.valueOf(position.toUpperCase) && player.name.toLowerCase == nameReversed.reverse.mkString(" ").toLowerCase
                    println(if correct then "Correct!\n" else s"Incorrect. Number ${player.number} is ${player.name} ${player.pos}\n")
                    runNumbersGivenPlayer(pullRandom)
                case _ =>
                    println("Invalid formatting. Input should be of format {player name} {position (C/W/D/G)}")
                    runNumbersGivenPlayer(player)
    println("Numbers game: A number will be given and you must respond with {player name} {position (C/W/D/G)}")
    runNumbersGivenPlayer(pullRandom)

def runNames(): Unit =
    def runNamesGivenPlayer(player: Player): Unit =
        println(player.name)
        parseInputForCommands(() => runNamesGivenPlayer(player)): input =>
            input.split(" ").toList match
                case List(number, position) if Try((number.toInt, Position.valueOf(position.toUpperCase))).isSuccess =>
                    val correct = player.number == number.toInt && player.pos == Position.valueOf(position.toUpperCase)
                    println(if correct then "Correct!\n" else s"Incorrect. Corrrect answer for ${player.name} is ${player.number} ${player.pos}")
                    runNamesGivenPlayer(pullRandom)
                case _ =>
                    println("Invalid formatting. Input should be of format {number} {position(C/W/D/G)}")
                    runNamesGivenPlayer(player)
    println("Names game: A player's name will be given and you must respond with {number} {position(C/W/D/G)}")
    runNamesGivenPlayer(pullRandom)

@main def main(args: String*) = 
    println("Capitals player knowledge game. Type \"help\" at any time for list of commands\n")
    Mode.Unknown.execute()