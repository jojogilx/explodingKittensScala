package rooms

import players.Player

case class Room(name: String, nPlayers: Int, started: Boolean, players: List[Player])

object Room { // allow spectating or limit 5 here?
  def joinRoom(room: Room , player: Player): Room =
    room.copy(nPlayers = room.nPlayers + 1, players = player :: room.players)

  def leaveRoom(room: Room , player: Player): Room = {
    room.players.find(_ == player).fold(room)( player =>
      room.copy(nPlayers = room.nPlayers - 1 match {
        case x if x > 0 =>  x
        case _ => 0
      }, players = room.players.filterNot(_ == player)))
  }
}
