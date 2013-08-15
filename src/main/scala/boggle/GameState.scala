object GameState {
  sealed trait State
  case object Setup extends State
  case object Running extends State
  case object Finished extends State
  
  trait GameState {
    var state = Setup
    var startTime = None

    def timeLeft(): Int
  }
}
