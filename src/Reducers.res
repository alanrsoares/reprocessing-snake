open Game

let reduceMove = (direction, state) => {
  let timeElapsed = Helpers.timeElapsed(state.lastMovedTimestamp)

  if timeElapsed >= Config.speed {
    let nextState = {
      ...state,
      lastMovedTimestamp: Js.Date.now(),
      snake: state.snake->Snake.move(direction),
      moves: List.length(state.moves) > 0 ? state.moves->List.tl : state.moves,
    }
    switch nextState.snake->Snake.detectCollision(state.food) {
    | None => nextState
    | Some(Tail) => {...nextState, gameStatus: GameOver}
    | Some(Food) => {
        ...nextState,
        score: state.score + 1,
        food: nextState.snake->spawnFood,
        snake: nextState.snake->Snake.grow,
      }
    }
  } else {
    state
  }
}

let reduceTogglePause = state =>
  switch state.gameStatus {
  | New => {...state, gameStatus: Playing}
  | Playing => {...state, gameStatus: Paused}
  | Paused => {...state, gameStatus: Playing}
  | GameOver => {...initialState, font: state.font}
  }

let reduce = (state, action) =>
  switch action {
  | Reset => initialState
  | Move(direction) => reduceMove(direction, state)
  | TogglePause => reduceTogglePause(state)
  | AddMoveToQueue(direction) when isLegalMove(direction, state) => {
      ...state,
      moves: List.cons(direction, state.moves),
    }
  | _ => state
  }
