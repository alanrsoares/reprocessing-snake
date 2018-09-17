open Game;

let moves = (moves, action, state) =>
  switch action {
  | AddMoveToQueue(direction) when is_legal_move(direction, state) =>
    [direction] @ moves
  | _ => moves
  };

let game_status = (game_status, action) =>
  switch action {
  | TogglePause => game_status == Playing ? Paused : Playing
  | _ => game_status
  };

let reduce = (state, action) =>
  switch action {
  | Reset => initial_state
  | Move(direction) =>
    let new_state = {
      ...state,
      snake: state.snake |> Snake.move(direction),
      moves:
        List.length(state.moves) > 0 ? state.moves |> List.tl : state.moves
    };
    if (new_state.snake |> Snake.has_collided_with_self) {
      {...new_state, game_status: GameOver}
    } else if (new_state.snake |> Snake.has_eaten(state.food)) {
      {
        ...new_state,
        score: state.score + 1,
        food: spawn_food(new_state.snake),
        snake: new_state.snake |> Snake.grow
      }
    } else {
      new_state
    }
  | TogglePause =>
    switch state.game_status {
    | New => {...state, game_status: Playing}
    | Playing => {...state, game_status: Paused}
    | Paused => {...state, game_status: Playing}
    | GameOver => initial_state
    }
  | _ => {
      ...state,
      moves: moves(state.moves, action, state),
      game_status: game_status(state.game_status, action)
    }
  };