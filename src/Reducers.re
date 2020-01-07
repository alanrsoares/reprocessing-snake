open Game;

let reduce_move = (direction, state) =>
  switch (Helpers.time_elapsed(state.last_moved_timestamp) >= Config.speed) {
  | true =>
    let next_state = {
      ...state,
      last_moved_timestamp: Unix.gettimeofday(),
      snake: state.snake->Snake.move(direction),
      moves:
        List.length(state.moves) > 0 ? state.moves->List.tl : state.moves,
    };
    switch (next_state.snake->Snake.detect_collision(state.food)) {
    | None => next_state
    | Some(Tail) => {...next_state, game_status: GameOver}
    | Some(Food) => {
        ...next_state,
        score: state.score + 1,
        food: next_state.snake->spawn_food,
        snake: next_state.snake->Snake.grow,
      }
    };
  | _ => state
  };

let reduce_toggle_pause = state =>
  switch (state.game_status) {
  | New => {...state, game_status: Playing}
  | Playing => {...state, game_status: Paused}
  | Paused => {...state, game_status: Playing}
  | GameOver => {...initial_state, font: state.font}
  };

let reduce = (state, action) =>
  switch (action) {
  | Reset => initial_state
  | Move(direction) => reduce_move(direction, state)
  | TogglePause => reduce_toggle_pause(state)
  | AddMoveToQueue(direction) when is_legal_move(direction, state) => {
      ...state,
      moves: [direction] @ state.moves,
    }
  | _ => state
  };