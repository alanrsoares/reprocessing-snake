open Game;

let snake = (snake, action) =>
  switch action {
  | Move(d) => snake |> Snake.move(d)
  | Reset => initial_state.snake
  | _ => snake
  };
  
let moves = (moves, action) =>
  switch action {
  | AddMoveToQueue(direction) => [direction] @ moves
  | Move(_) when List.length(moves) > 0 => moves |> List.tl
  | Reset => initial_state.moves
  | _ => moves
  };

let game_status = (game_status, action) =>
  switch action {
  | TogglePause => game_status == Playing ? Paused : Playing
  | Reset => initial_state.game_status
  | _ => game_status
  };

let reduce = (state, action) => {
  switch action {
  | Reset => initial_state
  | _ => {
    ...state,
    snake: snake(state.snake, action),
    moves: moves(state.moves, action),
    game_status: game_status(state.game_status, action)
  }
  };
};