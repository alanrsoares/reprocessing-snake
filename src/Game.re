type direction =
  | Up
  | Down
  | Left
  | Right;

type block = {
  y: int,
  x: int,
  direction
};

type game_status =
  | GameOver
  | Paused
  | Playing
  | New;

type snake = array(block);

type action =
  | TogglePause
  | Start
  | Reset
  | Move(direction)
  | AddMoveToQueue(direction);

type state = {
  snake,
  game_status,
  score: int,
  high_score: int,
  moves: list(direction),
  food: block,
  last_moved_timestamp: float,
  font: option(Reprocessing.fontT)
};

type collision =
  | Food
  | Tail;

let opposite_direction =
  fun
  | Up => Down
  | Down => Up
  | Left => Right
  | Right => Left;

let is_legal_move = (direction, state) =>
  state.game_status === Playing
  && direction != opposite_direction(state.snake[0].direction);

let is_collision = (a: block, snake) =>
  snake |> Array.to_list |> List.exists((b) => b.x == a.x && b.y == a.y);

let rec spawn_food = (snake) => {
  let new_food = {
    x: Reprocessing.Utils.random(~min=0, ~max=Config.tiles),
    y: Reprocessing.Utils.random(~min=0, ~max=Config.tiles),
    direction: Right
  };
  if (is_collision(new_food, snake)) {
    spawn_food(snake)
  } else {
    new_food
  }
};

let initial_snake = [|
  {y: 1, x: 6, direction: Right},
  {y: 1, x: 5, direction: Right},
  {y: 1, x: 4, direction: Right},
  {y: 1, x: 3, direction: Right},
  {y: 1, x: 2, direction: Right},
  {y: 1, x: 1, direction: Right}
|];

let initial_state: state = {
  snake: initial_snake,
  game_status: New,
  score: 0,
  high_score: 0,
  moves: [],
  food: spawn_food(initial_snake),
  last_moved_timestamp: 0.,
  font: None
};