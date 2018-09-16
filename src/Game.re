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
  | Lost
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
  moves: list(direction)
};

let initial_state: state = {
  snake: [|
    { y: 1, x: 5, direction: Right },
    { y: 1, x: 4, direction: Right },
    { y: 1, x: 3, direction: Right },
    { y: 1, x: 2, direction: Right },
    { y: 1, x: 1, direction: Right }
  |],
  game_status: New,
  score: 0,
  high_score: 0,
  moves: []
};