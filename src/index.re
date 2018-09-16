open Reprocessing;

module Helpers {
  let time_elapsed = t => Unix.gettimeofday() -. t;

  let compose = (fns, env) => fns |> List.iter(f => f(env));
};

module Config = {
  let board_size = 600;

  let padding = 1;

  let tiles = 60;

  let tile_size = (board_size / tiles) - padding;
};

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

let drop_last = xs => Array.sub(xs, 0, Array.length(xs) - 1);

let safe_index = n => {
  if (n > Config.tiles - 1) {
    0;
  } else if (n < 0) {
    Config.tiles - 1;
  } else {
    n;
  }
};

module Snake {
  let move_block = (direction, block) => {
    switch direction {
    | Up => { ...block, direction, y: safe_index(block.y - 1) }
    | Down => { ...block, direction, y: safe_index(block.y + 1) }
    | Right => { ...block, direction, x: safe_index(block.x + 1) }
    | Left => { ...block, direction, x: safe_index(block.x - 1) }
    };
  };
  
  let move = (direction: direction, snake: snake) => {
    let head = snake[0];
  
    Array.concat([ [| move_block(direction, head) |], drop_last(snake) ]);
  };
};

module Reducers {
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
    }
};

let reducer = (state, action) => {
  switch action {
  | Reset => initial_state
  | _ => {
    ...state,
    snake: Reducers.snake(state.snake, action),
    moves: Reducers.moves(state.moves, action),
    game_status: Reducers.game_status(state.game_status, action)
  }
  };
};

let setup = (initial_state, env) => {
  Env.size(~width=Config.board_size, ~height=Config.board_size, env);

  initial_state;
};

let draw_block = (env, i, block) => {
  let is_head = i === 0;

  let fill_color = is_head 
  ? Utils.color(~r=100, ~g=150, ~b=100, ~a=255)
  : Utils.color(~r=150, ~g=100, ~b=150, ~a=255);

  env |> Draw.(
    Helpers.compose([
      fill(fill_color),
      rect(
        ~pos=(
          block.x * (Config.tile_size + Config.padding),
          block.y * (Config.tile_size + Config.padding),
        ), 
        ~width=Config.tile_size, 
        ~height=Config.tile_size
      ),
    ])
  );
};

let draw_bg = Draw.background(Utils.color(~r=199, ~g=217, ~b=229, ~a=255));

let draw_snake = (state, env) => state.snake |> Array.iteri(draw_block(env));

let draw_overlay = (game_status, env) => 
  switch (game_status) {
  | Paused =>
    env |> Draw.(
      Helpers.compose([
        background(Utils.color(~r=200, ~g=200, ~b=200, ~a=150)),
        tint(Utils.color(~r=255, ~g=200, ~b=200, ~a=255)),
        text(
          ~pos=(180, 240),
          ~body="GAME PAUSED!"
        ),
        noTint
      ])
    )
  | _ => ()
  };

let last_executed: ref(float) = ref(0.);

let draw = (state, env) => {
  /* draw */
  env |> Helpers.compose([
    draw_bg,
    draw_snake(state),
    switch state.game_status {
    | Paused => draw_overlay(Paused)
    | _ => (_) => ()
    }
  ]);

  /* reduce */
  switch state.game_status {
  | Playing => {
    let direction = switch state.moves {
    | [] => state.snake[0].direction
    | moves => moves |> List.hd
    };
    
    switch (last_executed^) {
    | t when Helpers.time_elapsed(t) >= 0.05 => {
      last_executed := Unix.gettimeofday();
      reducer(state, Move(direction));
    }
    | _ => state
    };
  }
  | _ => state;
  };
};

let keyPressed = (state: state, env) =>
  switch (Env.keyCode(env)) {
  | Events.Space => reducer(state, TogglePause)
  | Events.Up | Events.W => reducer(state, AddMoveToQueue(Up))
  | Events.Down | Events.S => reducer(state, AddMoveToQueue(Down))
  | Events.Left | Events.A => reducer(state, AddMoveToQueue(Left))
  | Events.Right | Events.D => reducer(state, AddMoveToQueue(Right))
  | _ => state
  };

run(
  ~setup=setup(initial_state), 
  ~draw,
  ~keyPressed,
  ()
);