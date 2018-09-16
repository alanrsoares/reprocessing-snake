open Reprocessing;
open Game;

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
      Reducers.reduce(state, Move(direction));
    }
    | _ => state
    };
  }
  | _ => state;
  };
};

let keyPressed = (state: state, env) =>
  switch (Env.keyCode(env)) {
  | Events.Space => Reducers.reduce(state, TogglePause)
  | Events.Up | Events.W => Reducers.reduce(state, AddMoveToQueue(Up))
  | Events.Down | Events.S => Reducers.reduce(state, AddMoveToQueue(Down))
  | Events.Left | Events.A => Reducers.reduce(state, AddMoveToQueue(Left))
  | Events.Right | Events.D => Reducers.reduce(state, AddMoveToQueue(Right))
  | _ => state
  };

run(
  ~setup=setup(initial_state), 
  ~draw,
  ~keyPressed,
  ()
);