open Reprocessing;

open Game;
open Helpers;

let setup = (initial_state, env) => {
  Env.size(~width=Config.board_size, ~height=Config.board_size, env);
  {
    ...initial_state,
    font: Some(Draw.loadFont(~filename="assets/font16_1x.fnt", env)),
  };
};

let draw_block = (env, i, block) => {
  let is_head = i === 0;
  let fill_color =
    Theme.Colors.(
      is_head ? snakeHead : i mod 2 != 0 ? snakeEvenBlock : snakeOddBlock
    );

  env
  ->chain(
      Draw.[
        fill(fill_color),
        rect(
          ~pos=(
            block.x * (Config.tile_size + Config.padding),
            block.y * (Config.tile_size + Config.padding),
          ),
          ~width=Config.tile_size,
          ~height=Config.tile_size,
        ),
      ],
    );
};

let draw_background = Draw.background(Theme.Colors.background);

let draw_snake = (snake, env) => snake |> Array.iteri(env->draw_block);

let draw_food = (food, env) => env->draw_block(1, food);

let draw_score = (score, font) =>
  Draw.text(
    ~pos=(10, Config.board_size - 10),
    ~body="SCORE: " ++ score->string_of_int,
    ~font,
  );

let draw_overlay = (state, font, env) =>
  Draw.(
    switch (state.game_status) {
    | New =>
      env
      ->chain([
          background(Theme.Colors.overlay),
          text(~pos=(10, 280), ~body="PRESS SPACE TO START", ~font),
        ])
    | Paused =>
      env
      ->chain([
          background(Theme.Colors.overlay),
          text(~pos=(120, 280), ~body="GAME PAUSED", ~font),
        ])
    | GameOver =>
      env
      ->chain([
          background(Theme.Colors.overlay),
          text(~pos=(160, 280), ~body="GAME OVER", ~font),
        ])
    | _ => env |> draw_score(state.score, font)
    }
  );

let draw = (state, env) => {
  let font =
    switch (state.font) {
    | Some(font') => font'
    | _ => failwith("no-font")
    };

  /* draw */
  env
  ->chain([
      draw_background,
      draw_snake(state.snake),
      draw_food(state.food),
      draw_overlay(state, font),
    ]);

  /* reduce */
  switch (state.game_status) {
  | Playing =>
    let direction =
      switch (state.moves) {
      | [] => state.snake->Snake.direction
      | moves => moves->List.hd
      };
    state->Reducers.reduce(Move(direction));
  | _ => state
  };
};

let keyPressed = (state: state, env) =>
  switch (Env.keyCode(env)) {
  | Events.Space => state->Reducers.reduce(TogglePause)
  | Events.Up
  | Events.W => state->Reducers.reduce(AddMoveToQueue(Up))
  | Events.Down
  | Events.S => state->Reducers.reduce(AddMoveToQueue(Down))
  | Events.Left
  | Events.A => state->Reducers.reduce(AddMoveToQueue(Left))
  | Events.Right
  | Events.D => state->Reducers.reduce(AddMoveToQueue(Right))
  | _ => state
  };

run(~setup=setup(initial_state), ~draw, ~keyPressed, ());