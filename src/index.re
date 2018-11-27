open Reprocessing;

open Game;

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
    is_head ?
      Utils.color(~r=1, ~g=22, ~b=39, ~a=255) :
      i mod 2 != 0 ?
        Utils.color(~r=231, ~g=29, ~b=54, ~a=255) :
        Utils.color(~r=255, ~g=159, ~b=28, ~a=255);
  env
  |> Draw.(
       Helpers.compose([
         fill(fill_color),
         rect(
           ~pos=(
             block.x * (Config.tile_size + Config.padding),
             block.y * (Config.tile_size + Config.padding),
           ),
           ~width=Config.tile_size,
           ~height=Config.tile_size,
         ),
       ])
     );
};

let draw_bg = Draw.background(Utils.color(~r=46, ~g=196, ~b=182, ~a=255));

let draw_snake = (snake, env) => snake |> Array.iteri(env->draw_block);

let draw_food = (food, env) => draw_block(env, 1, food);

let draw_score = (score, font) =>
  Draw.text(
    ~pos=(10, Config.board_size - 40),
    ~body="SCORE: " ++ score->string_of_int,
    ~font,
  );

let draw_overlay = (state, font, env) =>
  switch (state.game_status) {
  | New =>
    env
    |> Draw.(
         Helpers.compose([
           background(Utils.color(~r=204, ~g=204, ~b=204, ~a=204)),
           text(~pos=(10, 280), ~body="PRESS SPACE TO START", ~font),
         ])
       )
  | Paused =>
    env
    |> Draw.(
         Helpers.compose([
           background(Utils.color(~r=204, ~g=204, ~b=204, ~a=204)),
           text(~pos=(120, 280), ~body="GAME PAUSED", ~font),
         ])
       )
  | GameOver =>
    env
    |> Draw.(
         Helpers.compose([
           background(Utils.color(~r=204, ~g=204, ~b=204, ~a=204)),
           text(~pos=(160, 280), ~body="GAME OVER", ~font),
         ])
       )
  | _ => env |> draw_score(state.score, font)
  };

let draw = (state, env) => {
  let font =
    switch (state.font) {
    | Some(font') => font'
    | _ => failwith("no-font")
    };

  /* draw */
  env
  |> Helpers.compose([
       draw_bg,
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
    Reducers.reduce(state, Move(direction));
  | _ => state
  };
};

let keyPressed = (state: state, env) =>
  switch (Env.keyCode(env)) {
  | Events.Space => Reducers.reduce(state, TogglePause)
  | Events.Up
  | Events.W => Reducers.reduce(state, AddMoveToQueue(Up))
  | Events.Down
  | Events.S => Reducers.reduce(state, AddMoveToQueue(Down))
  | Events.Left
  | Events.A => Reducers.reduce(state, AddMoveToQueue(Left))
  | Events.Right
  | Events.D => Reducers.reduce(state, AddMoveToQueue(Right))
  | _ => state
  };

run(~setup=setup(initial_state), ~draw, ~keyPressed, ());