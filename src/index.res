open Reprocessing

open Game
open Helpers

let setup = (initialState, env) => {
  open Config
  env |> Env.size(~width=boardSize, ~height=boardSize)
  {
    ...initialState,
    font: Some(env |> Draw.loadFont(~filename="assets/font16_1x.fnt", ~isPixel=false)),
  }
}

let drawFood = (env, block) => {
  let radius = Config.tileSize / 2
  let spacing = Config.tileSize + Config.padding
  let offset = x => x * spacing + radius

  env->chain([
    Draw.fill(Theme.Colors.red),
    Draw.ellipse(~center=(offset(block.x), offset(block.y)), ~radx=radius, ~rady=radius),
  ])
}

let drawBlock = (env, i, block) => {
  open Theme.Colors
  open Draw

  let isHead = i === 0
  let isEven = i->mod(2) != 0
  let fillColor = isHead ? black : isEven ? red : orange
  let offset = Config.tileSize + Config.padding

  env->chain([
    fill(fillColor),
    rect(
      ~pos=(block.x * offset, block.y * offset),
      ~width=Config.tileSize,
      ~height=Config.tileSize,
    ),
  ])
}

let drawBackground = Draw.background(Theme.Colors.background)

let drawSnake = (snake, env) => snake |> Array.iteri(env->drawBlock)

let drawFood = (food, env) => env->drawFood(food)

let drawScore = (score, font) =>
  Draw.text(~pos=(10, Config.boardSize - 10), ~body="SCORE: " ++ score->string_of_int, ~font)

let drawOverlay = (state, font, env) => {
  open Draw
  switch state.gameStatus {
  | New =>
    env->chain([
      background(Theme.Colors.overlay),
      text(~pos=(10, 280), ~body="PRESS SPACE TO START", ~font),
    ])
  | Paused =>
    env->chain([
      background(Theme.Colors.overlay),
      text(~pos=(120, 280), ~body="GAME PAUSED", ~font),
    ])
  | GameOver =>
    env->chain([background(Theme.Colors.overlay), text(~pos=(160, 280), ~body="GAME OVER", ~font)])
  | _ => env |> drawScore(state.score, font)
  }
}

let draw = (state, env) => {
  let font = switch state.font {
  | Some(font') => font'
  | _ => failwith("no-font")
  }

  /* draw */
  env->chain([
    drawBackground,
    drawSnake(state.snake),
    drawFood(state.food),
    drawOverlay(state, font),
  ])

  /* reduce */
  switch state.gameStatus {
  | Playing =>
    let direction = switch state.moves {
    | list{} => state.snake->Snake.direction
    | moves => moves->List.hd
    }
    state->Reducers.reduce(Move(direction))
  | _ => state
  }
}

let keyPressed = (state: state, env) =>
  switch Env.keyCode(env) {
  | Events.Space => state->Reducers.reduce(TogglePause)
  | Events.Up
  | Events.W =>
    state->Reducers.reduce(AddMoveToQueue(Up))
  | Events.Down
  | Events.S =>
    state->Reducers.reduce(AddMoveToQueue(Down))
  | Events.Left
  | Events.A =>
    state->Reducers.reduce(AddMoveToQueue(Left))
  | Events.Right
  | Events.D =>
    state->Reducers.reduce(AddMoveToQueue(Right))
  | _ => state
  }

run(~setup=setup(initialState), ~draw, ~keyPressed, ())
