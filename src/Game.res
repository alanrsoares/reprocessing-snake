type direction =
  | Up
  | Down
  | Left
  | Right

type block = {
  y: int,
  x: int,
  direction: direction,
}

type gameStatus =
  | GameOver
  | Paused
  | Playing
  | New

type snake = array<block>

type action =
  | TogglePause
  | Start
  | Reset
  | Move(direction)
  | AddMoveToQueue(direction)

type state = {
  snake: snake,
  gameStatus: gameStatus,
  score: int,
  highScore: int,
  moves: list<direction>,
  food: block,
  lastMovedTimestamp: float,
  font: option<Reprocessing.fontT>,
}

type collision =
  | Food
  | Tail

let oppositeDirection = direction =>
  switch direction {
  | Up => Down
  | Down => Up
  | Left => Right
  | Right => Left
  }

let isLegalMove = (direction, state) =>
  state.gameStatus === Playing && direction != oppositeDirection(state.snake[0].direction)

let isCollision = (a: block, snake) =>
  snake->Array.to_list |> List.exists(b => b.x == a.x && b.y == a.y)

let rec spawnFood = snake => {
  let newFood = {
    x: Reprocessing.Utils.random(~min=0, ~max=Config.tiles),
    y: Reprocessing.Utils.random(~min=0, ~max=Config.tiles),
    direction: Right,
  }
  if isCollision(newFood, snake) {
    snake->spawnFood
  } else {
    newFood
  }
}

let initialSnake = [
  {y: 1, x: 6, direction: Right},
  {y: 1, x: 5, direction: Right},
  {y: 1, x: 4, direction: Right},
  {y: 1, x: 3, direction: Right},
  {y: 1, x: 2, direction: Right},
  {y: 1, x: 1, direction: Right},
]

let initialState: state = {
  snake: initialSnake,
  gameStatus: New,
  score: 0,
  highScore: 0,
  moves: list{},
  food: initialSnake->spawnFood,
  lastMovedTimestamp: 0.,
  font: None,
}
