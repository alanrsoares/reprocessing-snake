open Game;

open Helpers;

let move_block = (direction, block) =>
  switch direction {
  | Up => {...block, direction, y: safe_index(block.y - 1)}
  | Down => {...block, direction, y: safe_index(block.y + 1)}
  | Right => {...block, direction, x: safe_index(block.x + 1)}
  | Left => {...block, direction, x: safe_index(block.x - 1)}
  };

let hd = (snake) => snake[0];

let tl = (snake) =>
  switch (Array.length(snake)) {
  | 1 => [||]
  | 2 => [|snake[1]|]
  | _ => Array.sub(snake, 1, Array.length(snake) - 2)
  };

let drop_last = (snake) => Array.sub(snake, 0, Array.length(snake) - 1);

let last = (snake) => snake[Array.length(snake) - 1];

let move = (direction, snake) =>
  Array.concat([[|move_block(direction, hd(snake))|], drop_last(snake)]);

let has_collided_with_self = (snake) => is_collision(hd(snake), tl(snake));

let has_eaten = (food, snake) => is_collision(hd(snake), [|food|]);

let grow = (snake) => {
  let last_block = snake |> last;
  let new_block = {
    ...move_block(opposite_direction(last_block.direction), last_block),
    direction: last_block.direction
  };
  Array.concat([snake, [|new_block|]])
};
