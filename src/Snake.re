open Game;
open Helpers;

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
  
  Array.concat([ 
    [| move_block(direction, head) |], 
    Helpers.drop_last(snake) 
  ]);
};