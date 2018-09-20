open Game;

open Helpers;

let move_block = (direction, block) =>
  switch direction {
  | Up => {...block, direction, y: safe_index(block.y - 1)}
  | Down => {...block, direction, y: safe_index(block.y + 1)}
  | Right => {...block, direction, x: safe_index(block.x + 1)}
  | Left => {...block, direction, x: safe_index(block.x - 1)}
  };

let hd = (self) => self[0];

let tl =
  fun
  | [||]
  | [|_|] => [||]
  | [|_, x|] => [|x|]
  | self => Array.sub(self, 1, Array.length(self) - 2);

let direction = (self) => hd(self).direction;

let drop_last = (self) => Array.sub(self, 0, Array.length(self) - 1);

let last = (self) => self[Array.length(self) - 1];

let move = (direction, self) =>
  Array.concat([[|move_block(direction, hd(self))|], drop_last(self)]);

let has_collided_with_self = (self) => is_collision(hd(self), tl(self));

let has_eaten = (food, self) => is_collision(hd(self), [|food|]);

let detect_collision = (food, self) =>
  if (self |> has_eaten(food)) {
    Some(Food)
  } else if (self |> has_collided_with_self) {
    Some(Tail)
  } else {
    None
  };

let grow = (self) => {
  let last_block = self |> last;
  let new_block = {
    ...move_block(opposite_direction(last_block.direction), last_block),
    direction: last_block.direction
  };
  Array.concat([self, [|new_block|]])
};