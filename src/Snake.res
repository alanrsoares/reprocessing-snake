open Game

open Helpers

let moveBlock = (direction, block) =>
  switch direction {
  | Up => {...block, direction: direction, y: safeIndex(block.y - 1)}
  | Down => {...block, direction: direction, y: safeIndex(block.y + 1)}
  | Right => {...block, direction: direction, x: safeIndex(block.x + 1)}
  | Left => {...block, direction: direction, x: safeIndex(block.x - 1)}
  }

let hd = self => self[0]

let tl = xs =>
  switch xs {
  | []
  | [_] => []
  | [_, x] => [x]
  | self => Array.sub(self, 1, Array.length(self) - 2)
  }

let direction = (self: snake) => hd(self).direction

let dropLast = self => Array.sub(self, 0, Array.length(self) - 1)

let last = self => self[Array.length(self) - 1]

let move = (self, direction) => Array.concat(list{[moveBlock(direction, self->hd)], self->dropLast})

let hasCollidedWithSelf = self => isCollision(self->hd, self->tl)

let hasEaten = (self, food) => isCollision(self->hd, [food])

let detectCollision = (self, food) =>
  if self->hasEaten(food) {
    Some(Food)
  } else if self->hasCollidedWithSelf {
    Some(Tail)
  } else {
    None
  }

let grow = self => {
  let lastBlock = self->last
  let newBlock = {
    ...moveBlock(oppositeDirection(lastBlock.direction), lastBlock),
    direction: lastBlock.direction,
  }

  Array.concat(list{self, [newBlock]})
}
