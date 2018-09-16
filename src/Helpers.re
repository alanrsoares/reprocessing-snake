let time_elapsed = t => Unix.gettimeofday() -. t;

let compose = (fns, env) => fns |> List.iter(f => f(env));

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
