let time_elapsed = t => Unix.gettimeofday() -. t;

let compose = (fns, env) => fns |> List.iter(f => f(env));

let chain = (env, fns) => fns |> List.iter(f => f(env));

let juxt = (fns, x) => fns |> List.map(f => f(x));

let safe_index = n =>
  if (n > Config.tiles - 1) {
    0;
  } else if (n < 0) {
    Config.tiles - 1;
  } else {
    n;
  };

let rgbColor = (r, g, b) => Reprocessing.Utils.color(~r, ~g, ~b, ~a=255);

let rgbaColor = (r, g, b, a) => Reprocessing.Utils.color(~r, ~g, ~b, ~a);