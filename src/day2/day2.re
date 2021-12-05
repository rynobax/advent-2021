let file_in_channel = Stdlib.open_in("./src/day2/input.txt");

exception BadInput(string);

let file_stream =
  Stream.from(_i => {
    switch (Stdlib.input_line(file_in_channel)) {
    | line => Some(line)
    | exception End_of_file => None
    }
  });

type direction =
  | Up
  | Down
  | Forward;

let revTuples = ref([]);
let res =
  file_stream
  |> Stream.iter(line => {
       let parts = line |> Str.split(Str.regexp(" "));
       let next =
         switch (parts) {
         | ["forward", dist] => (Forward, dist |> Stdlib.int_of_string)
         | ["up", dist] => (Up, dist |> Stdlib.int_of_string)
         | ["down", dist] => (Down, dist |> Stdlib.int_of_string)
         | _ => raise(BadInput(line))
         };
       revTuples := [next, ...revTuples^];
       ();
     });

let path = revTuples^ |> List.rev;

let (dist1, depth1) =
  path
  |> List.fold_left(
       ((pDist, pDepth), (dir, n)) => {
         switch (dir) {
         | Forward => (pDist + n, pDepth)
         | Up => (pDist, pDepth - n)
         | Down => (pDist, pDepth + n)
         }
       },
       (0, 0),
     );

Console.log("Part 1 answer:");
Console.log(dist1 * depth1);

let (dist2, depth2, _) =
  path
  |> List.fold_left(
       ((pDist, pDepth, aim), (dir, n)) => {
         switch (dir) {
         | Forward => (pDist + n, pDepth + aim * n, aim)
         | Up => (pDist, pDepth, aim - n)
         | Down => (pDist, pDepth, aim + n)
         }
       },
       (0, 0, 0),
     );

Console.log("Part 2 answer:");
Console.log(dist2 * depth2);
