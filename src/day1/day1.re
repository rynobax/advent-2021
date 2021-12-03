let file_in_channel = Stdlib.open_in("./src/day1/input.txt");

let file_stream =
  Stream.from(_i => {
    switch (Stdlib.input_line(file_in_channel)) {
    | line => Some(line)
    | exception End_of_file => None
    }
  });

let revInts = ref([]);

let processLine = (line: string) => {
  let v = Stdlib.int_of_string(line);
  revInts := [v, ...revInts^];
  ();
};
let res = file_stream |> Stream.iter(processLine);

let vals = revInts^ |> List.rev;

let (_, answer) =
  vals
  |> List.fold_left(
       ((mbPv, pacc): (option(int), int), c: int) =>
         (
           Some(c),
           switch (mbPv) {
           | Some(pv) => c > pv ? pacc + 1 : pacc
           | None => pacc
           },
         ),
       (None, 0),
     );

Console.log("Part 1 answer:");
Console.log(answer);
