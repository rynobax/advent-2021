let file_in_channel = Stdlib.open_in("./src/day3/input.txt");

exception BadInput(string);
exception GenericError(string);

let file_stream =
  Stream.from(_i => {
    switch (Stdlib.input_line(file_in_channel)) {
    | line => Some(line)
    | exception End_of_file => None
    }
  });

let revLines = ref([]);

file_stream
|> Stream.iter(line => {
     revLines := [line, ...revLines^];
     ();
   });

let lines = revLines^ |> List.rev;

let out =
  lines
  |> List.fold_left(
       (arr, line) => {
         line |> String.iteri((i, c) => c === '1' ? arr[i] = arr[i] + 1 : ());
         arr;
       },
       Array.make(lines |> List.hd |> String.length, 0),
     );

let dataLen = lines |> List.length;
let gammaStr =
  out
  |> Array.fold_left(
       (s, count) => s ++ (count * 2 >= dataLen ? "1" : "0"),
       "",
     );
let epsilonStr =
  out
  |> Array.fold_left(
       (s, count) => s ++ (count * 2 >= dataLen ? "0" : "1"),
       "",
     );
let gammaVal = "0b" ++ gammaStr |> int_of_string;
let epsilonVal = "0b" ++ epsilonStr |> int_of_string;

Console.log("Part 1 answer:");
Console.log(gammaVal * epsilonVal);

type mcvResult =
  | One
  | Zero
  | Tie;

let getMcv = (ls: list(string), ndx: int) => {
  let (zeroCount, oneCount) =
    ls
    |> List.fold_left(
         ((zero, one), line: string) =>
           line.[ndx] === '0' ? (zero + 1, one) : (zero, one + 1),
         (0, 0),
       );
  switch () {
  | _ when oneCount > zeroCount => One
  | _ when zeroCount > oneCount => Zero
  | _ => Tie
  };
};

let rec solveForOx = (ls: list(string), n: int) => {
  let keepVal =
    switch (getMcv(ls, n)) {
    | One => '1'
    | Zero => '0'
    | Tie => '1'
    };
  let filtered = ls |> List.filter(s => s.[n] === keepVal);
  switch (filtered) {
  | [answer] => answer
  | [] => raise(GenericError("Empty list"))
  | remaining => solveForOx(remaining, n + 1)
  };
};

let rec solveForCO2 = (ls: list(string), n: int) => {
  let keepVal =
    switch (getMcv(ls, n)) {
    | One => '0'
    | Zero => '1'
    | Tie => '0'
    };
  let filtered = ls |> List.filter(s => s.[n] === keepVal);
  switch (filtered) {
  | [answer] => answer
  | [] => raise(GenericError("Empty list"))
  | remaining => solveForCO2(remaining, n + 1)
  };
};

let oxRes = "0b" ++ solveForOx(lines, 0) |> int_of_string;
let c02Res = "0b" ++ solveForCO2(lines, 0) |> int_of_string;

Console.log("Part 2 answer:");
Console.log(oxRes);
Console.log(c02Res);
Console.log(oxRes * c02Res);
