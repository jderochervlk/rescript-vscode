@@ocaml.doc(" # Json parser
 *
 * Works with bucklescript and bsb-native
 *
 * ## Basics
 *
 * ```
 * open Json.Infix; /* for the nice infix operators */
 * let raw = {|{\"hello\": \"folks\"}|};
 * let who = Json.parse(raw) |> Json.get(\"hello\") |?> Json.string;
 * Js.log(who);
 * ```
 *
 * ## Parse & stringify
 *
 * @doc parse, stringify
 *
 * ## Accessing descendents
 *
 * @doc get, nth, getPath
 *
 * ## Coercing to types
 *
 * @doc string, number, array, obj, bool, null
 *
 * ## The JSON type
 *
 * @doc t
 *
 * ## Infix operators for easier working
 *
 * @doc Infix
 ")

external parseFloat: string => float = "parseFloat"

type rec t =
  | String(string)
  | Number(float)
  | Array(list<t>)
  | Object(list<(string, t)>)
  | True
  | False
  | Null

let string_of_number = f => {
  let s = Float.toString(f)
  if String.get(s, String.length(s) - 1) == Some(".") {
    String.slice(s, ~start=0, ~end=String.length(s) - 1)
  } else {
    s
  }
}

@ocaml.doc("
 * This module is provided for easier working with optional values.
 ")
module Infix = {
  @ocaml.doc(" The \"force unwrap\" operator
   *
   * If you're sure there's a value, you can force it.
   * ```
   * open Json.Infix;
   * let x: int = Some(10) |! \"Expected this to be present\";
   * Js.log(x);
   * ```
   *
   * But you gotta be sure, otherwise it will throw.
   * ```reason;raises
   * open Json.Infix;
   * let x: int = None |! \"This will throw\";
   * ```
   ")
  let \"|!" = (o, d) =>
    switch o {
    | None => failwith(d)
    | Some(v) => v
    }
  @ocaml.doc(" The \"upwrap with default\" operator
   * ```
   * open Json.Infix;
   * let x: int = Some(10) |? 4;
   * let y: int = None |? 5;
   * Js.log2(x, y);
   * ```
   ")
  let \"|?" = (o, d) =>
    switch o {
    | None => d
    | Some(v) => v
    }
  @ocaml.doc(" The \"transform contents into new optional\" operator
   * ```
   * open Json.Infix;
   * let maybeInc = x => x > 5 ? Some(x + 1) : None;
   * let x: option(int) = Some(14) |?> maybeInc;
   * let y: option(int) = None |?> maybeInc;
   * ```
   ")
  let \"|?>" = (o, fn) =>
    switch o {
    | None => None
    | Some(v) => fn(v)
    }
  @ocaml.doc(" The \"transform contents into new value & then re-wrap\" operator
   * ```
   * open Json.Infix;
   * let inc = x => x + 1;
   * let x: option(int) = Some(7) |?>> inc;
   * let y: option(int) = None |?>> inc;
   * Js.log2(x, y);
   * ```
   ")
  let \"|?>>" = (o, fn) =>
    switch o {
    | None => None
    | Some(v) => Some(fn(v))
    }
  @ocaml.doc(" \"handle the value if present, otherwise here's the default\"
   *
   * It's called fold because that's what people call it :?. It's the same as \"transform contents to new value\" + \"unwrap with default\".
   *
   * ```
   * open Json.Infix;
   * let inc = x => x + 1;
   * let x: int = fold(Some(4), 10, inc);
   * let y: int = fold(None, 2, inc);
   * Js.log2(x, y);
   * ```
   ")
  let fold = (o, d, f) =>
    switch o {
    | None => d
    | Some(v) => f(v)
    }
}

let escape = text => {
  let ln = String.length(text)
  let rec loop = (i, acc) =>
    if i < ln {
      let next = switch String.get(text, i) {
      | Some("\x0c") => acc ++ "\\f"
      | Some("\\") => acc ++ "\\\\"
      | Some("\"") => acc ++ "\\\""
      | Some("\n") => acc ++ "\\n"
      | Some("\b") => acc ++ "\\b"
      | Some("\r") => acc ++ "\\r"
      | Some("\t") => acc ++ "\\t"
      | Some(c) => acc ++ c
      | None => acc
      }
      loop(i + 1, next)
    } else {
      acc
    }
  loop(0, "")
}

@ocaml.doc(" ```
 * let text = {|{\"hello\": \"folks\", \"aa\": [2, 3, \"four\"]}|};
 * let result = Json.stringify(Json.parse(text));
 * Js.log(result);
 * assert(text == result);
 * ```
 ")
let rec stringify = t =>
  switch t {
  | String(value) => "\"" ++ (escape(value) ++ "\"")
  | Number(num) => string_of_number(num)
  | Array(items) => {
      let rec join = (items, sep) =>
        switch items {
        | list{} => ""
        | list{x} => x
        | list{x, ...rest} => x ++ sep ++ join(rest, sep)
        }
      let parts = List.map(items, stringify)
      "[" ++ join(parts, ", ") ++ "]"
    }
  | Object(items) => {
      let rec join = (items, sep) =>
        switch items {
        | list{} => ""
        | list{x} => x
        | list{x, ...rest} => x ++ sep ++ join(rest, sep)
        }
      let parts = List.map(items, ((k, v)) => "\"" ++ (escape(k) ++ ("\": " ++ stringify(v))))
      "{" ++ join(parts, ", ") ++ "}"
    }
  | True => "true"
  | False => "false"
  | Null => "null"
  }

let white = n => {
  let rec loop = (i, acc) =>
    if i < n {
      loop(i + 1, acc ++ " ")
    } else {
      acc
    }
  loop(0, "")
}

let rec stringifyPretty = (~indent=0, t) => {
  let rec join = (items, sep) =>
    switch items {
    | list{} => ""
    | list{x} => x
    | list{x, ...rest} => x ++ sep ++ join(rest, sep)
    }
  switch t {
  | String(value) => "\"" ++ (escape(value) ++ "\"")
  | Number(num) => string_of_number(num)
  | Array(list{}) => "[]"
  | Array(items) => {
      let parts = List.map(items, item => stringifyPretty(~indent=indent + 2, item))
      "[\n" ++
      white(indent + 2) ++
      join(parts, ",\n" ++ white(indent + 2)) ++
      "\n" ++
      white(indent) ++ "]"
    }
  | Object(list{}) => "{}"
  | Object(items) => {
      let parts = List.map(items, ((k, v)) =>
        "\"" ++ (escape(k) ++ ("\": " ++ stringifyPretty(~indent=indent + 2, v)))
      )
      "{\n" ++
      white(indent + 2) ++
      join(parts, ",\n" ++ white(indent + 2)) ++
      "\n" ++
      white(indent) ++ "}"
    }
  | True => "true"
  | False => "false"
  | Null => "null"
  }
}

let unwrap = (message, t) =>
  switch t {
  | Some(v) => v
  | None => failwith(message)
  }

@nodoc
module Parser = {
  let split_by = (~keep_empty=false, is_delim, str) => {
    let len = String.length(str)
    let rec loop = (acc, last_pos, pos) =>
      if pos == -1 {
        if last_pos == 0 && !keep_empty {
          acc
        } else {
          list{String.slice(str, ~start=0, ~end=last_pos), ...acc}
        }
      } else if is_delim(String.get(str, pos)) {
        let new_len = last_pos - pos - 1
        if new_len != 0 || keep_empty {
          let v = String.slice(str, ~start=pos + 1, ~end=pos + 1 + new_len)
          loop(list{v, ...acc}, pos, pos - 1)
        } else {
          loop(acc, pos, pos - 1)
        }
      } else {
        loop(acc, last_pos, pos - 1)
      }
    loop(list{}, len, len - 1)
  }
  let fail = (text, pos, message) => {
    let pre = String.slice(text, ~start=0, ~end=pos)
    let lines = split_by(c => c == Some("\n"), pre)
    let count = List.length(lines)
    let last = count > 0 ? List.getOrThrow(lines, count - 1) : ""
    let col = String.length(last) + 1
    let line = List.length(lines)
    let string =
      "Error \"" ++
      message ++
      "\" at " ++
      Int.toString(line) ++
      ":" ++
      Int.toString(col) ++
      " -> " ++
      last ++ "\n"
    failwith(string)
  }
  let rec skipToNewline = (text, pos) =>
    if pos >= String.length(text) {
      pos
    } else if String.get(text, pos) == Some("\n") {
      pos + 1
    } else {
      skipToNewline(text, pos + 1)
    }
  let stringTail = text => {
    let len = String.length(text)
    if len > 1 {
      String.slice(text, ~start=1, ~end=len)
    } else {
      ""
    }
  }
  let rec skipToCloseMultilineComment = (text, pos) =>
    if pos + 1 >= String.length(text) {
      failwith("Unterminated comment")
    } else if String.get(text, pos) == Some("*") && String.get(text, pos + 1) == Some("/") {
      pos + 2
    } else {
      skipToCloseMultilineComment(text, pos + 1)
    }
  let rec skipWhite = (text, pos) =>
    if (
      pos < String.length(text) &&
        (String.get(text, pos) == Some(" ") ||
          (String.get(text, pos) == Some("\t") ||
          (String.get(text, pos) == Some("\n") || String.get(text, pos) == Some("\r"))))
    ) {
      skipWhite(text, pos + 1)
    } else {
      pos
    }
  let parseString = (text, pos) => {
    let ln = String.length(text)
    let rec loop = (i, acc) =>
      i >= ln
        ? fail(text, i, "Unterminated string")
        : switch String.get(text, i) {
          | Some("\"") => (i + 1, acc)
          | Some("\\") =>
            i + 1 >= ln
              ? fail(text, i, "Unterminated string")
              : switch String.get(text, i + 1) {
                | Some("/") => loop(i + 2, acc ++ "/")
                | Some("f") => loop(i + 2, acc ++ "\x0c")
                | _ =>
                  let escaped = String.slice(text, ~start=i, ~end=i + 2)
                  loop(i + 2, acc ++ escaped)
                }
          | Some(c) => loop(i + 1, acc ++ c)
          | None => (i, acc)
          }
    let (final, result) = loop(pos, "")
    (result, final)
  }
  let parseDigits = (text, pos) => {
    let len = String.length(text)
    let rec loop = i =>
      if i >= len {
        i
      } else {
        switch String.get(text, i) {
        | Some("0")
        | Some("1")
        | Some("2")
        | Some("3")
        | Some("4")
        | Some("5")
        | Some("6")
        | Some("7")
        | Some("8")
        | Some("9") =>
          loop(i + 1)
        | _ => i
        }
      }
    loop(pos + 1)
  }
  let parseWithDecimal = (text, pos) => {
    let pos = parseDigits(text, pos)
    if pos < String.length(text) && String.get(text, pos) == Some(".") {
      let pos = parseDigits(text, pos + 1)
      pos
    } else {
      pos
    }
  }
  let parseNumber = (text, pos) => {
    let pos = parseWithDecimal(text, pos)
    let ln = String.length(text)
    if pos < ln - 1 && (String.get(text, pos) == Some("E") || String.get(text, pos) == Some("e")) {
      let pos = switch String.get(text, pos + 1) {
      | Some("-")
      | Some("+") =>
        pos + 2
      | _ => pos + 1
      }
      parseDigits(text, pos)
    } else {
      pos
    }
  }
  let parseNegativeNumber = (text, pos) => {
    let final = if String.get(text, pos) == Some("-") {
      parseNumber(text, pos + 1)
    } else {
      parseNumber(text, pos)
    }
    let numStr = String.slice(text, ~start=pos, ~end=final)
    (Number(parseFloat(numStr)), final)
  }
  let expect = (char, text, pos, message) =>
    if String.get(text, pos) != Some(char) {
      fail(text, pos, "Expected: " ++ message)
    } else {
      pos + 1
    }
  let parseComment: 'a. (string, int, (string, int) => 'a) => 'a = (text, pos, next) =>
    if String.get(text, pos) != Some("/") {
      if String.get(text, pos) == Some("*") {
        next(text, skipToCloseMultilineComment(text, pos + 1))
      } else {
        failwith("Invalid syntax")
      }
    } else {
      next(text, skipToNewline(text, pos + 1))
    }
  let maybeSkipComment = (text, pos) =>
    if pos < String.length(text) && String.get(text, pos) == Some("/") {
      if pos + 1 < String.length(text) && String.get(text, pos + 1) == Some("/") {
        skipToNewline(text, pos + 1)
      } else if pos + 1 < String.length(text) && String.get(text, pos + 1) == Some("*") {
        skipToCloseMultilineComment(text, pos + 1)
      } else {
        fail(text, pos, "Invalid synatx")
      }
    } else {
      pos
    }
  let rec skip = (text, pos) =>
    if pos == String.length(text) {
      pos
    } else {
      let n = maybeSkipComment(text, skipWhite(text, pos))
      if n > pos {
        skip(text, n)
      } else {
        n
      }
    }
  let rec parse = (text, pos) =>
    if pos >= String.length(text) {
      fail(text, pos, "Reached end of file without being done parsing")
    } else {
      switch String.get(text, pos) {
      | Some("/") => parseComment(text, pos + 1, parse)
      | Some("[") => parseArray(text, pos + 1)
      | Some("{") => parseObject(text, pos + 1)
      | Some("n") =>
        if String.slice(text, ~start=pos, ~end=pos + 4) == "null" {
          (Null, pos + 4)
        } else {
          fail(text, pos, "unexpected character")
        }
      | Some("t") =>
        if String.slice(text, ~start=pos, ~end=pos + 4) == "true" {
          (True, pos + 4)
        } else {
          fail(text, pos, "unexpected character")
        }
      | Some("f") =>
        if String.slice(text, ~start=pos, ~end=pos + 5) == "false" {
          (False, pos + 5)
        } else {
          fail(text, pos, "unexpected character")
        }
      | Some("\n")
      | Some("\t")
      | Some(" ")
      | Some("\r") =>
        parse(text, skipWhite(text, pos))
      | Some("\"") =>
        let (s, pos) = parseString(text, pos + 1)
        (String(s), pos)
      | Some("-")
      | Some("0")
      | Some("1")
      | Some("2")
      | Some("3")
      | Some("4")
      | Some("5")
      | Some("6")
      | Some("7")
      | Some("8")
      | Some("9") =>
        parseNegativeNumber(text, pos)
      | _ => fail(text, pos, "unexpected character")
      }
    }
  and parseArrayValue = (text, pos) => {
    let pos = skip(text, pos)
    let (value, pos) = parse(text, pos)
    let pos = skip(text, pos)
    switch String.get(text, pos) {
    | Some(",") =>
      let pos = skip(text, pos + 1)
      if String.get(text, pos) == Some("]") {
        (list{value}, pos + 1)
      } else {
        let (rest, pos) = parseArrayValue(text, pos)
        (list{value, ...rest}, pos)
      }
    | Some("]") => (list{value}, pos + 1)
    | _ => fail(text, pos, "unexpected character")
    }
  }
  and parseArray = (text, pos) => {
    let pos = skip(text, pos)
    switch String.get(text, pos) {
    | Some("]") => (Array(list{}), pos + 1)
    | _ =>
      let (items, pos) = parseArrayValue(text, pos)
      (Array(items), pos)
    }
  }
  and parseObjectValue = (text, pos) => {
    let pos = skip(text, pos)
    if String.get(text, pos) != Some("\"") {
      fail(text, pos, "Expected string")
    } else {
      let (key, pos) = parseString(text, pos + 1)
      let pos = skip(text, pos)
      let pos = expect(":", text, pos, "Colon")
      let (value, pos) = parse(text, pos)
      let pos = skip(text, pos)
      switch String.get(text, pos) {
      | Some(",") =>
        let pos = skip(text, pos + 1)
        if String.get(text, pos) == Some("}") {
          (list{(key, value)}, pos + 1)
        } else {
          let (rest, pos) = parseObjectValue(text, pos)
          (list{(key, value), ...rest}, pos)
        }
      | Some("}") => (list{(key, value)}, pos + 1)
      | _ =>
        let (rest, pos) = parseObjectValue(text, pos)
        (list{(key, value), ...rest}, pos)
      }
    }
  }
  and parseObject = (text, pos) => {
    let pos = skip(text, pos)
    if String.get(text, pos) == Some("}") {
      (Object(list{}), pos + 1)
    } else {
      let (pairs, pos) = parseObjectValue(text, pos)
      (Object(pairs), pos)
    }
  }
}

@ocaml.doc(" Turns some text into a json object. throws on failure ")
let parse = text => {
  let (item, pos) = Parser.parse(text, 0)
  let pos = Parser.skip(text, pos)
  if pos < String.length(text) {
    failwith(
      "Extra data after parse finished: " ++
      String.slice(text, ~start=pos, ~end=String.length(text)),
    )
  } else {
    item
  }
}

/* Accessor helpers */
let bind = (v, fn) =>
  switch v {
  | None => None
  | Some(v) => fn(v)
  }

@ocaml.doc(" If `t` is an object, get the value associated with the given string key ")
let get = (key, t) =>
  switch t {
  | Object(items) => {
      let rec find = items =>
        switch items {
        | list{} => None
        | list{(k, v), ...rest} => k == key ? Some(v) : find(rest)
        }
      find(items)
    }
  | _ => None
  }

@ocaml.doc(" If `t` is an array, get the value associated with the given index ")
let nth = (n, t) =>
  switch t {
  | Array(items) =>
    if n < List.length(items) {
      Some(List.getOrThrow(items, n))
    } else {
      None
    }
  | _ => None
  }

let string = t =>
  switch t {
  | String(s) => Some(s)
  | _ => None
  }

let number = t =>
  switch t {
  | Number(s) => Some(s)
  | _ => None
  }

let array = t =>
  switch t {
  | Array(s) => Some(s)
  | _ => None
  }

let obj = t =>
  switch t {
  | Object(s) => Some(s)
  | _ => None
  }

let bool = t =>
  switch t {
  | True => Some(true)
  | False => Some(false)
  | _ => None
  }

let null = t =>
  switch t {
  | Null => Some()
  | _ => None
  }

let rec parsePath = (keyList, t) =>
  switch keyList {
  | list{} => Some(t)
  | list{head, ...rest} =>
    switch get(head, t) {
    | None => None
    | Some(value) => parsePath(rest, value)
    }
  }

@ocaml.doc(" Get a deeply nested value from an object `t`.
 * ```
 * open Json.Infix;
 * let json = Json.parse({|{\"a\": {\"b\": {\"c\": 2}}}|});
 * let num = Json.getPath(\"a.b.c\", json) |?> Json.number;
 * assert(num == Some(2.))
 * ```
 ")
let getPath = (path, t) => {
  let keys = Parser.split_by(c => c == Some("."), path)
  parsePath(keys, t)
}
