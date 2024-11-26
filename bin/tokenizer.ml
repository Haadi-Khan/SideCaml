module StringMap = Map.Make(String)

type t = {
  vocab: int StringMap.t;
  inverse_vocab: string array;
  max_input_chars_per_word: int;
  unk_token: string;
}

(* Clean input text, keeping alphanumeric characters, punctuation & emojis *)
let preprocess_text text =
  let buf = Buffer.create (String.length text) in
  String.iter (fun c ->
    if (c >= 'a' && c <= 'z') ||
       (c >= 'A' && c <= 'Z') ||
       (c >= '0' && c <= '9') ||
       c = ' ' || c = '.' || c = ',' || c = '!' || c = '?' ||
       Char.code c > 127 then
      Buffer.add_char buf c
  ) text;
  Buffer.contents buf

let split_text text =
  let len = String.length text in
  let words = ref [] in
  let buf = Buffer.create 100 in
  let flush_buffer () =
    if Buffer.length buf > 0 then begin
      words := Buffer.contents buf :: !words;
      Buffer.clear buf
    end
  in
  for i = 0 to len - 1 do
    let c = text.[i] in
    if c = ' ' then
      flush_buffer ()
    else
      Buffer.add_char buf c
  done;
  flush_buffer ();
  List.rev !words

let train ?(vocab_size=30000) ?(min_frequency=2) ?(max_input_chars_per_word=100) texts =
  Printf.printf "Preprocessing texts...\n";
  
  (* Initial character vocabulary *)
  let char_freq = Hashtbl.create 256 in
  List.iter (fun text ->
    String.iter (fun c ->
      let char_str = String.make 1 c in
      let count = try Hashtbl.find char_freq char_str with Not_found -> 0 in
      Hashtbl.replace char_freq char_str (count + 1)
    ) (preprocess_text text)
  ) texts;
  
  (* Initialize vocabulary *)
  let vocab = ref StringMap.empty in
  let next_id = ref 0 in
  
  (* Add special tokens
      - UNK : used when we encounter a word/character outside of vocabulary
      - PAD : used to make all sequences the same length in a batch
      - CLS: add at the start of a sequence
      - SEP: separate different parts of the input
       *)
  let special_tokens = ["[UNK]"; "[PAD]"; "[CLS]"; "[SEP]"] in
  List.iter (fun token ->
    vocab := StringMap.add token !next_id !vocab;
    incr next_id
  ) special_tokens;
  
  (* Add characters *)
  Hashtbl.iter (fun char_str _ ->
    if not (StringMap.mem char_str !vocab) then begin
      vocab := StringMap.add char_str !next_id !vocab;
      incr next_id
    end
  ) char_freq;
  
  (* Process initial tokens *)
  let current_tokens = ref (
    List.map (fun text ->
      let processed = preprocess_text text in
      let chars = ref [] in
      String.iter (fun c ->
        chars := String.make 1 c :: !chars
      ) processed;
      List.rev !chars
    ) texts
  ) in
  
  (* Training loop. Looks at all adjacent pairs of tokens, counts how often each appears, finds the most frequent pair & adds it to vocabulary. *)
  let iteration = ref 0 in
  while StringMap.cardinal !vocab < vocab_size && !iteration < 1000 do
    if !iteration mod 10 = 0 then
      Printf.printf "Training iteration %d... (vocab size: %d/%d)\r%!"
        !iteration (StringMap.cardinal !vocab) vocab_size;
    
    (* Count pairs *)
    let pair_freq = Hashtbl.create 1000 in
    List.iter (fun tokens ->
      let rec count_pairs = function
        | a :: b :: rest -> 
            let pair = a ^ b in
            let count = try Hashtbl.find pair_freq pair with Not_found -> 0 in
            Hashtbl.replace pair_freq pair (count + 1);
            count_pairs (b :: rest)
        | _ -> ()
      in
      count_pairs tokens
    ) !current_tokens;
    
    (* Find best pair *)
    let best_pair = ref None in
    let best_freq = ref min_frequency in
    Hashtbl.iter (fun pair freq ->
      if freq > !best_freq && not (StringMap.mem pair !vocab) then begin
        best_pair := Some pair;
        best_freq := freq
      end
    ) pair_freq;
    
    match !best_pair with
    | Some pair ->
        (* Add to vocabulary *)
        vocab := StringMap.add pair !next_id !vocab;
        incr next_id;
        
        (* Merge pairs in current tokens *)
        current_tokens := List.map (fun tokens ->
          let rec merge = function
            | a :: b :: rest when a ^ b = pair -> pair :: merge rest
            | token :: rest -> token :: merge rest
            | [] -> []
          in
          merge tokens
        ) !current_tokens;
        
        incr iteration
    | None -> iteration := 1000
  done;
  
  Printf.printf "\nTraining complete! Final vocabulary size: %d\n" 
    (StringMap.cardinal !vocab);
  
  (* Create inverse vocabulary *)
  let inverse_vocab = Array.make (StringMap.cardinal !vocab) "" in
  StringMap.iter (fun token id ->
    Array.set inverse_vocab id token
  ) !vocab;
  
  { 
    vocab = !vocab;
    inverse_vocab;
    max_input_chars_per_word;
    unk_token = "[UNK]";
  }

(* Splits text into words, and for each word, tries to match the longest possible subword from vocabulary. Uses UNK token if unknown*)
let tokenize t text =
  let processed_text = preprocess_text text in
  let words = split_text processed_text in
  
  let tokenize_word word =
    if String.length word > t.max_input_chars_per_word then
      [StringMap.find t.unk_token t.vocab]
    else
      let tokens = ref [] in
      let start = ref 0 in
      let len = String.length word in
      
      while !start < len do
        let found = ref false in
        let end_pos = ref len in
        
        while not !found && !end_pos > !start do
          let substr = String.sub word !start (!end_pos - !start) in
          if StringMap.mem substr t.vocab then begin
            tokens := StringMap.find substr t.vocab :: !tokens;
            start := !end_pos;
            found := true
          end else
            decr end_pos
        done;
        
        if not !found then begin
          tokens := StringMap.find t.unk_token t.vocab :: !tokens;
          incr start
        end
      done;
      
      List.rev !tokens
  in
  
  List.concat_map tokenize_word words

(* Converts token IDs back to text by looking up in inverse vocabulary, then concatenates them back together. *)
let decode t token_ids =
  String.concat "" (List.map (fun id -> 
    try Array.get t.inverse_vocab id 
    with Invalid_argument _ -> t.unk_token
  ) token_ids)

let load_and_train_tokenizer path =
  Printf.printf "Loading posts from %s...\n" path;
  let ic = open_in path in
  let json = Yojson.Safe.from_channel ic in
  close_in ic;
  
  Printf.printf "Extracting texts from posts...\n";
  let posts = Yojson.Safe.Util.to_list json in
  let texts = List.map (fun post -> 
    Yojson.Safe.Util.(post |> member "text" |> to_string)
  ) posts in
  
  Printf.printf "Beginning tokenizer training on %d posts...\n" (List.length texts);
  let tokenizer = train ~vocab_size:190 texts in
  Printf.printf "Tokenizer training complete! Vocabulary size: %d\n" 
    (StringMap.cardinal tokenizer.vocab);
  tokenizer

let tokenize_random_post tokenizer path =
  Printf.printf "Selecting random post...\n";
  let ic = open_in path in
  let json = Yojson.Safe.from_channel ic in
  close_in ic;
  
  let posts = Yojson.Safe.Util.to_list json in
  let random_post = List.nth posts (Random.int (List.length posts)) in
  let text = Yojson.Safe.Util.(random_post |> member "text" |> to_string) in
  
  Printf.printf "Tokenizing post...\n";
  let tokens = tokenize tokenizer text in
  (text, tokens)