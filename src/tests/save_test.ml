open! Containers
module R = Rails_lib
module SC = R.Save_compression
module SM = R.Save_migrations
module SD = R.Save_game_d

let%expect_test "compression and decompression round trip" =
  let original = "{\"version\":1,\"save_title\":\"Test Title 1850\"}===={\"backend\":\"test\"}===={}==={}" in
  let compressed = SC.compress_string original in
  Printf.printf "Compressed smaller or non-empty: %b\n" (String.length compressed > 0);
  begin match SC.decompress_string compressed with
  | Ok restored -> Printf.printf "Restored equal: %b\n" (String.equal original restored)
  | Error err -> Printf.printf "Error: %s\n" err
  end;
  [%expect {|
    Compressed smaller or non-empty: true
    Restored equal: true |}]

let%expect_test "transparent uncompressed vs compressed read" =
  let uncompressed_save = "{\"version\":1,\"save_title\":\"Legacy Uncompressed\"}====backend====options====view" in
  begin match SC.read_save_data uncompressed_save with
  | Ok data -> Printf.printf "Uncompressed read OK: %b\n" (String.equal uncompressed_save data)
  | Error err -> Printf.printf "Error: %s\n" err
  end;
  let compressed = SC.compress_string uncompressed_save in
  begin match SC.read_save_data compressed with
  | Ok data -> Printf.printf "Compressed read OK: %b\n" (String.equal uncompressed_save data)
  | Error err -> Printf.printf "Error: %s\n" err
  end;
  [%expect {|
    Uncompressed read OK: true
    Compressed read OK: true |}]

let%expect_test "corrupted and empty save data error handling" =
  begin match SC.read_save_data "" with
  | Ok _ -> print_endline "Unexpected success on empty data"
  | Error msg -> Printf.printf "Empty data error: %s\n" msg
  end;
  begin match SC.read_save_data "corrupted garbage that is not json nor valid zlib" with
  | Ok _ -> print_endline "Unexpected success on corrupted data"
  | Error msg -> Printf.printf "Corrupted data caught gracefully: %b\n" (String.length msg > 0)
  end;
  [%expect {|
    Empty data error: Save file is empty
    Corrupted data caught gracefully: true |}]

let%expect_test "atomic file write and read" =
  let temp_file = Filename.temp_file "rails_test_save_" ".sav" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove temp_file with _ -> ())
    (fun () ->
      let content = "{\"version\":1,\"save_title\":\"Temp Save\"}====b====o====v" in
      match SC.write_save_file temp_file content with
      | Error err -> Printf.printf "Write error: %s\n" err
      | Ok () ->
          begin match SC.read_save_file temp_file with
          | Ok read_content -> Printf.printf "File roundtrip match: %b\n" (String.equal content read_content)
          | Error err -> Printf.printf "Read error: %s\n" err
          end);
  [%expect {| File roundtrip match: true |}]

let%expect_test "save migrations" =
  SM.clear_migrations ();
  let dummy_sections = {
    SM.backend = `Assoc [("cash", `Int 1000)];
    options = `Assoc [];
    view = `Assoc [];
  } in
  (* Target version same as save version *)
  begin match SM.migrate ~from_version:1 ~target_version:1 dummy_sections with
  | Ok _ -> print_endline "Version 1 to 1: OK"
  | Error err -> Printf.printf "Error: %s\n" err
  end;

  (* Save file is newer than target version *)
  begin match SM.migrate ~from_version:2 ~target_version:1 dummy_sections with
  | Ok _ -> print_endline "Unexpected success for newer save version"
  | Error err -> Printf.printf "Newer version error: %s\n" err
  end;

  (* Missing migration path *)
  begin match SM.migrate ~from_version:1 ~target_version:2 dummy_sections with
  | Ok _ -> print_endline "Unexpected success with missing migration"
  | Error err -> Printf.printf "Missing migration error: %s\n" err
  end;

  (* Register migration v1 -> v2 and v2 -> v3 *)
  SM.register_migration {
    from_version = 1;
    to_version = 2;
    migrate = (fun s ->
      let backend' = SM.update_assoc "cash" (fun _ -> `Int 2000) s.backend in
      Ok { s with backend = backend' });
  };
  SM.register_migration {
    from_version = 2;
    to_version = 3;
    migrate = (fun s ->
      let backend' = SM.update_assoc "currency" (fun _ -> `String "USD") s.backend in
      Ok { s with backend = backend' });
  };

  begin match SM.migrate ~from_version:1 ~target_version:3 dummy_sections with
  | Error err -> Printf.printf "Chained migration failed: %s\n" err
  | Ok result ->
      Printf.printf "Migrated JSON: %s\n" (Yojson.Safe.to_string result.backend)
  end;
  [%expect {|
    Version 1 to 1: OK
    Newer version error: Save file version 2 is newer than game version 1. Please update the game.
    Missing migration error: No migration path found from save version 1 to 2
    Migrated JSON: {"cash":2000,"currency":"USD"} |}]

let%expect_test "header parsing and resilience" =
  let valid_h = "{\"version\":1,\"save_title\":\"President (Tycoon) 1900\"}" in
  let h = SD.Header.of_string valid_h in
  begin match h with
  | Ok h -> Printf.printf "Valid: version=%d title=%s\n" h.version h.save_title
  | Error err -> Printf.printf "Error: %s\n" err
  end;

  let no_version_h = "{\"save_title\":\"Legacy Header without version\"}" in
  let h2 = SD.Header.of_string no_version_h in
  begin match h2 with
  | Ok h -> Printf.printf "Default version: %d title=%s\n" h.version h.save_title
  | Error err -> Printf.printf "Error: %s\n" err
  end;

  let invalid_h = "not valid json {{" in
  Printf.printf "Corrupted title: %s\n" (SD.Header.title_of_str invalid_h);
  [%expect {|
    Valid: version=1 title=President (Tycoon) 1900
    Default version: 1 title=Legacy Header without version
    Corrupted title: [CORRUPTED SAVE] |}]
