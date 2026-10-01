open Containers

type sections_json = {
  backend: Yojson.Safe.t;
  options: Yojson.Safe.t;
  view: Yojson.Safe.t;
}

type migration = {
  from_version: int;
  to_version: int;
  migrate: sections_json -> (sections_json, string) result;
}

let migrations_registry : migration list ref = ref []

let register_migration m =
  migrations_registry := m :: !migrations_registry

let clear_migrations () =
  migrations_registry := []

let update_assoc key (f : Yojson.Safe.t option -> Yojson.Safe.t) (json : Yojson.Safe.t) : Yojson.Safe.t =
  match json with
  | `Assoc fields ->
      let found = ref false in
      let fields' =
        List.filter_map (fun (k, (v : Yojson.Safe.t)) ->
          if String.equal k key then begin
            found := true;
            match f (Some v) with
            | `Null -> None
            | v' -> Some (k, v')
          end else
            Some (k, v)
        ) fields
      in
      if !found then `Assoc fields'
      else
        begin match f None with
        | `Null -> `Assoc fields'
        | v' -> `Assoc (fields' @ [(key, v')])
        end
  | (`Bool _ | `Float _ | `Int _ | `Intlit _ | `List _ | `Null | `String _) as other ->
      other

let rec migrate ~from_version ~target_version sections =
  if from_version = target_version then
    Ok sections
  else if from_version > target_version then
    Error (Printf.sprintf "Save file version %d is newer than game version %d. Please update the game." from_version target_version)
  else
    let matching =
      List.find_opt (fun m -> m.from_version = from_version) !migrations_registry
    in
    match matching with
    | None ->
        Error (Printf.sprintf "No migration path found from save version %d to %d" from_version target_version)
    | Some m ->
        if m.to_version <= m.from_version then
          Error (Printf.sprintf "Invalid migration: version must increase (from %d to %d)" m.from_version m.to_version)
        else
          match m.migrate sections with
          | Error err ->
              Error (Printf.sprintf "Migration from version %d to %d failed: %s" m.from_version m.to_version err)
          | Ok next_sections ->
              migrate ~from_version:m.to_version ~target_version next_sections
