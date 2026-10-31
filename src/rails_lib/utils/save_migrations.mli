(** Sequential JSON AST migrations for save games across versions. *)

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

val register_migration : migration -> unit
(** [register_migration m] registers a migration step. *)

val clear_migrations : unit -> unit
(** [clear_migrations ()] clears registered migrations (primarily for tests). *)

val migrate :
  from_version:int ->
  target_version:int ->
  sections_json ->
  (sections_json, string) result
(** [migrate ~from_version ~target_version sections] executes registered migrations
    sequentially until [target_version] is reached. Returns an error if the save
    version is newer than the target or if a required migration step is missing. *)

val update_assoc :
  string -> (Yojson.Safe.t option -> Yojson.Safe.t) -> Yojson.Safe.t -> Yojson.Safe.t
(** Helper for AST migrations: updates, adds, or removes a key in a JSON object. *)
