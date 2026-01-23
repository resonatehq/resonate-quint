module util {

  type Option =
    | Some(str)
    | None

  type Inspect = {
    id: str,
    branch: str
  }

}
