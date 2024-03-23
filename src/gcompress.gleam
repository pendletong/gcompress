import gcompress/zlib
import gleam/result
import gleam/io

pub fn main() {
  let z =
    zlib.start_deflate()
    |> zlib.set_compression(zlib.BestCompression)

  let ba =
    z
    |> result.try(fn(d) { zlib.deflate_string(d, "This is a test") })
    |> result.try(fn(d) { zlib.deflate(d, <<"This is a test2":utf8>>) })
    |> result.try(fn(d) { zlib.finish_deflate(d) })

  io.debug(ba)
  case ba {
    Ok(d) -> {
      let u = zlib.start_inflate()
      let r = zlib.inflate(u, d)
      case r {
        Ok(orig) -> {
          io.debug(orig)
          Nil
        }
        Error(e) -> {
          io.debug(e)
          Nil
        }
      }
    }
    Error(e) -> {
      io.debug(e)
      Nil
    }
  }
}
