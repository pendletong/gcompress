import gcompress/zlib
import gleam/result
import gleam/io

pub fn main() {
  let z =
    zlib.start_deflate()
    |> zlib.set_compression(zlib.BestCompression)

  let ba =
    z
    |> result.try(fn(d) { zlib.deflate(d, <<"This is a test":utf8>>) })
    |> result.try(fn(d) { zlib.deflate(d, <<"This is a test2":utf8>>) })
    |> result.try(fn(d) { zlib.finish_deflate(d) })

  io.debug(ba)
}
