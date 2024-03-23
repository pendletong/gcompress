import gleam/bit_array
import gleam/list

type ZStream

type InflateState {
  Continue
  Finished
}

@external(erlang, "zlib", "open")
fn zopen() -> ZStream

@external(erlang, "zlib", "close")
fn zclose(z: ZStream) -> Nil

@external(erlang, "zlib", "deflateInit")
fn deflate_init(z: ZStream, level: Int) -> Nil

@external(erlang, "zlib", "deflate")
fn do_deflate(z: ZStream, data: BitArray) -> BitArray

@external(erlang, "gcompress_ffi", "deflate_end")
fn deflate_end(z: ZStream) -> BitArray

pub type CompressionLevel {
  None
  Default
  Level(i: Int)
  BestCompression
  BestSpeed
}

pub type CompressionError {
  AlreadyInitialised
  InvalidCompressionLevel
}

pub opaque type Deflate {
  Deflate(stream: ZStream, chunks: List(BitArray), initialised: Bool)
}

pub fn start_deflate() -> Deflate {
  Deflate(zopen(), [], False)
}

pub fn set_compression(
  def_data: Deflate,
  level: CompressionLevel,
) -> Result(Deflate, CompressionError) {
  case def_data.initialised {
    True -> {
      Error(AlreadyInitialised)
    }
    False -> {
      let initialised = {
        case level {
          None -> {
            deflate_init(def_data.stream, 0)
            True
          }
          Default -> {
            deflate_init(def_data.stream, -1)
            True
          }
          BestCompression -> {
            deflate_init(def_data.stream, 9)
            True
          }
          BestSpeed -> {
            deflate_init(def_data.stream, 1)
            True
          }
          Level(l) if l >= 0 && l <= 9 -> {
            deflate_init(def_data.stream, l)
            True
          }
          _ -> {
            False
          }
        }
      }
      case initialised {
        True -> Ok(Deflate(..def_data, initialised: True))
        False -> Error(InvalidCompressionLevel)
      }
    }
  }
}

pub fn deflate(
  def_data: Deflate,
  data: BitArray,
) -> Result(Deflate, CompressionError) {
  let def_data = {
    case def_data.initialised {
      False -> {
        let assert Ok(d) = set_compression(def_data, Default)
        d
      }
      True -> def_data
    }
  }

  Ok(
    Deflate(
      ..def_data,
      chunks: [do_deflate(def_data.stream, data), ..def_data.chunks],
    ),
  )
}

pub fn finish_deflate(def_data: Deflate) -> Result(BitArray, CompressionError) {
  let new_def_data =
    Deflate(
      ..def_data,
      chunks: [deflate_end(def_data.stream), ..def_data.chunks],
    )

  zclose(new_def_data.stream)

  Ok(bit_array.concat(list.reverse(new_def_data.chunks)))
}
