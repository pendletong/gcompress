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

@external(erlang, "zlib", "inflateInit")
fn inflate_init(z: ZStream) -> Nil

@external(erlang, "zlib", "safeInflate")
fn do_inflate(z: ZStream, data: BitArray) -> #(InflateState, BitArray)

@external(erlang, "gcompress_ffi", "inflate_end")
fn inflate_end(z: ZStream) -> Result(Nil, Nil)

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
  StreamNotEnded
}

pub opaque type Deflate {
  Deflate(stream: ZStream, chunks: List(BitArray), initialised: Bool)
}

pub fn start_deflate() -> Deflate {
  Deflate(zopen(), [], False)
}

pub fn start_inflate() -> Deflate {
  let stream = zopen()
  inflate_init(stream)
  Deflate(stream, [], True)
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

pub fn deflate_string(
  def_data: Deflate,
  data: String,
) -> Result(Deflate, CompressionError) {
  deflate(def_data, bit_array.from_string(data))
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

pub fn inflate(
  def_data: Deflate,
  data: BitArray,
) -> Result(BitArray, CompressionError) {
  let new_def = keep_inflating(def_data, data)
  case inflate_end(new_def.stream) {
    Ok(_) -> {
      zclose(new_def.stream)

      Ok(bit_array.concat(list.reverse(new_def.chunks)))
    }
    Error(_) -> {
      zclose(new_def.stream)

      Error(StreamNotEnded)
    }
  }
}

fn keep_inflating(def_data: Deflate, data: BitArray) -> Deflate {
  case do_inflate(def_data.stream, data) {
    #(Continue, d) -> {
      keep_inflating(Deflate(..def_data, chunks: [d, ..def_data.chunks]), <<>>)
    }
    #(Finished, d) -> {
      Deflate(..def_data, chunks: [d, ..def_data.chunks])
    }
  }
}
