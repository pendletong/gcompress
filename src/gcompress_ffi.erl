-module(gcompress_ffi).

-export([deflate_end/1]).

deflate_end(Z) ->
    CompressedData = zlib:deflate(Z, <<>>, finish),
    zlib:deflateEnd(Z),
    CompressedData.
