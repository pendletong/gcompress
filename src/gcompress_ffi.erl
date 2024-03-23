-module(gcompress_ffi).

-export([deflate_end/1, inflate_end/1]).

deflate_end(Z) ->
    CompressedData = zlib:deflate(Z, <<>>, finish),
    zlib:deflateEnd(Z),
    CompressedData.

inflate_end(Z) ->
     try {ok, zlib:inflateEnd(Z)}
    catch error:data_error -> {error, nil}
    end.
