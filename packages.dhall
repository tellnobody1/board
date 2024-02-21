let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.14-20240213/packages.dhall
        sha256:c7196e3a5895098b1bf108c8210429b658d2baaa6c188879d75effc293ffe45f

in  upstream
  with protobuf =
    { dependencies = [ "integers", "arrays", "arraybuffer-types" ]
    , repo = "https://github.com/zero-deps/purescript-protobuf.git"
    , version = "1.3.4"
    }
