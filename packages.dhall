let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230325/packages.dhall sha256:98bd559174f64f82966f13583a14b67ab2010e8b1b79ba69f51af457d469d979

in  upstream
  with protobuf =
    { dependencies = [ "integers", "arrays", "arraybuffer-types" ]
    , repo = "https://github.com/zero-deps/purescript-protobuf.git"
    , version = "1.2.2"
    }
