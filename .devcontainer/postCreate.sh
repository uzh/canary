# ocaml/opam post create script

sudo chown -R opam: _build

opam init -a --shell=zsh

opam install -y --working-dir --with-dev-setup .
