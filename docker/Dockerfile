FROM ocaml/opam:ubuntu

COPY . .

RUN opam install . --deps-only --locked && \
    eval $(opam env) && \
    dune build main.exe && \
    sudo cp ./_build/default/main.exe /usr/bin/main.exe

CMD ["/usr/bin/main.exe"]
