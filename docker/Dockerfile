FROM ocaml/opam:ubuntu

WORKDIR /app

COPY . .

RUN sudo apt-get update && \
    sudo apt-get install -y cmake clang python3 llvm-13-dev

RUN opam install dune

RUN opam install . --deps-only && \
    eval $(opam env) && \
    dune build main.exe

RUN sudo chown -R opam:opam /app

CMD ["bash", "./misc/compile.sh"]
