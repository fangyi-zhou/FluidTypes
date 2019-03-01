FROM fsharp

RUN apt-get update && \
    apt-get install -y curl unzip libgomp1 && \
    curl -o z3.zip -L https://github.com/Z3Prover/z3/releases/download/z3-4.8.4/z3-4.8.4.d6df51951f4c-x64-debian-8.11.zip && \
    unzip z3.zip && \
    mv z3-4.8.4.d6df51951f4c-x64-debian-8.11 /z3

ENV PATH=$PATH:/z3/bin

