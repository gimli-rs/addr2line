IMAGE_NAME=opensuse-addr2line

docker build -t $IMAGE_NAME -f Dockerfile-opensuse .
docker run -v `pwd`:/addr2line -t $IMAGE_NAME bash -c "cd /addr2line && cargo build --release --features bin && ./benchmark-addr2line.sh"
