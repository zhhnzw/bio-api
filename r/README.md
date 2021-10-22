docker build -t bio-r:v1.0 .

docker run -p 6311:6311 --name bio-r -dit bio-r:v1.0 bash