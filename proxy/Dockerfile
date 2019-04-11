FROM nginx

RUN mkdir /etc/nginx/ssl

# In swarm replace with loaded secrets: https://docs.docker.com/engine/swarm/secrets/#use-secrets-in-compose
COPY certs/maximoff.crt /etc/nginx/ssl/maximoff.crt
COPY certs/maximoff.key /etc/nginx/ssl/maximoff.key

COPY nginx.conf /etc/nginx/nginx.conf
COPY locations /etc/nginx/locations

COPY bin/wait-for-it.sh wait-for-it.sh

CMD ["nginx", "-g", "daemon off;"]
