FROM swipl
COPY . /app
# Keep container alive, retaining background server process, with sleep (temporarily).
CMD ["swipl", "-f", "app/server.pl", "-g", "server(1234)", "-g", "sleep(500000)"]
