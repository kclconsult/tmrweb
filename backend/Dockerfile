FROM swipl
COPY . /app
EXPOSE 1234
CMD ["swipl", "-f", "app/server.pl", "-g", "server(1234)", "-g", "sleep(5000000)"]
