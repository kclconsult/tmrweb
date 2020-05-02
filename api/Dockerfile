FROM node:11
ENV user node
# Replace with root cert.
# COPY ./proxy/certs/consult.crt consult.crt
COPY package.json /home/$user/
COPY . /home/$user/
WORKDIR /home/$user
RUN chown $user --recursive .
USER $user
RUN cat requirements.txt | xargs npm install
COPY bin/wait-for-it.sh wait-for-it.sh
ENV NODE_ENV production
EXPOSE 8888
CMD ["npm", "start"]
