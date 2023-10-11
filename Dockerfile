FROM veridise/picus:base

# copy current version of Picus
COPY ./ /Picus/

WORKDIR /Picus/
RUN raco make picus.rkt
CMD [ "/bin/bash" ]
