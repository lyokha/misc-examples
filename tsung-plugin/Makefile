BOOST_LIBS      := -lboost_system-mt -lboost_serialization-mt
CRYPTO_LIB      := -lcrypto
ERL_NIF_INCLUDE := -I/usr/lib64/erlang/usr/include

SERVER  := server
CLIENT  := client
ERL_NIF := ts_p_md5hex_nif.so


all : server client erl_nif

server : server.cc
	g++ -Wall -g -o $(SERVER) server.cc $(BOOST_LIBS) $(CRYPTO_LIB) -lpthread

client: client.cc
	g++ -Wall -g -o $(CLIENT) client.cc $(BOOST_LIBS) -lpthread

erl_nif: erl_nif.cc
	g++ -Wall -fPIC -shared -o $(ERL_NIF) $(ERL_NIF_INCLUDE) erl_nif.cc \
		$(BOOST_LIBS) -lpthread

clean:
	rm $(SERVER) $(CLIENT) $(ERL_NIF)
