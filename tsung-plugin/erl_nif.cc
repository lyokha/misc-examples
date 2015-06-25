/*
 * =============================================================================
 *
 *       Filename:  erl_nif.cc
 *
 *    Description:  md5hex protocol to Erlang binding
 *
 *        Version:  1.0
 *        Created:  04.04.2013 12:16:45
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#include <erl_nif.h>

#include <cstring>
#include <sstream>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/cstdint.hpp>
#include <arpa/inet.h>

#include "data.h"


namespace
{
    enum { max_length = 1024 };
}


extern "C"
{
    static ERL_NIF_TERM  request( ErlNifEnv *  env, int  argc,
                                  const ERL_NIF_TERM  argv[] )
    {
        if ( argc < 1 )
            return enif_make_badarg( env );

        char    message[ max_length + sizeof( boost::uint32_t ) ];
        size_t  len( 0 );

        if ( ( len = enif_get_string( env, argv[ 0 ],
                                      message + sizeof( boost::uint32_t ),
                                      max_length, ERL_NIF_LATIN1 ) ) <= 0 )
            return enif_make_badarg( env );

        std::ostringstream  message_str;
        Request             request = { message + sizeof( boost::uint32_t ) };

        {
            boost::archive::binary_oarchive  archive( message_str );
            archive << request;
        }

        std::string        data( message_str.str() );
        boost::uint32_t *  size( ( boost::uint32_t * )&message[ 0 ] );

        *size = htonl( data.size() );

        ErlNifBinary       result;

        enif_alloc_binary( sizeof( boost::uint32_t ) + ntohl( *size ), &result );

        std::memcpy( message + sizeof( boost::uint32_t ), data.c_str(),
                     data.size() );
        std::memcpy( result.data, message,
                     sizeof( boost::uint32_t ) + ntohl( *size ) );

        result.size = sizeof( boost::uint32_t ) + ntohl( *size );

        return enif_make_binary( env, &result );
    }

    static ERL_NIF_TERM  response( ErlNifEnv *  env, int  argc,
                                   const ERL_NIF_TERM  argv[] )
    {
        if ( argc < 1 )
            return enif_make_badarg( env );

        ERL_NIF_TERM  message( argv[ 0 ] );

        if ( ! enif_is_binary( env, message ) )
            return enif_make_badarg( env );

        ErlNifBinary    bin;

        enif_inspect_binary( env, message, &bin );

        if ( bin.size < sizeof( boost::uint32_t ) )
            return enif_make_badarg( env );

        //boost::uint32_t *  size( ( boost::uint32_t * )&bin.data );

        //if ( bin.size != sizeof( boost::uint32_t ) + ntohl( *size ) )
            //return enif_make_badarg( env );

        std::istringstream  reply_str;
        Reply               reply;

        reply_str.str( std::string(
                                ( char * )bin.data + sizeof( boost::uint32_t ),
                                bin.size - sizeof( boost::uint32_t ) ) );

        {
            boost::archive::binary_iarchive  archive( reply_str );
            archive >> reply;
        }

        ErlNifBinary  value;
        ErlNifBinary  md5hex;

        enif_alloc_binary( reply.value.size(), &value );
        std::memcpy( value.data, reply.value.c_str(), reply.value.size() );
        value.size = reply.value.size();

        enif_alloc_binary( reply.md5hex.size(), &md5hex );
        std::memcpy( md5hex.data, reply.md5hex.c_str(), reply.md5hex.size() );
        md5hex.size = reply.md5hex.size();

        return enif_make_tuple2( env, enif_make_binary( env, &value ),
                                 enif_make_binary( env, &md5hex ) );
    }

    static ErlNifFunc  ts_p_md5hex_funcs[] =
    {
        { "request", 1, request },
        { "response", 1, response }
    };
}

ERL_NIF_INIT( ts_p_md5hex_nif, ts_p_md5hex_funcs, NULL, NULL, NULL, NULL )

