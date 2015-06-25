/*
 * =============================================================================
 *
 *       Filename:  client.cc
 *
 *    Description:  test client
 *
 *        Version:  1.0
 *        Created:  11.04.2013 16:28:46
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#include <cstring>
#include <sstream>
#include <iostream>
#include <boost/asio.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/cstdint.hpp>
#include <arpa/inet.h>

#include "data.h"

using namespace  boost::asio;
using            boost::asio::ip::tcp;


namespace
{
    enum { max_length = 1024 };
}


int  main( int  argc, char *  argv[] )
{
    try
    {
        if ( argc != 3 )
        {
            std::cerr << "Usage: client <host> <port>\n";

            return 1;
        }

        io_service  io_;

        tcp::resolver            resolver( io_ );
        tcp::resolver::query     query( tcp::v4(), argv[ 1 ], argv[ 2 ] );
        tcp::resolver::iterator  iterator( resolver.resolve( query ) );
        tcp::socket              sock( io_ );

        connect( sock, iterator );

        std::cout << "Enter message: ";

        char  message[ max_length ];

        std::cin.getline( message, max_length );

        std::ostringstream  message_str;
        Request             request = { message };

        {
            boost::archive::binary_oarchive  archive( message_str );
            archive << request;
        }

        std::string  data( message_str.str() );

        if ( data.size() > max_length )
            throw std::runtime_error( "Too big message" );

        boost::uint32_t  size( htonl( data.size() ) );

        write( sock, buffer( &size, sizeof( boost::uint32_t ) ) );
        write( sock, buffer( data.c_str(), data.size() ) );

        size_t    reply_length( read( sock, buffer( message,
                                                sizeof( boost::uint32_t ) ) ) );

        if ( reply_length != sizeof( boost::uint32_t ) )
            throw std::runtime_error( "Error while reading message size" );

        size = ntohl( *( ( boost::uint32_t * )message ) );

        reply_length = read( sock, buffer( message, size ) );

        if ( reply_length != size )
            throw std::runtime_error( "Error while reading message" );

        std::istringstream  reply_str;
        Reply               reply;

        reply_str.str( std::string( message, size ) );

        {
            boost::archive::binary_iarchive  archive( reply_str );
            archive >> reply;
        }

        std::cout << "Reply is: {'" << reply.value << "', '" << reply.md5hex <<
                "'}" << std::endl;
    }
    catch ( const std::exception &  e )
    {
        std::cerr << "Exception: " << e.what() << "\n";
    }

    return 0;
}

