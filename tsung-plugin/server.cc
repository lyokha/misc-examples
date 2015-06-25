/*
 * =============================================================================
 *
 *       Filename:  server.cc
 *
 *    Description:  server
 *
 *        Version:  1.0
 *        Created:  11.04.2013 16:14:01
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <boost/bind.hpp>
#include <boost/asio.hpp>
#include <boost/cstdint.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <openssl/md5.h>
#include <arpa/inet.h>

#include "data.h"

using namespace  boost::asio;
using            boost::asio::ip::tcp;


namespace
{
    void  calc_md5hex( unsigned char  src[ MD5_DIGEST_LENGTH ],
                       char  dst[ MD5_DIGEST_LENGTH * 2 ] )
    {
        std::sprintf( dst, "%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x"
                           "%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x",
                      src[ 0 ],  src[ 1 ],  src[ 2 ],  src[ 3 ],
                      src[ 4 ],  src[ 5 ],  src[ 6 ],  src[ 7 ],
                      src[ 8 ],  src[ 9 ],  src[ 10 ], src[ 11 ],
                      src[ 12 ], src[ 13 ], src[ 14 ], src[ 15 ] );
    }
}


class  Session
{
    public:
        explicit Session( io_service &  io ) : socket_( io ), size_( 0 )
        {
        }

        tcp::socket &  socket( void )
        {
            return socket_;
        }

        void  start( void )
        {
            async_read( socket_, buffer( data_, sizeof( boost::uint32_t ) ),
                        boost::bind( &Session::handle_read_size, this,
                                     placeholders::error,
                                     placeholders::bytes_transferred ) );
        }

    private:
        void handle_read_size( const boost::system::error_code &  error,
                               size_t  bytes_transferred )
        {
            if ( ! error )
            {
                size_ =  *( ( boost::uint32_t * )data_ );

                async_read( socket_, buffer( data_, ntohl( size_ ) ),
                            boost::bind( &Session::handle_read, this,
                                         placeholders::error,
                                         placeholders::bytes_transferred ) );
            }
            else
            {
                delete this;
            }
        }

        void handle_read( const boost::system::error_code &  error,
                          size_t  bytes_transferred )
        {
            if ( ! error )
            {
                std::istringstream  message_str;
                Request             request;

                message_str.str( std::string( data_, bytes_transferred ) );

                {
                    boost::archive::binary_iarchive  archive( message_str );
                    archive >> request;
                }

                std::ostringstream  reply_str;
                unsigned char       md5sum[ MD5_DIGEST_LENGTH ];
                char                md5hex[ MD5_DIGEST_LENGTH * 2 ];

                MD5( ( unsigned char * )request.value.c_str(),
                     request.value.size(), md5sum );

                calc_md5hex( md5sum, md5hex );

                Reply  reply = { request.value, md5hex };

                {
                    boost::archive::binary_oarchive  archive( reply_str );
                    archive << reply;
                }

                std::string  data( reply_str.str() );

                if ( data.size() > max_length )
                    throw std::runtime_error( "Too big message" );

                std::memcpy( data_, data.c_str(), data.size() );

                size_ = htonl( data.size() );

                async_write( socket_,
                             buffer( &size_, sizeof( boost::uint32_t ) ),
                             boost::bind( &Session::handle_write_size, this,
                                          boost::asio::placeholders::error,
                                          bytes_transferred ) );
            }
            else
            {
                delete this;
            }
        }

        void handle_write_size( const boost::system::error_code &  error,
                                size_t  size )
        {
            if ( ! error )
            {
                async_write( socket_, buffer( data_, ntohl( size_ ) ),
                             boost::bind( &Session::handle_write, this,
                                          boost::asio::placeholders::error ) );
            }
            else
            {
                delete this;
            }
        }

        void handle_write( const boost::system::error_code &  error )
        {
            if ( ! error )
            {
                socket_.shutdown( tcp::socket::shutdown_both );
            }
            else
            {
                delete this;
            }
        }
    private:
        enum { max_length = 1024 };

    private:
        tcp::socket      socket_;

        char             data_[ max_length ];

        boost::uint32_t  size_;
};


class  Server
{
    public:
        Server( io_service &  io, short  port ) : io_( io ),
            acceptor_( io_, tcp::endpoint( tcp::v4(), port ) )
        {
            start_accept();
        }

    private:
        void  start_accept( void )
        {
            Session *  new_session = new Session( io_ );

            acceptor_.async_accept( new_session->socket(),
                                    boost::bind( &Server::handle_accept, this,
                                        new_session, placeholders::error ) );
        }

        void  handle_accept( Session *  new_session,
                                    const boost::system::error_code &  error )
        {
            if ( ! error )
            {
                new_session->start();
            }
            else
            {
                delete new_session;
            }

            start_accept();
        }

    private:
        io_service &   io_;

        tcp::acceptor  acceptor_;
};


int  main( int  argc, char *  argv[] )
{
    try
    {
        if ( argc != 2 )
        {
            std::cerr << "Usage: server <port>\n";

            return 1;
        }

        io_service  io_;

        Server  serv( io_, std::atoi( argv[ 1 ] ) );

        io_.run();
    }
    catch ( const std::exception &  e )
    {
        std::cerr << "Exception: " << e.what() << "\n";
    }

    return 0;
}

