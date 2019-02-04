/*
 * =============================================================================
 *
 *       Filename:  test.cc
 *
 *    Description:  test Unicode collation
 *
 *        Version:  1.0
 *        Created:  13.05.2015 15:27:24
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#include <boost/locale.hpp>
#include <iostream>

#include <vector>
#include <string>
#include <algorithm>
#include <iterator>

#include <unicode/unistr.h>
#include <unicode/stsearch.h>
#include <unicode/coll.h>

using namespace  boost::locale;

using namespace  icu;


namespace
{
    generator                 gen;
    std::locale               loc( gen( "" ) );
    const collator< char > &  col( std::use_facet< collator < char > >( loc ) );
}


bool  find( const std::vector< std::string > &  p, const std::string &  s,
            collator_base::level_type  level = collator_base::primary )
{
    std::size_t  pos( 0 );
    std::string  snorm( col.transform( level, s ) );

    std::size_t  ssize( snorm.size() - 1 );
    std::size_t  len( ssize );

    for ( std::vector< std::string >::const_iterator  k( p.begin() );
          k != p.end(); ++k )
    {
        std::string  pnorm( col.transform( level, *k ) );
        std::size_t  psize( pnorm.size() - 1 );

        if ( psize > len )
            return false;

        std::string  scur( std::string( snorm, pos, len ) );
        std::size_t  scur_pos( scur.find( pnorm.c_str(), 0, psize ) );

        if ( scur_pos == std::string::npos )
            return false;

        std::size_t  shift( scur_pos + psize );

        pos += shift;
        len -= shift;
    }

    return true;
}


bool  find_icu( const std::vector< std::string > &  p, const std::string &  s,
                Collator::ECollationStrength  strength = Collator::PRIMARY )
{
    int32_t        pos( 0 );
    UnicodeString  su( UnicodeString::fromUTF8( StringPiece( s ) ) );

    for ( std::vector< std::string >::const_iterator  k( p.begin() );
          k != p.end(); ++k )
    {
        UnicodeString  pu( UnicodeString::fromUTF8( StringPiece( *k ) ) );
        UErrorCode     status( U_ZERO_ERROR );

        StringSearch   it( pu, UnicodeString( su, pos ), Locale::getDefault(),
                           NULL, status );

        it.getCollator()->setStrength( strength );

        int32_t        scur_pos( it.first( status ) );

        if ( scur_pos == USEARCH_DONE )
            return false;

        pos += scur_pos + it.getMatchedLength();
    }

    return true;
}


int  main( void )
{
    std::string  a( "Семён зна́ет" );
    std::string  b( "семен знает" );

    bool  eq( col.compare( collator_base::primary, a, b ) == 0 );

    std::cout << a << " and " << b << " are " <<
            ( eq ? "identical" : "different" ) << " on primary comparison" <<
            std::endl;

    eq = col.compare( collator_base::secondary, a, b ) == 0;

    std::cout << a << " and " << b << " are " <<
            ( eq ? "identical" : "different" ) << " on secondary comparison" <<
            std::endl;

    std::string  c( "приём!" );

    std::cout << to_upper( c, loc ) << std::endl;

    std::vector< std::string >  parts;

    parts.push_back( "емен" );
    parts.push_back( "Зн" );
    parts.push_back( "ет" );

    bool  in( find( parts, a ) );

    std::cout << a << ( in ? " contains " : " does not contain " );
    std::copy( parts.begin(), parts.end(),
               std::ostream_iterator< std::string >( std::cout, " " ) );
    std::cout << "on primary comparison" << std::endl;

    in = find_icu( parts, a );

    std::cout << a << ( in ? " contains " : " does not contain " );
    std::copy( parts.begin(), parts.end(),
               std::ostream_iterator< std::string >( std::cout, " " ) );
    std::cout << "on ICU primary comparison" << std::endl;

    in = find_icu( parts, a, Collator::SECONDARY );

    std::cout << a << ( in ? " contains " : " does not contain " );
    std::copy( parts.begin(), parts.end(),
               std::ostream_iterator< std::string >( std::cout, " " ) );
    std::cout << "on ICU secondary comparison" << std::endl;
}

