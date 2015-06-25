/*
 * =============================================================================
 *
 *       Filename:  data.h
 *
 *    Description:  request and reply data serialization
 *
 *        Version:  1.0
 *        Created:  11.04.2013 16:43:34
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Alexey Radkov (), 
 *        Company:  
 *
 * =============================================================================
 */

#ifndef DATA_H
#define DATA_H

#include <string>
#include <boost/serialization/string.hpp>


struct  Request
{
    std::string  value;

    template < typename  Archive >
    void  serialize( Archive &  ar, unsigned int  /* version */ )
    {
        ar & value;
    }
};


struct  Reply
{
    std::string  value;

    std::string  md5hex;

    template < typename  Archive >
    void  serialize( Archive &  ar, unsigned int  /* version */ )
    {
        ar & value;
        ar & md5hex;
    }
};


#endif

