#!/bin/bash
URL_BASE=http://localhost:8000
URL_ADMIN=$URL_BASE/@/local/plugins/yaks


function GET ()
{
    echo "GET $1"
    curl -g $1
    echo ""
}

function EVAL ()
{
    echo "EVAL $1"
    curl -H "is.yaks.eval;" $1
    echo ""
}

function PUT ()
{
    echo "PUT $1"
    curl -X PUT -d $2 $1
}

function DEL ()
{
    echo "DEL $1"
    curl -X DELETE $1
}

# Get all from admin space
GET $URL_ADMIN/**
echo ">"; read

# Create memory storage
PUT $URL_ADMIN/backend/Memory/storage/s1 '{"selector":"/is/test/mem/**"}' 

# Create SQL storage on legacy table "test"
PUT $URL_ADMIN/backend/SQL/storage/s2 '{"selector":"/is/test/db/leg-table","is.yaks.backend.sql.table":"test"}' 

# Create "auto" storage (using kind=dbms) on on a new key/value
PUT $URL_ADMIN/backend/auto/storage/s3 '{"selector":"/is/test/db/new-table/**","is.yaks.backend.kind":"dbms","is.yaks.backend.sql.on_dispose":"drop"}'

# Get all from admin space
GET $URL_ADMIN/**
echo ">"; read

####### SQL storage - key/value table #######
echo "==== Tests on key/value table (/is/test/db/new-table)"
PUT $URL_BASE/is/test/db/new-table/A/B 'BCD' 
PUT $URL_BASE/is/test/db/new-table/A/D 'DEF' 
PUT $URL_BASE/is/test/db/new-table/A/B/G 'GHI' 
PUT $URL_BASE/is/test/db/new-table/A/B/H/I/JKL 'JKL' 

GET $URL_BASE/is/test/db/new-table/A/B
GET $URL_BASE/is/test/db/new-table/A/*
GET $URL_BASE/is/test/db/new-table/A/**
GET $URL_BASE/is/test/db/new-table/A/**/J*
echo ">"; read

# replacement
GET $URL_BASE/is/test/db/new-table/A/B/G
PUT $URL_BASE/is/test/db/new-table/A/B/G 'XXXX' 
GET $URL_BASE/is/test/db/new-table/A/B/G

# deletion
GET $URL_BASE/is/test/db/new-table/A/*
DEL $URL_BASE/is/test/db/new-table/A/D
GET $URL_BASE/is/test/db/new-table/A/*

####### SQL storage - legacy table #######
echo "==== Tests on legacy table (/is/test/db/leg-table)"
GET $URL_BASE/is/test/db/leg-table
echo ">"; read

####### DISPOSE ALL #######
echo "==== DISPOSE ALL"
DEL $URL_ADMIN/backend/Memory/storage/s1
DEL $URL_ADMIN/backend/SQL/storage/s2
DEL $URL_ADMIN/backend/SQL/storage/s3
GET $URL_ADMIN/**

