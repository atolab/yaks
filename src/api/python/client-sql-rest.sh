#!/bin/bash 

# CREATE TABLE test (id SERIAL NOT NULL PRIMARY KEY, mystring VARCHAR(255),
#  myint INT, myfloat REAL, mydate DATE);
# INSERT INTO test VALUES (1, "test1", 1, 1.1, "2018-01-01";
# INSERT INTO test VALUES (2, "test2", 2, 2.2, "2018-02-02";
# INSERT INTO test VALUES (3, "test3", 3, 3.3, "2018-03-03";


    echo "creating api"
    y = api.YAKS(sys.argv[1])

    echo ">> Create memory storage"
    read
    # storage1 = y.create_storage("//is/test/mem",
    #                             {"is.yaks.backend.kind": "memory"})
    curl -X POST "http://localhost:8000/yaks/storages?path=//is/test/mem&is.yaks.backend.kind=memory"

    echo ">> Create SQL storage on legacy table "test""
    read
    # storage2 = y.create_storage("//is/test/db/leg-table",
    #                             {"is.yaks.backend.kind": "dbms",
    #                              "is.yaks.backend.sql.table": "test"})
    curl -X POST "http://localhost:8000/yaks/storages?path=//is/test/db/leg-table&is.yaks.backend.kind=dbms&is.yaks.backend.sql.table"


    echo ">> Create SQL storage on a new key/value "
          "table which will be droped at storage disposal"
    read
    # storage3 = y.create_storage("//is/test/db/new-table",
    #                             {"is.yaks.backend.kind": "dbms",
    #                              "is.yaks.backend.sql.on_dispose": "drop"})
    curl -X POST http://localhost:8000/yaks/storages?path=//is/test/db/new-table&is.yaks.backend.kind=dbms&is.yaks.backend.sql.on_dispose=drop

    echo ">> Create access"
    read
    access = y.create_access("//is/test/"

    echo "****** SQL storage - key/value table ********"

    echo ">> Put //is/test/db/new-table/A"
    read
    access.put("//is/test/db/new-table/A", "ABC"

    echo ">> Put //is/test/db/new-table/D"
    read
    access.put("//is/test/db/new-table/D", "DEF"

    echo ">> Put //is/test/db/new-table/G"
    read
    access.put("//is/test/db/new-table/G", "GHI"

    echo ">> Get //is/test/db/new-table/A"
    read
    echo "GET: {}".format(access.get("//is/test/db/new-table/A"))

    echo ">> Get //is/test/db/new-table/D"
    read
    echo "GET: {}".format(access.get("//is/test/db/new-table/D"))

    echo ">> Remove //is/test/db/new-table/D"
    read
    access.remove("//is/test/db/new-table/D"

    echo ">> Get //is/test/db/new-table/D"
    read
    echo "GET: {}".format(access.get("//is/test/db/new-table/D"))


    echo "****** SQL storage - legacy table ********"

    echo ">> Get //is/test/db/leg-table"
    read
    echo "GET: {}".format(access.get("//is/test/db/leg-table"))

    echo ">> Put //is/test/db/leg-table"
    read
    access.put("//is/test/db/leg-table", "4, "test4", 4, 4.4, "2018-04-04""

    echo ">> Get //is/test/db/leg-table"
    read
    echo "GET: {}".format(access.get("//is/test/db/leg-table"))

    echo ">> Remove //is/test/db/leg-table?id=4"
    read
    access.remove("//is/test/db/leg-table?id=4",)

    echo ">> Get //is/test/db/leg-table"
    read
    echo "GET: {}".format(access.get("//is/test/db/leg-table"))


    echo "****** DISPOSE ALL ********"

    echo ">> Dispose Access"
    read
    access.dispose()

    echo ">> Dispose Storage1"
    read
    storage1.dispose()

    echo ">> Dispose Storage2"
    read
    storage2.dispose()

    echo ">> Dispose Storage3"
    read
    storage3.dispose()

    y.close()
    echo "bye!"


