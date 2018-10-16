from yaks_api import api
import sys

# CREATE TABLE test (id SERIAL NOT NULL PRIMARY KEY, mystring VARCHAR(255),
#  myint INT, myfloat REAL, mydate DATE);
# INSERT INTO test VALUES (1, 'test1', 1, 1.1, '2018-01-01');
# INSERT INTO test VALUES (2, 'test2', 2, 2.2, '2018-02-02');
# INSERT INTO test VALUES (3, 'test3', 3, 3.3, '2018-03-03');


def main():
    print('creating api')
    y = api.YAKS(sys.argv[1])

    print('>> Create memory storage')
    input()
    storage1 = y.create_storage('//is/test/mem',
                                {'is.yaks.backend.kind': 'memory'})

    print('>> Create SQL storage on legacy table "test"')
    input()
    storage2 = y.create_storage('//is/test/db/leg-table',
                                {'is.yaks.backend.kind': 'dbms',
                                 'is.yaks.backend.sql.table': 'test'})

    print('>> Create SQL storage on a new key/value '
          'table which will be droped at storage disposal')
    input()
    storage3 = y.create_storage('//is/test/db/new-table',
                                {'is.yaks.backend.kind': 'dbms',
                                 'is.yaks.backend.sql.on_dispose': 'drop'})

    print('>> Create access')
    input()
    access = y.create_access('//is/test/')

    print('****** SQL storage - key/value table ********')

    print('>> Put //is/test/db/new-table/A/B')
    input()
    access.put('//is/test/db/new-table/A/B', "BCD")

    print('>> Put //is/test/db/new-table/A/D')
    input()
    access.put('//is/test/db/new-table/A/D', "DEF")

    print('>> Put //is/test/db/new-table/A/B/G')
    input()
    access.put('//is/test/db/new-table/A/B/G', "GHI")

    print('>> Put //is/test/db/new-table/A/B/H/I/J')
    input()
    access.put('//is/test/db/new-table/A/B/H/I/J', "JKL")

    print('>> Get //is/test/db/new-table/A/B')
    input()
    print('GET: {}'.format(access.get('//is/test/db/new-table/A/B')))

    print('>> Get //is/test/db/new-table/A/*')
    input()
    print('GET: {}'.format(access.get('//is/test/db/new-table/A/*')))

    print('>> Get //is/test/db/new-table/A/**')
    input()
    print('GET: {}'.format(access.get('//is/test/db/new-table/A/**')))

    print('>> Get //is/test/db/new-table/A/**/J')
    input()
    print('GET: {}'.format(access.get('//is/test/db/new-table/A/**/J')))

    print('>> Remove //is/test/db/new-table/A/D')
    input()
    access.remove('//is/test/db/new-table/A/D')

    print('>> Get //is/test/db/new-table/A/D')
    input()
    print('GET: {}'.format(access.get('//is/test/db/new-table/A/D')))


    print('****** SQL storage - legacy table ********')

    print('>> Get //is/test/db/leg-table')
    input()
    print('GET: {}'.format(access.get('//is/test/db/leg-table')))

    print('>> Put //is/test/db/leg-table')
    input()
    access.put('//is/test/db/leg-table', "4, 'test4', 4, 4.4, '2018-04-04'")

    print('>> Get //is/test/db/leg-table')
    input()
    print('GET: {}'.format(access.get('//is/test/db/leg-table')))

    print('>> Remove //is/test/db/leg-table?id=4')
    input()
    access.remove('//is/test/db/leg-table?id=4',)

    print('>> Get //is/test/db/leg-table')
    input()
    print('GET: {}'.format(access.get('//is/test/db/leg-table')))


    print('****** DISPOSE ALL ********')

    print('>> Dispose Access')
    input()
    access.dispose()

    print('>> Dispose Storage1')
    input()
    storage1.dispose()

    print('>> Dispose Storage2')
    input()
    storage2.dispose()

    print('>> Dispose Storage3')
    input()
    storage3.dispose()

    y.close()
    print('bye!')


if __name__ == '__main__':
    main()
