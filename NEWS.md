# [kwb.db 0.7.0](https://github.com/KWB-R/kwb.db/releases/tag/v0.7.0) <small>2023-09-25</small>

* Add and export `read_schemata()`, `read_relations()`, `merge_relations()`

# [kwb.db 0.6.0](https://github.com/KWB-R/kwb.db/releases/tag/v0.6.0) <small>2021-07-07</small>

* `getDatabaseSchema()`: add arguments "tableNames", "tableTypes"
* `dumpDatabase()`: add argument "as.is"
* `hsFields()`: Try `RODBC::sqlColumns()` before raising an error

# [kwb.db 0.5.0](https://github.com/KWB-R/kwb.db/releases/tag/v0.5.0) <small>2020-06-19</small>

* Try using RODBC functions first before using functions from rodb32 package
  (Since some MS Office version there are 64 Bit RODBC drivers available!)
* Export more functions:
    + `dumpDatabase()`
    + `sqlForSelectByKey()`
    + `sqlForUpdate()`

# [kwb.db 0.4.0](https://github.com/KWB-R/kwb.db/releases/tag/v0.4.0) <small>2019-11-03</small>

* Use the package odbc32 by Vaclav Hausenblas in order to use Window's 32 bit
  ODBC driver from within a 64 bit R session.
* Decrease the number of exported functions. 
  TODO: Check if some functions need to be exported again as they may be used
  in some scripts that were/are used at KWB.

# kwb.db 0.3.0 (2019-06-07) 

* give a warning when using deprecated functions `hsOpenMdb()`, `hsCloseMdb()`

# kwb.db 0.2.0 (2019-06-06)

* support 64 Bit in `odbcConnectionAccess()`

# kwb.db 0.1.1

* xmdb now points to example database in this package not in kwb.base

# kwb.db 0.1.0

* package created with functions that were originally in kwb.base
