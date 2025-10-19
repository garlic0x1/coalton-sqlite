# coalton-sqlite/sqlite

## Types

### SQLITECONDITION :: *\**

> Many of the routines in the SQLite C-language Interface return
> numeric result codes indicating either success or failure, and in the
> event of a failure, providing some idea of the cause of the
> failure.

### STATEMENT :: *\**

> Prepared Statement Object.
> 
> An instance of this object represents a single SQL statement that has
> been compiled into binary form and is ready to be evaluated.
> 
> Think of each SQL statement as a separate computer program. The
> original SQL text is source code. A prepared statement object is the
> compiled object code. All SQL must be converted into a prepared
> statement before it can be run.

### DATABASE :: *\**

> Database Connection Handle
> 
> Each open SQLite database is represented by a pointer to an instance
> of the opaque structure named `sqlite3'.

## Values

### STATEMENT-DATABASE :: *(STATEMENT → DATABASE)*

> Find The Database Handle Of A Prepared Statement.

### FINALIZE-STATEMENT :: *(STATEMENT → UNIT)*

> Destroy A Prepared Statement Object.
> 
> The \`finalize-statement' function is called to delete a prepared
> statement. If the most recent evaluation of the statement encountered
> no errors or if the statement is never been evaluated, then
> \`finalize-statement' returns \`Unit'. If the most recent evaluation of
> statement \`stmt' failed, then \`finalize-statement' signals the
> appropriate error code or extended error code.
> 
> The \`finalize-statement' routine can be called at any point during the
> life cycle of prepared \`stmt': before it is ever evaluated, after one
> or more calls to \`reset-statement', or after any call to
> \`step-statement' regardless of whether or not the statement has
> completed execution.
> 
> The application must finalize every prepared statement in order to
> avoid resource leaks. It is a grievous error for the application to
> try to use a prepared statement after it has been finalized. Any use
> of a prepared statement after it has been finalized can result in
> undefined and undesirable behavior such as segfaults and heap
> corruption.

### PREPARE-STATEMENT :: *(DATABASE → STRING → STATEMENT)*

> To execute an SQL statement, it must first be compiled into a
> byte-code program using one of these routines. Or, in other words,
> these routines are constructors for the prepared statement object.

### DATABASE-FILENAME :: *(DATABASE → STRING)*

> The \`database-filename' function returns a string of the filename
> associated with database \`db'. If the database is closed or is a
> temporary or in-memory database, then this function will return an
> empty string.

### RESET-STATEMENT :: *(STATEMENT → UNIT)*

> Reset A Prepared Statement Object.
> 
> The \`reset-statement' function is called to reset a prepared statement
> object back to its initial state, ready to be re-executed. Any SQL
> statement variables that had values bound to them using the \`bind-\*'
> API retain their values. \`clear-statement' to reset the bindings.

### PARAMETER-INDEX :: *(STATEMENT → STRING → UFIX)*

> Return the index of an SQL parameter given its name. The index value
> returned is suitable for use as the second parameter to \`bind-\*'. A
> zero is returned if no matching parameter is found.

### PARAMETER-COUNT :: *(STATEMENT → UFIX)*

> This function can be used to find the number of SQL parameters in a
> prepared statement. SQL parameters are tokens of the form ?, ?NNN,
> :AAA, $AAA, or @AAA that serve as placeholders for values that are
> bound to the parameters at a later time.
> 
> This function actually returns the index of the largest (rightmost)
> parameter. For all forms except ?NNN, this will correspond to the
> number of unique parameters. If parameters of the ?NNN form are used,
> there may be gaps in the list.

### CLEAR-STATEMENT :: *(STATEMENT → UNIT)*

> Reset All Bindings On A Prepared Statement.
> 
> Contrary to the intuition of many, \`reset-statement' does not reset the
> bindings on a prepared statement. Use this routine to reset all host
> parameters to NULL.

### WITH-STATEMENT :: *∀ A. (DATABASE → STRING → (STATEMENT → A) → A)*

> Safely prepare a \`Statement' that will be finalized as the stack unwinds.

### STEP-STATEMENT :: *(STATEMENT → BOOLEAN)*

> Evaluate An SQL Statement.
> 
> After a prepared statement has been prepared using
> \`prepare-statement', this function must be called one or more times to
> evaluate the statement.

### PARAMETER-NAME :: *(STATEMENT → UFIX → STRING)*

> The \`parameter-name' function returns the name of the N-th SQL
> parameter in the prepared statement P. SQL parameters of the form?NNN
> or :AAA or @AAA or $AAA have a name which is the string ?NNN or :AAA
> or @AAA or $AAA respectively. In other words, the initial : or $ or @
> or ? is included as part of the name. Parameters of the form ? without
> a following integer have no name and are referred to as nameless or
> anonymous parameters.
> 
> The first host parameter has an index of 1, not 0.
> 
> If the value N is out of range or if the N-th parameter is nameless,
> then NULL is returned.

### CLOSE-DATABASE :: *(DATABASE → UNIT)*

> The \`close-database' function is a destructor for the \`Database'
> object and deallocates all associated resources. Calls to
> \`close-database' signal \`SqliteError' if the object is not
> successfully destroyed.
> 
> Ideally, applications should finalize all prepared statements prior to
> attempting to close the object. If the database connection is
> associated with unfinalized prepared statements, BLOB handlers, and/or
> unfinished sqlite3_backup objects then \`close-database' will leave the
> database connection open and signal \`ErrBusy'.
> 
> If a \`Database' object is destroyed while a transaction is open, the
> transaction is automatically rolled back.

### WITH-DATABASE :: *∀ A. (STRING → (OPTIONAL (LIST SQLITEOPENFLAG)) → (DATABASE → A) → A)*

> Safely open a \`Database' that will be closed as the stack unwinds.

### OPEN-DATABASE :: *(STRING → (OPTIONAL (LIST SQLITEOPENFLAG)) → DATABASE)*

> Open a SQLite database file as specified by \`path', with optional
> flags.
> 
> Resources associated with the database connection handle should be
> released by passing it to \`close-database' when it is no longer
> required.

### COLUMN-COUNT :: *(STATEMENT → UFIX)*

> Number Of Columns In A Result Set
> 
> Return the number of columns in the result set returned by the
> prepared statement. If this routine returns 0, that means the prepared
> statement returns no data (for example an UPDATE). However, just
> because this routine returns a positive number does not mean that one
> or more rows of data will be returned. A SELECT statement will always
> have a positive column count but depending on the WHERE
> clause constraints and the table content, it might return no rows.

### COLUMN-TYPE :: *(STATEMENT → UFIX → SQLITETYPE)*

> The \`column-type' function returns the datatype code for the
> initial data type of the result column. The returned value is one of
> \`SqliteInt' \`SqliteFloat', \`SqliteText', \`SqliteBlob', or
> \`SqliteNull'. The return value of \`column-type' can be used to decide
> which of the first six interface should be used to extract the column
> value. The value returned by \`column-type' is only meaningful if no
> automatic type conversions have occurred for the value in
> question. After a type conversion, the result of calling \`column-type'
> is undefined, though harmless. Future versions of SQLite may change
> the behavior of \`column-type' following a type conversion.

### COLUMN-TEXT :: *(STATEMENT → UFIX → STRING)*

> Read a string from column.

### COLUMN-NAME :: *(STATEMENT → UFIX → STRING)*

> This function returns the name assigned to a particular column in
> the result set of a SELECT statement. The \`column-name' interface
> returns a string. The first parameter is the prepared statement that
> implements the SELECT statement. The second parameter is the column
> number. The leftmost column is number 0.

### COLUMN-BLOB :: *(STATEMENT → UFIX → (VECTOR U8))*

> Read a blob from column.

### COLUMN-I64 :: *(STATEMENT → UFIX → I64)*

> Read an integer from column.

### COLUMN-F64 :: *(STATEMENT → UFIX → F64)*

> Read a float from column.

### BIND-TEXT :: *(STATEMENT → UFIX → STRING → UNIT)*

> Bind a string to statement.

### BIND-NULL :: *(STATEMENT → UFIX → UNIT)*

> Bind NULL to statement.

### BIND-BLOB :: *(STATEMENT → UFIX → (VECTOR U8) → UNIT)*

> Bind a blob to statement.

### BIND-I64 :: *(STATEMENT → UFIX → I64 → UNIT)*

> Bind an integer to statement.

### BIND-F64 :: *(STATEMENT → UFIX → F64 → UNIT)*

> Bind a float to statement.

# coalton-sqlite/value

## Types

### DYNAMICVALUE :: *\**

> A union type that represents all possible SQLite values.

## Classes

### SQLITEVALUE :: *[(A) :: (\*)]*

> Objects that can be bound and read from statements.

- #### COLUMN-VALUE :: *∀ A. SQLITEVALUE A ⇒ (STATEMENT → UFIX → A)*

> Read a value from column.


- #### BIND-VALUE :: *∀ A. SQLITEVALUE A ⇒ (STATEMENT → UFIX → A → UNIT)*

> Bind a value to statement.



## Values

### COLUMN-DYNAMIC-VALUE :: *(STATEMENT → UFIX → DYNAMICVALUE)*

> Read a dynamic value from column.

### BIND-DYNAMIC-VALUE :: *(STATEMENT → UFIX → DYNAMICVALUE → UNIT)*

> Bind a dynamic value to statement.

### FLOAT :: *(F64 → DYNAMICVALUE)*

> NIL

### TEXT :: *(STRING → DYNAMICVALUE)*

> NIL

### NULL :: *DYNAMICVALUE*

> NIL

### BLOB :: *((VECTOR U8) → DYNAMICVALUE)*

> NIL

### INT :: *(I64 → DYNAMICVALUE)*

> NIL

# coalton-sqlite/query

## Values

### WITH-TRANSACTION :: *∀ A. (DATABASE → (UNIT → A) → A)*

> Call the thunk with all SQLite actions wrapped in a transaction.
> 
> If a condition is signaled, the transaction is rolled back, otherwise,
> it is committed after evaluation.

### QUERY-ONE-COLUMN :: *(DATABASE → STRING → (LIST DYNAMICVALUE) → (OPTIONAL DYNAMICVALUE))*

> Prepare and execute a statement with parameters bound.
> 
> Returns an Optional \`DynamicValue' representing the first column of
> the first row found.

### EXECUTE-STRING :: *(DATABASE → STRING → UNIT)*

> Prepare and execute a simple SQL string.

### QUERY-ONE :: *(DATABASE → STRING → (LIST DYNAMICVALUE) → (OPTIONAL (LIST DYNAMICVALUE)))*

> Prepare and execute a statement with parameters bound.
> 
> Returns an Optional row represented as a list of \`DynamicValue' objects.

### EXECUTE :: *(DATABASE → STRING → (LIST DYNAMICVALUE) → UNIT)*

> Prepare and execute a statement with parameters bound.

### DO-ROWS :: *(DATABASE → STRING → (LIST DYNAMICVALUE) → ((LIST DYNAMICVALUE) → UNIT) → UNIT)*

> Call \`func' on every row yielded by a query.

### QUERY :: *(DATABASE → STRING → (LIST DYNAMICVALUE) → (LIST (LIST DYNAMICVALUE)))*

> Prepare and execute a statement with parameters bound.
> 
> Returns a list of rows represented as lists of \`DynamicValue' objects.

# coalton-sqlite/cache

## Types

### STATEMENTCACHE :: *\**

> A synchronous FIFO replacement cache for SQlite statements.
> 
> Finalizing statements that are not already in the cache causes
> them to be enqueued into the cache, deferring ~real~ finalization
> until more space is needed.

## Values

### WITH-CACHED-STATEMENT :: *∀ A. (STATEMENTCACHE → STRING → (STATEMENT → A) → A)*

> Get a \`Statement' from \`cache' that will be put back in the cache as
> the stack unwinds, possibly finalizing some other \`Statement'.

### WITH-STATEMENT-CACHE :: *∀ A. (DATABASE → UFIX → (STATEMENTCACHE → A) → A)*

> Create a \`StatementCache' using \`db' holding up to \`size' statements.
> 
> Usage of a \`StatementCache' shall be constrained to a single thread.

