databaseInit(Database):-
    Database = {[movies,M],[directors,D]} &
    M = {} &
    D = {}.
    
databaseInv(Database) :- 
    Database = {[movies,M],[directors,D]} & dom(D,M).

addFilm(Database,T_i,Dir_i,Database_) :-
    addFilmOk(Database,T_i,Dir_i,Database_) or
    titleAlreadyExists(Database,T_i,Database_).
    
addFilmOk(Database,T_i,Dir_i,Database_) :-
    Database = {[movies,M],[directors,D]} &
    T_i nin M &
    un(M,{T_i},M_) &
    un(D,{[T_i,Dir_i]},D_) &
    Database_ = {[movies,M_],[directors,D_]}.

titleAlreadyExists(Database,T_i,Database_) :-
  Database = {[movies,M] / _} & 
  set(M) &
  T_i in M &
  Database_ = Database.

removeFilm(Database,T_i,Database_) :-
    removeFilmOk(Database,T_i,Database_) or
    titleDoesNotExist(Database,T_i,Database_).
    
removeFilmOk(Database,T_i,Database_) :-
    Database = {[movies,M],[directors,D]} &
    T_i in M &
    M = {T_i / M_} & 
    T_i nin M_ &
    apply(D,T_i,Dir) &
    D = {[T_i,Dir] / D_ } &
    [T_i,Dir] nin D_ &
    Database_ = {[movies,M_],[directors,D_]}.

titleDoesNotExist(Database,T_i,Database_) :-
  Database = {[movies,M] / _} & 
  set(M) &
  T_i nin M &
  Database_ = Database.



findDirector(Database,T_i,Dir_o,Database_) :-
  findDirectorOk(Database,T_i,Dir_o,Database_)
  or
  titleDoesNotExist(Database,T_i,Database_).

findDirectorOk(Database,T_i,Dir_o,Database_) :-
  Database = {[movies,M],[directors,D]} &
  T_i in M & 
  apply(D,T_i,Dir_o) &
  Database_ = Database.


sameDirector(Database,Dir_i,Films_o,Database_) :-
  Database = {[directors,D] / _} &
  rres(D,{Dir_i},M) &
  dom(M,Films_o) &
  Database_ = Database.

