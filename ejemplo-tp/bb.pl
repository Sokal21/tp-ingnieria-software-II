% consult('/home/mcristia/fceia/is/material/setlog/bb.pl').

birthdayBookInit(BirthdayBook) :-
  BirthdayBook = {[known,K],[birthday,B]} &
  K = {} &
  B = {}.  

birthdayBookInv(BirthdayBook) :-
  BirthdayBook = {[known,K],[birthday,B]} & dom(B,K).

addBirthday(BirthdayBook,Name_i,Date_i,BirthdayBook_) :-
  addBirthdayOk(BirthdayBook,Name_i,Date_i,BirthdayBook_)
  or
  nameAlreadyExists(BirthdayBook,Name_i,BirthdayBook_).

addBirthdayOk(BirthdayBook,Name_i,Date_i,BirthdayBook_) :-
  BirthdayBook = {[known,K],[birthday,B]} &
  Name_i nin K & 
  un(K,{Name_i},K_) &
  un(B,{[Name_i,Date_i]},B_) &
  BirthdayBook_ = {[known,K_],[birthday,B_]}.

nameAlreadyExists(BirthdayBook,Name_i,BirthdayBook_) :-
  BirthdayBook = {[known,K] / _} & 
  set(K) &
  Name_i in K &
  BirthdayBook_ = BirthdayBook.

findBirthday(BirthdayBook,Name_i,Date_o,BirthdayBook_) :-
  findBirthdayOk(BirthdayBook,Name_i,Date_o,BirthdayBook_)
  or
  notAFriend(BirthdayBook,Name_i,BirthdayBook_).

findBirthdayOk(BirthdayBook,Name_i,Date_o,BirthdayBook_) :-
  BirthdayBook = {[known,K],[birthday,B]} &
  Name_i in K & 
  apply(B,Name_i,Date_o) &
  BirthdayBook_ = BirthdayBook.

notAFriend(BirthdayBook,Name_i,BirthdayBook_) :-
  BirthdayBook = {[known,K] / _} &
  Name_i nin K & 
  BirthdayBook_ = BirthdayBook.

remind(BirthdayBook,Today_i,Cards_o,BirthdayBook_) :-
  BirthdayBook = {[birthday,B] / _} &
  rres(B,{Today_i},M) &
  dom(M,Cards_o) &
  BirthdayBook_ = BirthdayBook.

