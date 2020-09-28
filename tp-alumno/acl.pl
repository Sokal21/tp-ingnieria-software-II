% Tipos enumerados
PERM = {r, w}.
ANS = {yes, no}.

% Defincion axiomatica
userGroups_da(F) :- pfun(F).

% Estado inicial
aclInit(ACL) :-
  ACL = {[usrs, Us],[grps, Gr]} &
  Us = {} &
  Gr = {}.

% Operaciones

%%%%%%%%%%%%%%%%%%%%%% addUserRight %%%%%%%%%%%%%%%%%%%%%%
addNewUserRight(ACL, U_i, R_i, UserGroups, ACL_) :-
  ACL = {[usrs, Us],[grps, Gr]} &
  userGroups_da(UserGroups) &
  dom(Us, DUs) &
  U_i nin DUs &
  [U_i, _] in UserGroups &
  R_i in PERM &
  oplus(Us, {[U_i, {R_i}]}, Us_) &
  Gr_ = Gr &
  ACL_ = {[usrs, Us_],[grps, Gr_]}.

addExistingUserRight(ACL, U_i, R_i, ACL_) :-
  ACL = {[usrs, Us],[grps, Gr]} &
  R_i in PERM &
  [U_i, PrevPerms] in Us &
  un(PrevPerms, {R_i}, NewPerms) &
  oplus(Us, {[U_i, NewPerms]}, Us_) &
  Gr_ = Gr &
  ACL_ = {[usrs, Us_],[grps, Gr_]}.

userDoesNotExist(ACL, U_i, UserGroups, ACL_) :-
  ACL = ACL_ &
  userGroups_da(UserGroups) &
  dom(UserGroups, DUserGroups) &
  U_i nin DUserGroups.

addUserRight(ACL, U_i, R_i, UserGroups, ACL_) :-
  addNewUserRight(ACL, U_i, R_i, UserGroups, ACL_)
  or
  addExistingUserRight(ACL, U_i, R_i, ACL_)
  or
  userDoesNotExist(ACL, U_i, UserGroups, ACL_).

%%%%%%%%%%%%%%%%%%%%%% addGroupRight %%%%%%%%%%%%%%%%%%%%%%
addNewGroupRight(ACL, G_i, R_i, ACL_) :-
  ACL = {[usrs, Us],[grps, Gr]} &
  dom(Gt, DGr) &
  G_i nin DGr &
  R_i in PERM &
  oplus(Gr, {[G_i, {R_i}]}, Gr_) &
  Us_ = Us &
  ACL_ = {[usrs, Us_],[grps, Gr_]}.

addExistingGroupRight(ACL, G_i, R_i, ACL_) :-
  ACL = {[usrs, Us],[grps, Gr]} &
  R_i in PERM &
  [G_i, PrevPerms] in Gr &
  un(PrevPerms, {R_i}, NewPerms) &
  oplus(Gr, {[G_i, NewPerms]}, Gr_) &
  Us_ = Us &
  ACL_ = {[usrs, Us_],[grps, Gr_]}.

addGroupRight(ACL, G_i, R_i, ACL_) :-
  addNewGroupRight(ACL, G_i, R_i, ACL_)
  or
  addExistingGroupRight(ACL, G_i, R_i, ACL_).

%%%%%%%%%%%%%%%%%%%%%% isReader %%%%%%%%%%%%%%%%%%%%%%
isReaderUser(ACL, U_i, ANS_o, ACL_) :-
  ACL = {[usrs, Us],[grps, Gr]} &
  [U_i, Perms] in Us &
  set(Perms) &
  r in Perms &
  ANS_o = yes &
  ACL_ = ACL.

isReaderGroup(ACL, U_i, ANS_o, UserGroups, ACL_) :-
  consult_lib &
  ACL = {[usrs, Us],[grps, Gr]} &
  userGroups_da(UserGroups) &
  [U_i, Groups] in UserGroups &
  dres(Groups, Gr, GrRes) &
  ran(GrRes, PSets) &
  bun(PSets, Perms) &
  set(Perms) &
  r in Perms &
  ANS_o = yes &
  ACL_ = ACL.

isNotReaderNotInList(ACL, U_i, ANS_o, UserGroups, ACL_) :-
  consult_lib &
  ACL = {[usrs, Us],[grps, Gr]} &
  userGroups_da(UserGroups) &
  [U_i, Groups] in UserGroups &
  dom(Us, DUs) &
  U_i nin DUs &
  dres(Groups, Gr, GrRes) &
  ran(GrRes, PSets) &
  bun(PSets, Perms) &
  set(Perms) &
  r nin Perms &
  ANS_o = no &
  ACL_ = ACL.

isNotReaderInList(ACL, U_i, ANS_o, UserGroups, ACL_) :-
  consult_lib &
  ACL = {[usrs, Us],[grps, Gr]} &
  userGroups_da(UserGroups) &
  [U_i, Groups] in UserGroups &
  [U_i, Perms] in Us &
  set(Perms) &
  r nin Perms &
  dres(Groups, Gr, GrRes) &
  ran(GrRes, PSets) &
  bun(PSets, GPerms) &
  set(GPerms) &
  r nin GPerms &
  ANS_o = no &
  ACL_ = ACL.

isNotReader(ACL, U_i, ANS_o, UserGroups, ACL_) :-
  isNotReaderNotInList(ACL, U_i, ANS_o, UserGroups, ACL_)
  or
  isNotReaderInList(ACL, U_i, ANS_o, UserGroups, ACL_).

isReader(ACL, U_i, ANS_o, UserGroups, ACL_) :-
  isReaderUser(ACL, U_i, ANS_o, ACL_)
  or
  isReaderGroup(ACL, U_i, ANS_o, UserGroups, ACL_)
  or
  isNotReader(ACL, U_i, ANS_o, UserGroups, ACL_)
  or
  userDoesNotExist(ACL, U_i, UserGroups, ACL_).

%%%%%%%%%%%%%%%%%%%%%% isWriter %%%%%%%%%%%%%%%%%%%%%%
isWriterUser(ACL, U_i, ANS_o, ACL_) :-
  ACL = {[usrs, Us],[grps, Gr]} &
  [U_i, Perms] in Us &
  set(Perms) &
  w in Perms &
  ANS_o = yes &
  ACL_ = ACL.

isWriterGroup(ACL, U_i, ANS_o, UserGroups, ACL_) :-
  consult_lib &
  ACL = {[usrs, Us],[grps, Gr]} &
  userGroups_da(UserGroups) &
  [U_i, Groups] in UserGroups &
  dres(Groups, Gr, GrRes) &
  ran(GrRes, PSets) &
  bun(PSets, Perms) &
  set(Perms) &
  w in Perms &
  ANS_o = yes &
  ACL_ = ACL.

isNotWriterNotInList(ACL, U_i, ANS_o, UserGroups, ACL_) :-
  consult_lib &
  ACL = {[usrs, Us],[grps, Gr]} &
  userGroups_da(UserGroups) &
  [U_i, Groups] in UserGroups &
  dom(Us, DUs) &
  U_i nin DUs &
  dres(Groups, Gr, GrRes) &
  ran(GrRes, PSets) &
  bun(PSets, Perms) &
  set(Perms) &
  w nin Perms &
  ANS_o = no &
  ACL_ = ACL.

isNotWriterInList(ACL, U_i, ANS_o, UserGroups, ACL_) :-
  consult_lib &
  ACL = {[usrs, Us],[grps, Gr]} &
  ACL_ = ACL &
  userGroups_da(UserGroups) &
  [U_i, Groups] in UserGroups &
  [U_i, Perms] in Us &
  set(Perms) &
  w nin Perms &
  dres(Groups, Gr, GrRes) &
  ran(GrRes, PSets) &
  bun(PSets, GPerms) &
  set(GPerms) &
  w nin GPerms&
  ANS_o = no &
  ACL_ = ACL.

isNotWriter(ACL, U_i, ANS_o, UserGroups, ACL_) :-
  isNotWriterNotInList(ACL, U_i, ANS_o, UserGroups, ACL_)
  or
  isNotWriterInList(ACL, U_i, ANS_o, UserGroups, ACL_).

isWriter(ACL, U_i, ANS_o, UserGroups, ACL_) :-
  isWriterUser(ACL, U_i, ANS_o, ACL_)
  or
  isWriterGroup(ACL, U_i, ANS_o, UserGroups, ACL_)
  or
  isNotWriter(ACL, U_i, ANS_o, UserGroups, ACL_)
  or
  userDoesNotExist(ACL, U_i, UserGroups, ACL_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Simulaciones

%%%%%%%%%%%%%%%%%%%%%% Simulacion 1 %%%%%%%%%%%%%%%%%%%%%%

ug1(F) :- F = {[antonio, {g1,g2}], [locascio, {g1}]}.

t1(UG,S0,S1,S2,S3,S4,S5,R1,R2) :-
  aclInit(S0)                           &
  ug1(UG)                               &
  addUserRight(S0, antonio, w, UG, S1)  &
  addUserRight(S1, locascio, r, UG, S2) &
  addGroupRight(S2, g2, r, S3)          &
  isReader(S3, antonio, R1, UG, S4)     &
  isWriter(S4, locascio, R2, UG, S5).


%%%%%%%%%%%%%%%%%%%%%% Simulacion 2 %%%%%%%%%%%%%%%%%%%%%%

ug2(F) :- F = {[u1, {g1,g2}], [u2, {g1}], [u3, {}], [u4, {g3}]}.

t2(UG,S0,S1,S2,S3,S4,S5,R1,R2) :-
  aclInit(S0)                     &
  ug2(UG)                         &
  addUserRight(S0, u1, w, UG, S1) &
  addUserRight(S1, u3, r, UG, S2) &
  addGroupRight(S2, g3, r, S3)    &
  addGroupRight(S3, g3, w, S4)    &
  addGroupRight(S4, g2, r, S5)    &
  isWriter(S5, u1, R1, UG, S6)    &
  isReader(S6, u4, R2, UG, S7).


% Pruebas

%%%%%%%%%%%%%%%%%%%%%% Prueba 1 %%%%%%%%%%%%%%%%%%%%%%
p1(S, S_) :-
  S = {[usrs, Us],[grps, Gr]}                 &
  S_ = {[usrs, Us_],[grps, Gr_]}              &
  dom(Us, DUs) & dom(UserGroups, DUserGroups) &
  subset(DUs, DUserGroups)                    &
  addUserRight(S,U,P,UserGroups,S_)           &
  dom(Us_, DUs_)                              &
  nsubset(DUs, DUserGroups).

%%%%%%%%%%%%%%%%%%%%%% Prueba 2 %%%%%%%%%%%%%%%%%%%%%%
p2(S_S_) :-
  S = {[usrs, Us],[grps, Gr]}                 &
  S_ = {[usrs, Us_],[grps, Gr_]}              &
  pfun(Gr)                                    &
  addGroupRight(S, G, P, S_)                  &
  npfun(Gr_).






