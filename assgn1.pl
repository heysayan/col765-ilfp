/*structure of file:
sections
1. code
	a. facts
    b. rules
2. test cases
*/

/*<-----code------>*/

:- use_module(library(lists)).
/* <--------- House of Targaryens ------------>*/

/* FACTS */
source(targaryenfamilytree,
'https://awoiaf.westeros.org/index.php/House_Targaryen#Targaryen_Family_Tree').

born(targaryen,aenar).
male(aenar).%unknown wives and siblings
female(aenar_wife).
married(aenar,aenar_wife).married(aenar_wife,aenar).

born(aenar,gaemon_aenar).born(aenar_wife,gaemon_aenar).
male(gaemon_aenar).
alias(gaemon_aenar,gaenar_the_glorious).
alias(gaenar_the_glorious,gaemon_aenar).

born(aenar,daenys).born(aenar_wife,daenys).
female(daenys).
alias(daenys,daenys_the_dreamer).
alias(daenys_the_dreamer,daenys).

married(gaemon_aenar,daenys).married(daenys,gaemon_aenar).

born(gaemon_aenar,aegon_gaemon).born(daenys,aegon_gaemon).
male(aegon_gaemon).

born(gaemon_aenar,elaena).born(daenys,elaena).
female(elaena).

% gaemon and daenys had one more daughter
born(gaemon_aenar,gaemon_daughter_last).born(daenys,gaemon_daughter_last).
female(gaemon_daughter_last).

married(aegon_gaemon,elaena).married(elaena,aegon_gaemon).

born(aegon_gaemon,maegon).born(elaena,maegon).
male(maegon).

born(aegon_gaemon,aerys).born(elaena,aerys).
male(aerys).

% aerys had unknown wife
female(aerys_wife).
married(aerys,aerys_wife).married(aerys_wife,aerys).

born(aerys,aelyx).born(aerys_wife,aelyx).
male(aelyx).

born(aerys,baelon).born(aerys_wife,baelon).
male(baelon).

born(aerys,daemion).born(aerys_wife,daemion).
male(daemion).

% daemion had unknown wife
female(daemion_wife).
married(daemion,daemion_wife).married(daemion_wife,daemion).

born(daemion,aerion).born(daemion_wife,aerion).
male(aerion).

female(valaena_velaryon).
married(aerion,valaena_velaryon).married(valaena_velaryon,aerion).

born(aerion,visenya).born(valaena_velaryon,visenya).
female(visenya).

born(aerion,aegon_I).born(valaena_velaryon,aegon_I).
male(aegon_I).

alias(aegon_I,aegon_the_conqueror).
alias(aegon_the_conqueror,aegon_I).

alias(aegon_I,aegon_the_dragon).
alias(aegon_the_dragon,aegon_I).

born(aerion,rhaenys).born(valaena_velaryon,rhaenys).
female(rhaenys).

%aerion had a bastard orys
%orys is the founder of baratheon family
born(aerion,orys_baratheon).
born(aerion_extra_marital,orys_baratheon).
male(orys_baratheon).

married(aegon_I,visenya).married(visenya,aegon_I).
married(aegon_I,rhaenys).married(rhaenys,aegon_I).

born(aegon_I,maegor_I).born(visenya,maegor_I).
male(maegor_I).

alias(maegor_I,maegor_the_cruel).
alias(maegor_the_cruel,maegor_I).

born(aegon_I,aenys_I).born(rhaenys,aenys_I).
male(aenys_I).

% maegor_I had six wives
married(maegor_I,ceryse_hightower).
married(ceryse_hightower,maegor_I).
female(ceryse_hightower).

married(maegor_I,alys_harroway).
married(alys_harroway,maegor_I).
female(alys_harroway).

married(maegor_I,tyanna).
married(tyanna,maegor_I).
female(tyanna).

married(maegor_I,elinor_costayne).
married(elinor_costayne,maegor_I).
female(elinor_costayne).

married(maegor_I,jeyne_westerling).
married(jeyne_westerling,maegor_I).
female(jeyne_westerling).

married(maegor_I,rhaena).
married(rhaena,maegor_I).
female(rhaena).

married(aenys_I,alyssa_velaryon).
married(alyssa_velaryon,aenys_I).
female(alyssa_velaryon).

born(aenys_I,rhaena).born(alyssa_velaryon,rhaena).

born(aenys_I,aegon_aenys).born(alyssa_velaryon,aegon_aenys).
male(aegon_aenys).

born(aenys_I,viserys).born(alyssa_velaryon,viserys).
male(viserys).

born(aenys_I,jaehaerys_I).born(alyssa_velaryonjaehaerys_I).
male(jaehaerys_I).

born(aenys_I,alysanne).born(alyssa_velaryon,alysanne).
female(alysanne).

born(aenys_I,vaella).born(alyssa_velaryon,vaella).
female(vaella).

married(aegon_aenys,rhaena).married(rhaena,aegon_aenys).

married(jaehaerys_I,alysanne).married(alysanne,jaehaerys_I).

born(aegon_aenys,aerea).born(rhaena,aerea).
female(aerea).
born(aegon_aenys,rhaella).born(rhaena,rhaella).
female(rhaella).
twin(aerea,rhaella).twin(rhaella,aerea).

born(jaehaerys_I,aegon_jaehaerys).born(alysanne,aegon_jaehaerys).
male(aegon_jaehaerys).

born(jaehaerys_I,aemon).born(alysanne,aemon).
male(aemon).

born(jaehaerys_I,daenerys).born(alysanne,daenerys).
female(daenerys).

born(jaehaerys_I,daella).born(alysanne,daella).
female(daella).

born(jaehaerys_I,gaemon_jaehaerys).born(alysanne,gaimon_jaehaerys).
male(gaemon_jaehaerys).

born(jaehaerys_I,valerion).born(alysanne,valerion).
male(valerion).

born(jaehaerys_I,baelon).born(alysanne,baelon).
male(baelon).

born(jaehaerys_I,vaegon).born(alysanne,vaegon).
male(vaegon).

born(jaehaerys_I,alyssa).born(alysanne,alyssa).
female(alyssa).

born(jaehaerys_I,maegelle).born(alysanne,maegelle).
female(maegelle).

born(jaehaerys_I,viserra).born(alysanne,viserra).
female(viserra).

born(jaehaerys_I,saera).born(alysanne,saera).
female(saera).

born(jaehaerys_I,gael).born(alysanne,gael).
female(gael).

married(aemon,jocelyn_baratheon).
married(jocelyn_baratheon,aemon).
female(jocelyn_baratheon).

married(daella,rodrik_arryn).
married(rodrik_arryn,daella).
male(rodrik_arryn).

married(baelon,alyssa).
married(alyssa,baelon).

born(aemon,rhaenys_jocelyn).
born(jocelyn_baratheon,rhaenys_jocelyn).
female(rhaenys_jocelyn).

married(rhaenys_jocelyn,corlys_velaryon).
married(corlys_velaryon,rhaenys_jocelyn).
male(corlys_velaryon).

born(daella,aemma_arryn).
born(rodrik_arryn,aemma_arryn).
female(aemma_arryn).

born(baelon,viserys_I).
born(alyssa,viserys_I).
male(viserys_I).

alias(viserys_I,the_young_king).
alias(the_young_key,viserys_I).

born(baelon,daemon_baelon).
born(alyssa,daemon_baelon).
male(daemon_baelon).

born(baelon,aegon_baelon).
born(alyssa,aegon_baelon).
male(aegon_baelon).

married(aemma_arryn,viserys_I).
married(viserys_I,aemma_arryn).

married(alicent_hightower,viserys_I).
married(viserys_I,alicent_hightower).
female(alicent_hightower).

married(daemon_baelon,rhea_royce).
married(rhea_royce,daemon_baelon).
female(rhea_royce).

born(corlys_velaryon,laena_velaryon).
born(rhaenys_jocelyn,laena_velaryon).
female(laena_velaryon).

born(corlys_velaryon,laenor_velaryon).
born(rhaenys_jocelyn,laenor_velaryon).
male(laenor_velaryon).

married(laena_velaryon,daemon_baelon).
married(daemon_baelon,laena_velaryon).

married(laenor_velaryon,rhaenyra).
married(rhaenyra,laenor_velaryon).
female(rhaenyra).

married(rhaenyra,daemon_baelon).
married(daemon_baelon,rhaenyra).

born(aemma_arryn,rhaenyra).
born(viserys_I,rhaenyra).
female(rhaenyra).

alias(rhaenyra,the_realms_delight).
alias(the_realms_delight,rhaenyra).

born(aemma_arryn,baelon_viserys).
born(viserys_I,baelon_viserys).
male(baelon_viserys).

born(aemma_arryn,viserys_unnamed_son).
born(viserys_I,viserys_unnamed_son).
male(viserys_unnamed_son).

born(viserys_I,aegon_II).
born(alicent_hightower,aegon_II).
male(aegon_II).

alias(aegon_II,aegon_the_elder).
alias(aegon_the_elder,aegon_II).

born(viserys_I,helaena).
born(alicent_hightower,helaena).
female(helaena).

born(viserys_I,aemond).
born(alicent_hightower,aemond).
male(aemond).

born(viserys_I,daeron).
born(alicent_hightower,daeron).
male(daeron).

married(aegon_II,helaena).
married(helaena,aegon_II).

/* 13th generation of targaryen family ends here.*/
adopted(no_one,none).
/* ALTHOUGH I added support for defining adoption,
very focused on maintaining the purity of their bloodline,
often marrying within the family. So, adoption was so scarce that
it was not seen in entire 13 generations!!*/




/* RULES */
% same person with different names or clone of person
alias(X,Y) :- X=Y.
%% Note: being identical twin is different than alias
% different persons if not alias or clone
diff(X,Y) :- X\=Y, \+alias(X,Y), \+alias(Y,X).

/* born and adopted are rather taken as basic predicate
   to define child.*/
% born(X,Y) if X born Y, adopted(X,Y) if X adopted Y
child(X,Y) :- born(Y,X).
child(X,Y) :- adopted(Y,X).
%% if Y is spouse of legal parent
child(X,Y) :- married(Y,W), child(X,W).
%%if parent has alias or clone
child(X,Y) :- alias(Y,W), child(X,W).
%%if child has alias or clone
child(X,Y) :- alias(X,W), child(W,Y).
%%if X is child in law of Y
child_in_law(X,Y):-child(W,Y),married(W,X).
% bastard(X,Y) if X is bastard of Y
bastard(X,Y) :-( born(Y,X), %_ born by Y
	%_ Y didn't marry other biological parent
	( born(W,X),diff(W,Y),\+married(Y,W) )
    ).


% parent defined using child predicate
parent(X,Y) :- child(Y,X).
%% parent classification
step_parent(X,Y) :- (
	\+born(X,Y),\+adopted(X,Y) %_ neither born nor adopted
    ), %_ and married to Y's parent
	( married(X,W), child(Y,W) ).
adoptive_parent(X,Y) :- adopted(X,Y).
parent_in_law(X,Y) :- married(W,Y),child(W,X).
/*Note: one can only be classified into one parent category at a time*/

% first degree related if child(or clone of child) or,
%+ parent(or clone or twin of parent) of someone.
first_degree(X,Y) :- child(X,Y);child(Y,X).
%% first degree gender wise relations
son(X,Y) :- child(X,Y),male(X).
step_son(X,Y) :- step_parent(Y,X),male(X).
adoptive_son(X,Y) :-  adoptive_parent(Y,X),male(X).
son_in_law(X,Y):- child_in_law(X,Y),male(X).
bastard_son(X,Y) :- bastard(X,Y),male(X).
father(X,Y) :- parent(X,Y),male(X).
step_father(X,Y) :- step_parent(X,Y),male(X).
adoptive_father(X,Y) :- adoptive_parent(X,Y),male(X).
father_in_law(X,Y):-parent_in_law(X,Y),male(X).

daughter(X,Y) :- child(X,Y),female(X).
step_daughter(X,Y) :- step_parent(Y,X),female(X).
adoptive_daughter(X,Y) :- adoptive_parent(Y,X),female(X).
daughter_in_law(X,Y):- child_in_law(X,Y),female(X).
bastard_daughter(X,Y) :- bastard(X,Y),female(X).
mother(X,Y) :- parent(X,Y),female(X).
step_mother(X,Y) :- step_parent(X,Y),female(X).
adoptive_mother(X,Y) :- adoptive_parent(X,Y),female(X).
mother_in_law(X,Y):-parent_in_law(X,Y),female(X).


% Sibling relations
sibling(X,Y) :- child(X,W), child(Y,W). %_some common parent
%%if X and Y shares both biological parents,they're full-siblings
full_sibling(X,Y) :-( born(W,X), born(V,X), diff(W,V),
	born(W,Y), born(V,Y) ).
%%if exactly one biological parent is common, they're half-siblings
half_sibling(X,Y) :- born(W,X), born(V,X), diff(W,V),
    ( %_both biolgical parents should not be common simultaneously
        (born(W, Y), \+ born(V, Y));%_ Y shares parent W but not V
        (born(V, Y), \+ born(W, Y)) %_ Y shares parent V but not W
    ).
%%if one common biological parent and unshared parents
%%+ are first degree relatives, they're three-quarter siblings
three_quarter_sibling(X,Y) :-born(W,X), born(V,X), different(W,V),
( %_both biolgical parents should not be common simultaneously
	%%_ Y born by W but not V,and V is Ist degree
    %%_ related to other biological parent of Y.
	(born(W, Y), \+ born(V, Y),(born(Z,Y),diff(Z,W),
    first_degree(V,Z)));
    %%_ Y born by V but not W,and W is Ist degree
    %%_ related to other biological parent of Y.
	(born(V, Y), \+ born(W, Y),(born(U,Y),diff(U,V),
    first_degree(W,U)))
).
%% child(of any kind) of your stepparent is your stepsibling.
step_sibling(X,Y) :- step_parent(W,X), parent(W,Y).
%% either Y is adopted by X's parent or X is adopted by Y's parent,
%%+ irrespective of what kind of child X and Y are to there own parent.
adoptive_sibling(X,Y) :-( (parent(W,X), adopted(W,Y));
	(parent(V,Y),adopted(V,X)) ).
%% in law siblings
sibling_in_law(X,Y) :- sibling(X,W),married(W,Y).

% all kind of brothers defined using kinds of sibling
%% X is brother to Y.
brother(X,Y) :- sibling(X,Y),male(X).
%% X is half brother to Y.
half_brother(X,Y) :- half_sibling(X,Y),male(X).
%% X is third-quarter brother to Y.
three_quarter_brother(X,Y) :- three_quarter_sibling(X,Y),male(X).
%% X is step brother to Y.
step_brother(X,Y) :- step_sibling(X,Y),male(X).
%% X is adoptive brother to Y.
adoptive_brother(X,Y) :- adoptive_sibling(X,Y),male(X).
%% X is brother-in-law of Y
brother_in_law(X,Y) :- sibling_in_law(X,Y),male(X).


% all kind of sisters defined using kinds of sibling
%% X is sister to Y.
sister(X,Y) :- sibling(X,Y),female(X).
%% X is half-sister to Y.
half_sister(X,Y) :- half_sibling(X,Y),female(X).
%% X is third-quarter sister to Y.
three_quarter_sister(X,Y) :- three_quarter_sibling(X,Y),female(X).
%% X is step-sister to Y.
step_sister(X,Y) :- step_sibling(X,Y),female(X).
%% X is adoptive sister to Y.
adoptive_sister(X,Y) :- adoptive_sibling(X,Y),female(X).
%% X is sister-in-law of Y
sister_in_law(X,Y) :- sibling_in_law(X,Y),female(X).

% Spouse specification
spouse(X,Y) :- married(X,Y).
husband(X,Y) :- married(X,Y), male(X).
wife(X,Y) :- married(Y,X), female(X).


% siblings of parents
uncle(X,Y) :- parent(W,Y), sibling(W,Z), male(Z), alias(X,Z).
aunt(X,Y) :- parent(W,Y), sibling(W,Z), female(Z), alias(X,Z).
maternal_uncle(X,Y):-mother(W,Y), sibling(W,Z), male(Z), alias(X,Z).
maternal_aunt(X,Y):-mother(W,Y), sibling(W,Z), female(Z), alias(X,Z).
paternal_uncle(X,Y):-father(W,Y), sibling(W,Z), male(Z), alias(X,Z).
paternal_aunt(X,Y):-father(W,Y), sibling(W,Z), female(Z), alias(X,Z).
step_aunt(X,Y):-step_parent(W,Y), sibling(W,Z), female(Z), alias(X,Z).
step_uncle(X,Y):-step_parent(W,Y), sibling(W,Z), male(Z), alias(X,Z).
aunt_by_marriage(X,Y):-married(X,W), uncle(W,Y).
uncle_by_marriage(X,Y):-married(X,W), aunt(W,Y).
%%siblings' children
nephew(X,Y) :- child(X,W),male(X),
	(sibling(W,Y);sibling_in_law(W,Y)).
niece(X,Y) :- child(X,W),female(X),
	(sibling(W,Y);sibling_in_law(W,Y)).
%% X is cousin of Y
cousin(X,Y):- parent(W,X),parent(V,Y),
	sibling(W,V).
cousin_brother(X,Y):-cousin(X,Y),male(X).
cousin_sister(X,Y):-cousin(X,Y),female(X).

% grand relatives
grandparent(X,Y):-parent(W,Y), parent(X,W).
grandparent(X,Y):-grandparent(W,Y),
	(sibling(X,W);sibling_in_law(X,W)).
grandfather(X,Y):-grandparent(X,Y),male(X).
grandmother(X,Y):-grandparent(X,Y),female(X).
maternal_grandparent(X,Y):-mother(W,Y),
	(daughter(W,X);niece(W,X)).
paternal_grandparent(X,Y):-father(W,Y),
	(son(W,X);nephew(W,X)).
maternal_grandfather(X,Y):-maternal_grandparent(X,Y),male(X).
maternal_grandmother(X,Y):-maternal_grandparent(X,Y),female(X).
paternal_grandfather(X,Y):-paternal_grandparent(X,Y),male(X).
paternal_grandmother(X,Y):-paternal_grandparent(X,Y),female(X).

grandchild(X,Y):- grandparent(Y,X).
grandson(X,Y):- grandparent(Y,X),male(X).
granddaughter(X,Y):- grandparent(Y,X),female(X).

%greatgrand relatives
greatgrandparent(X,Y):-grandparent(W,Y), parent(X,W).
greatgrandparent(X,Y):-greatgrandparent(W,Y),
	(sibling(X,W);sibling_in_law(X,W)).
greatgrandfather(X,Y):-greatgrandparent(X,Y),male(X).
greatgrandmother(X,Y):-greatgrandparent(X,Y),female(X).
maternal_greatgrandparent(X,Y):-mother(W,Y),
	grandchild(W,X).
paternal_greatgrandparent(X,Y):-father(W,Y),
	grandchild(W,X).
maternal_greatgrandfather(X,Y):-maternal_greatgrandparent(X,Y),male(X).
maternal_greatgrandmother(X,Y):-maternal_greatgrandparent(X,Y),female(X).
paternal_greatgrandfather(X,Y):-paternal_greatgrandparent(X,Y),male(X).
paternal_greatgrandmother(X,Y):-paternal_greatgrandparent(X,Y),female(X).

greatgrandchild(X,Y) :- greatgrandparent(Y,X).
greatgrandson(X,Y):- greatgrandparent(Y,X),male(X).
greatgranddaughter(X,Y):- greatgrandparent(Y,X),female(X).

% ancestor is parent or ancestor of parent
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,W), ancestor(W,Y).

% if one causes his existence, a time paradox
bootstrapped(X) :- ancestor(X,X).

/* <--code ends here--->*/

/* <-----test cases---->
?- parent(X,aenar).
X = targaryen

?- child(X,aenar).
X = daenys
X = gaemon_aenar
ref=https://awoiaf.westeros.org/index.php/Aenar_Targaryen

?- alias(daenys,Z).
Z = daenys_the_dreamer
ref = https://awoiaf.westeros.org/index.php/Daenys_Targaryen

?- alias(gaemon_aenar,Z).
Z = gaenar_the_glorious
ref = https://awoiaf.westeros.org/index.php/Aenar_Targaryen

?- spouse(aenar,X).
X = aenar_wife

?- spouse(daenys,X).
X = gaemon_aenar
ref = https://awoiaf.westeros.org/index.php/Daenys_Targaryen

?- sibling(daenys,X).
X = gaemon_aenar
ref = https://awoiaf.westeros.org/index.php/Daenys_Targaryen

?- full_sibling(X,daenys).
X = gaemon_aenar
ref = https://awoiaf.westeros.org/index.php/Gaemon_Targaryen_(son_of_Aenar)

?- child(X,daenys).
X = elaena
X = aegon_gaemon
ref = https://awoiaf.westeros.org/index.php/Daenys_Targaryen

?- daughter(X,daenys).
X = elaena
ref = https://awoiaf.westeros.org/index.php/Elaena_Targaryen_(daughter_of_Gaemon)

?- child_in_law(X,daenys).
X = aegon_gaemon
X = elaena
ref = https://awoiaf.westeros.org/index.php/House_Targaryen#Targaryen_Family_Tree

?- daughter_in_law(X,daenys).
X = elaena
ref = https://awoiaf.westeros.org/index.php/House_Targaryen#Targaryen_Family_Tree

?- mother_in_law(X,aegon_gaemon).
X = daenys
ref = https://awoiaf.westeros.org/index.php/House_Targaryen#Targaryen_Family_Tree

?- spouse(elaena,X).
X = aegon_gaemon
ref = https://awoiaf.westeros.org/index.php/Elaena_Targaryen_(daughter_of_Gaemon)

?- child(X,aegon_gaemon).
X = aerys
X = maegon
ref = https://awoiaf.westeros.org/index.php/Aegon_Targaryen_(son_of_Gaemon)

?- child(X,aerys).
X = daemion
X = baelon
X = aelyx
ref = https://awoiaf.westeros.org/index.php/Aerys_Targaryen_(son_of_Aegon)

?- grandparent(X,aelyx).
X = elaena
X = aegon_gaemon
ref = https://awoiaf.westeros.org/index.php/House_Targaryen#Targaryen_Family_Tree

?- greatgrandparent(X,aelyx).
X = daenys
X = gaemon_aenar
ref = https://awoiaf.westeros.org/index.php/House_Targaryen#Targaryen_Family_Tree

?- child(daemion,X).
X = aerys_wife
X = aerys
ref = https://awoiaf.westeros.org/index.php/House_Targaryen#Targaryen_Family_Tree

?- married(X,aelyx).
false
ref = https://awoiaf.westeros.org/index.php/House_Targaryen#Targaryen_Family_Tree

?- child(X,daemion).
X = aerion
ref = https://awoiaf.westeros.org/index.php/Daemion_Targaryen

?- wife(X,aerion).
X = valaena_velaryon
ref = https://awoiaf.westeros.org/index.php/Aerion_Targaryen_(son_of_Daemion)

?- bastard(X,aerion).
X = orys_baratheon
ref = https://awoiaf.westeros.org/index.php/Orys_Baratheon

?- child(X,aerion).
X = orys_baratheon
X = rhaenys
X = aegon_I
X = visenya
ref = https://awoiaf.westeros.org/index.php/Aerion_Targaryen_(son_of_Daemion)

?- child_in_law(X,aerion).
X = rhaenys
X = visenya
X = aegon_I
ref = https://awoiaf.westeros.org/index.php/House_Targaryen#Targaryen_Family_Tree

?- spouse(X,rhaenys).
X = aegon_I
ref = https://awoiaf.westeros.org/index.php/Rhaenys_Targaryen

?- wife(X,aegon_I).
X = rhaenys
X = visenya
ref = https://awoiaf.westeros.org/index.php/House_Targaryen#Targaryen_Family_Tree

?- born(rhaenys,X).
false
X = aenys_I
ref = https://awoiaf.westeros.org/index.php/House_Targaryen#Targaryen_Family_Tree

?- parent(rhaenys,X).
X = maegor_I
X = aenys_I
ref = https://awoiaf.westeros.org/index.php/House_Targaryen#Targaryen_Family_Tree

?- half_sibling(maegor_I,aenys_I).
true
ref = https://awoiaf.westeros.org/index.php/House_Targaryen#Targaryen_Family_Tree

?- uncle(baelon,aerion).
true
ref = https://awoiaf.westeros.org/index.php/House_Targaryen#Targaryen_Family_Tree

*/
% link : http://tau-prolog.org/sandbox/phgM37z
