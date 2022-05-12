from ast import Constant
from inspect import isfunction
from re import S
from types import new_class
from xmlrpc.client import boolean
from prolog_structures import Rule, RuleBody, Term, Function, Variable, Atom, Number
from typing import List
from functools import reduce

import sys
import random

class Not_unifiable(Exception):
	pass

'''
Please read prolog_structures.py for data structures
that represent Prolog terms, rules, and goals.
'''
class Interpreter:
	def __init__(self):
		pass

	'''
	Example
	occurs_check (v, t) where v is of type Variable, t is of type Term.
	occurs_check (v, t) returns true if the Prolog Variable v occurs in t.
	Please see the lecture note Control in Prolog to revisit the concept of
	occurs-check.
	'''
	def occurs_check (self, v : Variable, t : Term) -> bool:
		if isinstance(t, Variable):
			return v == t
		elif isinstance(t, Function):
			for t in t.terms:
				if self.occurs_check(v, t):
					return True
			return False
		return False


	'''
	Problem 1
	variables_of_term (t) where t is of type Term.
	variables_of_clause (c) where c is of type Rule.

	The function should return the Variables contained in a term or a rule
	using Python set.

	The result must be saved in a Python set. The type of each element (a Prolog Variable)
	in the set is Variable.
	'''
	def variables_of_term (self, t : Term) -> set :
		x = set()

		if isinstance(t,Variable):
			return set([t])
		elif isinstance(t,Function):
			for e in t.terms:
				if isinstance(e,Variable):
					x.add(e) 
				elif isinstance(e,Function):
					x=x.union(self.variables_of_term(e))

		return x

	def variables_of_clause (self, c : Rule) -> set :
		x=set()
		x = x.union(self.variables_of_term(c.head))
		for e in c.body.terms:
			if isinstance(e,Variable) or isinstance(e,Function):
				x=x.union(self.variables_of_term(e))
		return x

	'''
	Problem 2
	substitute_in_term (s, t) where s is of type dictionary and t is of type Term
	substitute_in_clause (s, t) where s is of type dictionary and c is of type Rule,

	The value of type dict should be a Python dictionary whose keys are of type Variable
	and values are of type Term. It is a map from variables to terms.

	The function should return t_ obtained by applying substitution s to t.

	Please use Python dictionary to represent a subsititution map.
	'''
	def substitute_in_term (self, s : dict, t : Term) -> Term:
		if isinstance(t,Function):
			new_terms =[]
			for term in t.terms:
				if isinstance(term,Variable):
					tempTerm = self.substitute_in_term(s,term)
					new_terms.append(tempTerm)
				else:
					new_terms.append(term)
			return Function(t.relation, new_terms)
		elif isinstance(t,Variable):
			if t in s.keys():
				return s[t]
		return t

	def substitute_in_clause (self, s : dict, c : Rule) -> Rule:
		r1 = self.substitute_in_term(s,c.head)
		r2=[]
		for e in c.body.terms:
			r2.append(self.substitute_in_term(s,e))
		return Rule(r1,RuleBody(r2))



	'''
	Problem 3
	unify (t1, t2) where t1 is of type term and t2 is of type Term.
	The function should return a substitution map of type dict,
	which is a unifier of the given terms. You may find the pseudocode
	of unify in the lecture note Control in Prolog useful.

	The function should raise the exception raise Not_unfifiable (),
	if the given terms are not unifiable.

	Please use Python dictionary to represent a subsititution map.
	'''
	def unify_helper (self, t1: Term, t2: Term, s: dict) -> dict:
		
		X=self.substitute_in_term(s,t1)
		Y=self.substitute_in_term(s,t2)
		

		if isinstance(X,Variable) and not self.occurs_check(X,Y):
			s[X]= self.substitute_in_term(s,Y)
			for x in s.keys():
				s[x] = self.substitute_in_term(s,s[x])
			return s
		elif isinstance(Y,Variable) and not self.occurs_check(Y,X):
			s[Y]= self.substitute_in_term(s,X)
			for x in s.keys():
				s[x] = self.substitute_in_term(s,s[x])
			return s
		elif isinstance(X,Function) and isinstance(Y,Function):
			for i in range(len(X.terms)):
				s.update(self.unify_helper(X.terms[i],Y.terms[i],s))
		elif X == Y:
			return s
		else:
			raise Not_unifiable()
		

	def unify (self, t1: Term, t2: Term) -> dict:
		s={}
		self.unify_helper(t1,t2,s)
		return s

	fresh_counter = 0
	def fresh(self) -> Variable:
		self.fresh_counter += 1
		return Variable("_G" + str(self.fresh_counter))
	def freshen(self, c: Rule) -> Rule:
		c_vars = self.variables_of_clause(c)
		theta = {}
		for c_var in c_vars:
			theta[c_var] = self.fresh()

		return self.substitute_in_clause(theta, c)


	'''
	Problem 4
	Following the Abstract interpreter pseudocode in the lecture note Control in Prolog to implement
	a nondeterministic Prolog interpreter.

	nondet_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of Terms (results), which is an instance of the original goal and is
	a logical consequence of the program. See the tests cases (in src/main.py) as examples.
	'''

	def nondet_query (self, program : List[Rule], pgoal : List[Term]) -> List[Term]:
	
		G=pgoal
		resolvent=G.copy()

		while len(resolvent) > 0:
			APos = random.randint(0,len(resolvent)-1)
			isReplaced = False
			result ={}
			for e in program:
				renamedClause = self.freshen(e)
				try:
					result=self.unify (resolvent[APos],renamedClause.head)
				except Not_unifiable:
					continue
				
				isReplaced = True
				resolvent[APos] = e.body
				for i in range(len(resolvent)):
					resolvent[i] = self.substitute_in_term(result,resolvent[i])
						
				for i in range(len(G)):
					G[i] = self.substitute_in_term(result,G[i])

			if not isReplaced:
				resolvent.pop(APos)
		
		return G

	'''
	Challenge Problem

	det_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of term lists (results). Each of these results is
	an instance of the original goal and is a logical consequence of the program.
	If the given goal is not a logical consequence of the program, then the result
	is an empty list. See the test cases (in src/main.py) as examples.
	'''

	def det_query_dfs (self, resolvent : List[Term], goal : List[Term], solutions : List[List[Term]] , program : List[Rule]) -> boolean:
		if not resolvent:
			solutions.append(goal)
		else:
			md={}
			chosen_goal = resolvent.pop(0)
			for rule in program:
				rule =self.freshen(rule)
				try:
					md= self.unify (chosen_goal,rule.head)
				except:
					continue


				new_resolvent, new_goal=resolvent.copy(),goal.copy()
				new_resolvent.insert(0,rule.body)

				for i in range(len(new_resolvent)):
					new_resolvent[i] = self.substitute_in_term(md,new_resolvent[i])
					
				for i in range(len(new_goal)):			
					new_goal[i] = self.substitute_in_term(md,new_goal[i])
				
				
				self.det_query_dfs(new_resolvent.copy(),new_goal.copy(),solutions,program) 
		
	def det_query (self, program : List[Rule], pgoal : List[Term]) -> List[List[Term]]:
		solutions = []
		self.det_query_dfs(pgoal.copy(),pgoal.copy(),solutions,program)
		return solutions
	
