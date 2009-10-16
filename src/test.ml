(*********************************************************************************)
(*                OCaml-R                                                        *)
(*                                                                               *)
(*    Copyright (C) 2008-2009 Institut National de Recherche en                  *)
(*    Informatique et en Automatique. All rights reserved.                       *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation; either version 3 of the         *)
(*    License, or  any later version.                                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public                  *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Testing R interface. *)

let main () =
  prerr_endline "Initialization";
  let n = R.init() in
  prerr_endline ("Returned: "^(string_of_int n));

  R.current_test ();
  for i = 0 to 30 do
    prerr_endline "defining sexp";
    let sexp = R.sexp (Printf.sprintf "0:%d" i) in
    R.r_print_value sexp;
    Gc.full_major();
    prerr_endline "binding x";
    R.set_var "x" sexp;
    Gc.full_major();
    prerr_endline "getting value of x";
    let sexp = R.sexp_of_symbol "x" in
    prerr_endline "sexp = ...";
    R.r_print_value sexp;
    Gc.full_major();
  done;

  let b = R.of_bool true in
  R.r_print_value b;
  let b = R.of_bool false in
  R.r_print_value b;
  let n = R.of_int 123456789 in
  R.r_print_value n;
  let f = R.of_float 3.101101 in
  R.r_print_value f;
  let s = R.of_string "coucou\nla\ncompagnie" in
  R.r_print_value s;


  R.set_var "vrai" (R.sexp "TRUE");
  prerr_endline (Printf.sprintf "vrai = %b" (R.to_bool (R.sexp_of_symbol "vrai")));
  R.set_var "faux" (R.sexp "FALSE");
  prerr_endline (Printf.sprintf "faux = %b" (R.to_bool (R.sexp_of_symbol "faux")));
  R.set_var "quinze" (R.of_int 15);
  prerr_endline (Printf.sprintf "quinze = %d" (R.to_int (R.sexp_of_symbol "quinze")));
  prerr_endline (Printf.sprintf "pi = %f" (R.to_float (R.sexp_of_symbol "pi")));
  prerr_endline (Printf.sprintf "foo bar = %s" (R.to_string (R.of_string "foo bar")));
  prerr_endline (Printf.sprintf "gee buz = %s" (R.to_string (R.sexp "\"gee buz\"")));

  let bool_array = R.of_bool_array (Array.init 8 (fun i -> i mod 2 = 0)) in
  R.r_print_value bool_array;
  prerr_endline (String.concat ", "
   (List.map (fun b -> Printf.sprintf "%b" b)
    (Array.to_list (R.to_bool_array bool_array))));

  let int_array = R.of_int_array (Array.init 8 (fun i -> i)) in
  R.r_print_value int_array;
  prerr_endline
    (String.concat ", "
     (List.map string_of_int
      (Array.to_list (R.to_int_array int_array))));

  let string_array = R.of_string_array
    (Array.append [| "chaine1" ; "chaine2" |] (Array.init 8 string_of_int))
  in
  R.r_print_value string_array;
  prerr_endline
    (String.concat ", "
     (Array.to_list (R.to_string_array string_array)));

  let float_array = R.of_float_array (Array.init 8 (fun i -> sqrt (float i))) in
  R.set_var "float" float_array;
  R.r_print_value float_array;
  prerr_endline
    (String.concat ", "
     (List.map string_of_float
     (Array.to_list (R.to_float_array (R.sexp_of_symbol "float")))));
  let double = R.sexp "float*2" in
  R.r_print_value double;

  prerr_endline "\n### conversions of r values to and from ocaml floats\n";
  let pi = R.sexp "pi" in
  R.r_print_value (R.of_float (R.to_float pi));
  R.r_print_value (R.of_int (R.to_int pi));
  R.r_print_value (R.of_bool (R.to_bool pi));
  R.r_print_value (R.of_string (R.to_string pi));

  R.r_print_value (R.of_float_array (R.to_float_array float_array));
  R.r_print_value (R.of_int_array (R.to_int_array float_array));
  R.r_print_value (R.of_bool_array (R.to_bool_array float_array));
  R.r_print_value (R.of_string_array (R.to_string_array float_array));

  prerr_endline "\n### conversions of r values to and from ocaml ints\n";
  let n = R.sexp "12345" in
  R.r_print_value (R.of_float (R.to_float n));
  R.r_print_value (R.of_int (R.to_int n));
  R.r_print_value (R.of_bool (R.to_bool n));
  R.r_print_value (R.of_string (R.to_string n));

  R.r_print_value (R.of_float_array (R.to_float_array int_array));
  R.r_print_value (R.of_int_array (R.to_int_array int_array));
  R.r_print_value (R.of_bool_array (R.to_bool_array int_array));
  R.r_print_value (R.of_string_array (R.to_string_array int_array));

  prerr_endline "\n### conversions of r values to and from ocaml booleans\n";
  let b = R.sexp "TRUE" in
  R.r_print_value (R.of_float (R.to_float b));
  R.r_print_value (R.of_int (R.to_int b));
  R.r_print_value (R.of_bool (R.to_bool b));
  R.r_print_value (R.of_string (R.to_string b));

  R.r_print_value (R.of_float_array (R.to_float_array bool_array));
  R.r_print_value (R.of_int_array (R.to_int_array bool_array));
  R.r_print_value (R.of_bool_array (R.to_bool_array bool_array));
  R.r_print_value (R.of_string_array (R.to_string_array bool_array));

  prerr_endline "\n### conversions of r values to and from ocaml strings\n";
  let s = R.sexp "\"la chaîne\navec deux lignes\"" in
  R.r_print_value (R.of_float (R.to_float s));
  R.r_print_value (R.of_int (R.to_int s));
  R.r_print_value (R.of_bool (R.to_bool s));
  R.r_print_value (R.of_string (R.to_string s));

  R.r_print_value (R.of_float_array (R.to_float_array string_array));
  R.r_print_value (R.of_int_array (R.to_int_array string_array));
  R.r_print_value (R.of_bool_array (R.to_bool_array string_array));
  R.r_print_value (R.of_string_array (R.to_string_array string_array));

  prerr_endline "\n### getting dims\n";
  let s = R.sexp "array(1:20, dim=c(4,5))" in
  R.r_print_value s;
  let dims = R.dim s in
  Printf.eprintf "Dimensions of s = %s\n"
    (String.concat " * " (List.map string_of_int (Array.to_list dims)));

  ignore(R.sexp "x=array(1:9,dim=c(3,3))") ;
  ignore(R.sexp "dimnames(x)<-list(c(\"a\",\"b\",\"c\"),c(\"x\",\"y\",\"z\"))");
  let x = R.sexp_of_symbol "x" in
  R.r_print_value x;
  let dimnames = R.dimnames x in
  let dims = R.dim x in
  Printf.eprintf "Dimensions of x = %s\n"
    (String.concat " * " (List.map string_of_int (Array.to_list dims)));
  Printf.eprintf "Dimnames of x = %s\n"
    (String.concat " * " (Array.to_list dimnames));

  prerr_endline "\n### Running some tests using R math library\n";
  let module M = Rmath in
  let sf s = R.to_float (R.sexp s) in
  let tests =
    [
      "pow", (fun () -> M.pow 2.0 2.0 = 4.0) ;
      "pow_di", (fun () -> M.pow_di 3.0 3 = 27.);
      "norm_rand", (fun () -> let _ = M.norm_rand () in true);
      "unif_rand", (fun () -> let _ = M.norm_rand () in true);
      "exp_rand", (fun () -> let _ = M.norm_rand () in true);

      "dnorm",
      (fun () -> let x = M.dnorm 0.95 in let y = (sf "dnorm(0.95)") in x=y);
      "pnorm",
      (fun () -> let x = M.pnorm 0.95 in let y = (sf "pnorm(0.95)") in x=y);
      "qnorm",
      (fun () -> let x = M.qnorm 0.95 in let y = (sf "qnorm(0.95)") in x=y);
      "rnorm", (fun () -> ignore(M.rnorm ~mean: 2.0 ()); true);

      "dunif",
      (fun () -> let x = M.dunif 0.95 in let y = (sf "dunif(0.95)") in x=y);
      "punif",
      (fun () -> let x = M.punif 0.95 in let y = (sf "punif(0.95)") in x=y);
      "qunif",
      (fun () -> let x = M.qunif 0.95 in let y = (sf "qunif(0.95)") in x=y);
      "runif", (fun () -> ignore(M.runif ()); true) ;

      "dgamma",
      (fun () -> let x = M.dgamma ~shape: 1.0 0.95 in let y = (sf "dgamma(0.95,shape=1.0)") in x=y);
      "pgamma",
      (fun () -> let x = M.pgamma ~shape: 1.0 0.95 in let y = (sf "pgamma(0.95,shape=1.0)") in x=y);
      "qgamma",
      (fun () -> let x = M.qgamma ~shape: 1.0 0.95 in let y = (sf "qgamma(0.95, shape=1.0)") in x=y);
      "rgamma", (fun () -> ignore(M.rgamma 0.5); true);

      "dbeta",
      (fun () -> let x = M.dbeta ~a: 1.0 ~b: 0.5 0.95 in let y = (sf "dbeta(0.95, shape1=1.0, shape2=0.5)") in x=y);
      "dnbeta",
      (fun () -> let x = M.dbeta ~a: 1.0 ~b: 0.5 ~ncp: 4.0 0.95 in let y = (sf "dbeta(0.95, shape1=1.0, shape2=0.5, ncp=4.0)") in x=y);
      "pbeta",
      (fun () -> let x = M.pbeta ~a: 1.0 ~b: 0.5 0.95 in let y = (sf "pbeta(0.95, shape1=1.0, shape2=0.5)") in x=y);
      "pnbeta",
      (fun () -> let x = M.pbeta ~a: 1.0 ~b: 0.5 ~ncp: 4.0 0.95 in let y = (sf "pbeta(0.95, shape1=1.0, shape2=0.5, ncp=4.0)") in x=y);
      "qbeta",
      (fun () -> let x = M.qbeta ~a: 1.0 ~b: 0.5 0.95 in let y = (sf "qbeta(0.95, shape1=1.0, shape2=0.5)") in x=y);
      "qnbeta",
      (fun () -> let x = M.qbeta ~a: 1.0 ~b: 0.5 ~ncp: 4.0 0.95 in let y = (sf "qbeta(0.95, shape1=1.0, shape2=0.5, ncp=4.0)") in x=y);
      "rbeta", (fun () -> ignore(M.rbeta  ~a: 1.0 ~b: 0.5); true) ;

      "dlnorm",
      (fun () -> let x = M.dlnorm 0.95 in let y = (sf "dlnorm(0.95)") in x=y);
      "plnorm",
      (fun () -> let x = M.plnorm 0.95 in let y = (sf "plnorm(0.95)") in x=y);
      "qlnorm",
      (fun () -> let x = M.qlnorm 0.95 in let y = (sf "qlnorm(0.95)") in x=y);
      "rlnorm", (fun () -> ignore(M.rlnorm ()); true);

      "dchisq",
      (fun () -> let x = M.dchisq ~df: 0.5 0.5 in let y = (sf "dchisq(0.5, df=0.5)") in x=y);
      "dnchisq",
      (fun () -> let x = M.dchisq ~ncp: 3.0 ~df: 0.5 0.5 in let y = (sf "dchisq(0.5, df=0.5, ncp=3.0)") in x=y);
      "pchisq",
      (fun () -> let x = M.pchisq ~df: 0.5 0.5 in let y = (sf "pchisq(0.5, df=0.5)") in x=y);
      "pnchisq",
      (fun () -> let x = M.pchisq ~ncp: 3.0 ~df: 0.5 0.5 in let y = (sf "pchisq(0.5, df=0.5, ncp=3.0)") in x=y);
      "qchisq",
      (fun () -> let x = M.qchisq ~df: 1.0 0.999 in let y = (sf "qchisq(0.999, df=1)") in prerr_endline (Printf.sprintf "qchisq=%f" x); x=y);
      "qnchisq",
      (fun () -> let x = M.qchisq ~ncp: 3.0 ~df: 0.5 0.5 in let y = (sf "qchisq(0.5, df=0.5, ncp=3.0)") in x=y);
      "rchisq",
      (fun () -> let _x = M.rchisq ~ncp: 0.0 0.5 in true) ;

      "df",
      (fun () -> let x = M.df ~df1: 0.5 ~df2: 0.75 0.5 in let y = (sf "df(0.5, df1=0.5, df2=0.75)") in x=y);
      "dnf",
      (fun () -> let x = M.df ~ncp: 3.0 ~df1: 0.5 ~df2: 0.75 0.5 in let y = (sf "df(0.5, df1=0.5, df2=0.75, ncp=3.0)") in x=y);
(*
 the call to M.pf makes a segfault:
      "pf",
      (fun () -> let x = M.pf ~df1: 0.5 ~df2: 0.75 0.5 in let y = (sf "pf(0.5, df1=0.5, df2=0.75)") in x=y);
*)
      "pnf",
      (fun () -> let x = M.pf ~ncp: 3.0 ~df1: 0.5 ~df2: 0.75 0.5 in let y = (sf "pf(0.5, df1=0.5, df2=0.75, ncp=3.0)") in x=y);
      "qf",
      (fun () -> let x = M.qf ~df1: 0.5 ~df2: 0.75 0.5 in let y = (sf "qf(0.5, df1=0.5, df2=0.75)") in x=y);
      "qnf",
      (fun () -> let x = M.qf ~ncp: 3.0 ~df1: 0.5 ~df2: 0.75 0.5 in let y = (sf "qf(0.5, df1=0.5, df2=0.75, ncp=3.0)") in x=y);
      "rf",
      (fun () -> let _x = M.rf ~df1: 0.5 ~df2: 0.75 in true) ;

      "dt",
      (fun () -> let x = M.dt ~df: 2.0 0.5 in let y = (sf "dt(0.5, df=2)") in x=y);
      "dnt",
      (fun () -> let x = M.dt ~ncp: 3.0 ~df: 2.0 0.5 in let y = (sf "dt(0.5, df=2, ncp=3.0)") in x=y);
      "pt",
      (fun () -> let x = M.pt ~df: 2.0 0.5 in let y = (sf "pt(0.5, df=2)") in x=y);
      "pnt",
      (fun () -> let x = M.pt ~ncp: 3.0 ~df: 2.0 0.5 in let y = (sf "pt(0.5, df=2, ncp=3.0)") in x=y);
      "qt",
      (fun () -> let x = M.qt ~df: 2.0 0.75 in let y = (sf "qt(0.75, df=2)") in x=y);
      "qnt",
      (fun () -> let x = M.qt ~ncp: 3.0 ~df: 2.0 0.5 in let y = (sf "qt(0.5, df=2, ncp=3.0)") in x=y);
      "rt",
      (fun () -> let _x = M.rt 0.5 in true) ;

      "dbinom",
      (fun () -> let x = M.dbinom ~size: 12.0 ~prob: 0.49 0.95 in let y = (sf "dbinom(0.95, size=12, prob=0.49)") in x=y);
      "pbinom",
      (fun () -> let x = M.pbinom ~size: 12.0 ~prob: 0.49 0.95 in let y = (sf "pbinom(0.95, size=12, prob=0.49)") in x=y);
      "qbinom",
      (fun () -> let x = M.qbinom ~size: 12.0 ~prob: 0.49 0.95 in let y = (sf "qbinom(0.95, size=12, prob=0.49)") in x=y);
      "rbinom", (fun () -> ignore(M.rbinom ~size: 12.0 ~prob: 0.49); true);

      "dcauchy",
      (fun () -> let x = M.dcauchy 0.95 in let y = (sf "dcauchy(0.95)") in x=y);
      "pcauchy",
      (fun () -> let x = M.pcauchy 0.95 in let y = (sf "pcauchy(0.95)") in x=y);
      "qcauchy",
      (fun () -> let x = M.qcauchy 0.95 in let y = (sf "qcauchy(0.95)") in x=y);
      "rcauchy", (fun () -> ignore(M.rcauchy ()); true);

      "dexp",
      (fun () -> let x = M.dexp 0.95 in let y = (sf "dexp(0.95)") in x=y);
      "pexp",
      (fun () -> let x = M.pexp 0.95 in let y = (sf "pexp(0.95)") in x=y);
      "qexp",
      (fun () -> let x = M.qexp 0.95 in let y = (sf "qexp(0.95)") in x=y);
      "rexp", (fun () -> ignore(M.rexp ()); true);

      "dgeom",
      (fun () -> let x = M.dgeom ~prob: 0.5 0.5 in let y = (sf "dgeom(0.5, prob=0.5)") in x=y);
      "pgeom",
      (fun () -> let x = M.pgeom ~prob: 0.5 0.5 in let y = (sf "pgeom(0.5, prob=0.5)") in x=y);
      "qgeom",
      (fun () -> let x = M.qgeom ~prob: 0.15 0.75 in let y = (sf "qgeom(0.75, prob=0.15)") in x=y);
(* C function rgeom loops
      "rgeom",
      (fun () -> let _x = M.rgeom  0.5 in true) ;
*)

      "dhyper",
      (fun () -> let x = M.dhyper ~m: 5.0 ~n: 6.0 ~k: 2.0 0.5 in let y = (sf "dhyper(0.5, m=5, n=6, k=2)") in x=y);
      "phyper",
      (fun () -> let x = M.phyper ~m: 5.0 ~n: 6.0 ~k: 2.0 0.5 in let y = (sf "phyper(0.5, m=5, n=6, k=2)") in x=y);
      "qhyper",
      (fun () -> let x = M.qhyper ~m: 5.0 ~n: 6.0 ~k: 2.0 0.75 in let y = (sf "qhyper(0.75, m=5, n=6, k=2)") in x=y);
      "rhyper",
      (fun () -> let _x = M.rhyper ~m: 5.0 ~n: 6.0 ~k: 2.0 in true) ;

      "dnbinom",
      (fun () -> let x = M.dnbinom ~size: 12.0 ~prob: 0.49 0.95 in let y = (sf "dnbinom(0.95, size=12, prob=0.49)") in x=y);
      "pnbinom",
      (fun () -> let x = M.pnbinom ~size: 12.0 ~prob: 0.49 0.95 in let y = (sf "pnbinom(0.95, size=12, prob=0.49)") in x=y);
      "qnbinom",
      (fun () -> let x = M.qnbinom ~size: 12.0 ~prob: 0.49 0.95 in let y = (sf "qnbinom(0.95, size=12, prob=0.49)") in x=y);
      (* C function loops :
      "rnbinom", (fun () -> ignore(M.rnbinom ~size: 12.0 ~prob: 0.49); true);
      *)

      "dpois",
      (fun () -> let x = M.dpois ~lambda: 3.5 2.0 in let y = (sf "dpois(2.0, lambda=3.5)") in x=y);
      "ppois",
      (fun () -> let x = M.ppois ~lambda: 3.5 0.95 in let y = (sf "ppois(0.95, lambda=3.5)") in x=y);
      "qpois",
      (fun () -> let x = M.qpois ~lambda: 3.5 0.95 in let y = (sf "qpois(0.95, lambda=3.5)") in x=y);
      "rpois", (fun () -> ignore(M.rpois 3.5); true);

      "dweibull",
      (fun () -> let x = M.dweibull ~shape: 1.0 0.95 in let y = (sf "dweibull(0.95,shape=1.0)") in x=y);
      "pweibull",
      (fun () -> let x = M.pweibull ~shape: 1.0 0.95 in let y = (sf "pweibull(0.95,shape=1.0)") in x=y);
      "qweibull",
      (fun () -> let x = M.qweibull ~shape: 1.0 0.95 in let y = (sf "qweibull(0.95, shape=1.0)") in x=y);
      "rweibull", (fun () -> ignore(M.rweibull 0.5); true);

      "dlogis",
      (fun () -> let x = M.dlogis 0.95 in let y = (sf "dlogis(0.95)") in x=y);
      "plogis",
      (fun () -> let x = M.plogis 0.95 in let y = (sf "plogis(0.95)") in x=y);
      "qlogis",
      (fun () -> let x = M.qlogis 0.95 in let y = (sf "qlogis(0.95)") in x=y);
      "rlogis", (fun () -> ignore(M.rlogis ()); true);

(* FIXME: ptukey and qtuckey always return NaN *)
      "ptukey",
      (fun () -> let x = M.ptukey ~nmeans:6.0 ~df: 5.0 0.95 in let y = (sf "ptukey(0.95, nm=6, df=5)") in print_float x; flush(stdout);x=y);
      "qtukey",
       (fun () -> let x = M.qtukey ~nmeans:6.0 ~df: 5.0 0.95 in let y = (sf "qtukey(0.95, nm=6, df=5)") in print_float x; flush(stdout);x=y);

      "dwilcox",
      (fun () -> let x = M.dwilcox ~m: 5.0 ~n: 6.0 0.5 in let y = (sf "dwilcox(0.5, m=5, n=6)") in x=y);
      "pwilcox",
      (fun () -> let x = M.pwilcox ~m: 5.0 ~n: 6.0 0.5 in let y = (sf "pwilcox(0.5, m=5, n=6)") in x=y);
      "qwilcox",
      (fun () -> let x = M.qwilcox ~m: 5.0 ~n: 6.0 0.75 in let y = (sf "qwilcox(0.75, m=5, n=6)") in x=y);
      "rwilcox",
      (fun () -> let _x = M.rwilcox ~m: 5.0 ~n: 6.0 in true) ;

      "dsignrank",
      (fun () -> let x = M.dsignrank ~n: 3.5 2.0 in let y = (sf "dsignrank(2.0, n=3.5)") in x=y);
      "psignrank",
      (fun () -> let x = M.psignrank ~n: 3.5 0.95 in let y = (sf "psignrank(0.95, n=3.5)") in x=y);
      "qsignrank",
      (fun () -> let x = M.qsignrank ~n: 3.5 0.95 in let y = (sf "qsignrank(0.95, n=3.5)") in x=y);
      "rsignrank", (fun () -> ignore(M.rsignrank 3.5); true);

      "gammafn",
      (fun () -> let x = M.gammafn 10.0 in let y = sf "gamma(10.0)" in x=y);
      "lgammafn",
      (fun () -> let x = M.lgammafn 10.0 in let y = sf "lgamma(10.0)" in x=y);
      "psigamma",
      (fun () -> let x = M.psigamma ~deriv:1.0 10.0 in let y = sf "psigamma(10.0,deriv=1.0)" in x=y);

      "digamma",
      (fun () -> let x = M.digamma 10.0 in let y = sf "digamma(10.0)" in x=y);
      "trigamma",
      (fun () -> let x = M.trigamma 10.0 in let y = sf "trigamma(10.0)" in x=y);

      "bessel_i",
      (fun () -> let x = M.bessel_i ~nu: 10.0 4.0 in let y = sf "besselI(x=4.0, nu=10.0)" in
         Printf.printf "x(from C)=%e, y(from R)=%e\n" x y;
         flush(stdout);x=y);
      "bessel_k",
      (fun () -> let x = M.bessel_k ~nu: 10.0 4.0 in let y = sf "besselK(x=4.0, nu=10.0)" in
         Printf.printf "x(from C)=%e, y(from R)=%e\n" x y;
         flush(stdout);x=y);
      "bessel_j",
      (fun () -> let x = M.bessel_j ~nu: 10.0 ~x: 4.0 in let y = sf "besselJ(x=4.0, nu=10.0)" in
         Printf.printf "x(from C)=%e, y(from R)=%e\n" x y;
         flush(stdout);x=y);
      "bessel_y",
      (fun () -> let x = M.bessel_y ~nu: 10.0 ~x: 4.0 in let y = sf "besselY(x=4.0, nu=10.0)" in
         Printf.printf "x(from C)=%e, y(from R)=%e\n" x y;
         flush(stdout);x=y);

       "sign",
       (fun () ->
         let (x1,x2,x3) = (M.sign 11.17, -12.34, 0.0) in
         let (y1,y2,y3) =
           match R.to_float_array (R.sexp "sign(c(11.17,-12.34,0.0))") with
             [| y1 ; y2 ; y3 |] -> (y1,y2,y3)
           | _ -> assert false
         in
         Printf.printf "x1=%f, x2=%f, x3=%f\ny1=%f, y2=%f, y3=%f\n" x1 x2 x3 y1 y2 y3;
         flush(stdout);
         x1 = y1 && x2 = y2 && x3 = y3
      );

      "fround",
      (fun () ->
         let x = M.fround ~digits: 1 11.17 in
         let y = sf "round(11.17,digits=1)" in
(*         Printf.printf "x(from C)=%e, y(from R)=%e\n" x y;flush stdout;*)
         x = y
      );

      "fsign",
      (fun () ->
         let x = M.fsign ~digits: 2 3.14156 in
         let y = sf "signif(3.14156,digits=2)" in
         Printf.printf "x(from C)=%e, y(from R)=%e\n" x y;flush stdout;
         x = y
      );

      "ftrunc",
      (fun () ->
         let x = M.ftrunc 3.14156 in
         let y = sf "trunc(3.14156)" in
         x = y
      );

    ]
  in
  let test (name, f) =
    if f () then
      print_endline (Printf.sprintf "test \"%s\" OK" name)
    else
      prerr_endline (Printf.sprintf "ERROR: test \"%s\" failed" name)
  in
  List.iter test tests;

  print_newline();

  prerr_endline "### Trying plots and error handling";

  R.exec "plot" [|
   `Anon (R.sexp "1:10") ;
   `Anon (R.sexp "11:20") ;
   `Named ("col", R.sexp "3") ;
   `Named ("ty", R.sexp "\"l\"") ;
  |] ;

  (* create an array of int from 1 to 20, using R *)
  let r_t = R.sexp "1:20" in
  (* create a simple function *)
  let f x = x + 1 in
  (* copy the R array to an OCaml int array and apply (Array.map f) on it *)
  let u = Array.map f (R.to_int_array r_t) in
  (* get a R expression from the resulting OCaml int array *)
  let r_u = R.of_int_array u in
  R.r_print_value r_u;
(*  Unix.sleep 5;*)
  prerr_endline "Ending";
  R.terminate()
  ;;

main ();;
