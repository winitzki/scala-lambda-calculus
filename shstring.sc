// Get the list of arguments for a Unix shell command
// Usage: %!"ls -l $a*.txt"
// Variable substitution is performed, but no escaping of quotes or spaces.

def ins_sep(s: String):List[Option[String]] = {
                            val with_seps = s.split(' ').toList.filter(_.nonEmpty).foldLeft(List[Option[String]]()){
                            case (acc : List[Option[String]],p) => acc :+ None :+ Some(p) }
                            val trim1 = if (with_seps.nonEmpty) with_seps.tail else with_seps
                            val trim2 =  if (s.startsWith(" ")) List(None) ++ trim1 else trim1
                            val trim3 = if (s.endsWith(" ")) trim2 :+ None else trim2
                            trim3
                          }

def make_arg_list(parts: Seq[String], args: Seq[String]): Seq[String] = {
		val parts_and_args = parts.zipAll(args.map(_.toString), "", "")
		val list_with_separators = parts_and_args.foldLeft(Seq[Option[String]]()){ 
			case(acc,(p,a)) => acc ++ ins_sep(p) :+ Some(a)  
		}
		val (final_v, final_list) = list_with_separators.foldLeft( ("", Seq[String]()) ){ 
			case ((v,acc),Some(s)) => (v+s, acc)
            case ((v,acc), None) => ("", acc :+ v)
        }
        if (final_v.nonEmpty)
        	final_list :+ final_v
        else
        	final_list
	}

def arg_list(sc: StringContext, args: Seq[Any]): Seq[String] = make_arg_list(sc.parts.toSeq, args.map(_.toString).toSeq)

implicit class BashEscape(val sc: StringContext) extends AnyVal {
	def c(args: Any*) = %.apply(arg_list(sc, args))
	def co(args: Any*) = %%.apply(arg_list(sc, args))
}
