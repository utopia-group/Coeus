val error_code : int

val error_info : Cmdliner.Term.exit_info

type cache_t =
  Server.ProtocolMessage.Response.t Searcher.RuleHistoryCache.KeepAll.t

val create_cache :
  string option -> (string option * cache_t, string) Core.Result.t

val cmd : CommandUtil.Command.t
