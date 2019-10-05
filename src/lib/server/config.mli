type call_with_cache_t =
  ( Searcher.RuleHistoryCache.Key.t
  , ProtocolMessage.Response.t )
  Searcher.FlushableCache.CallWithCache.t

type t =
  { training_benchs: string array
  ; testing_benchs: string array
  ; frontend_config: Frontend.FrontendConfig.t
  ; call_with_cache: call_with_cache_t
  ; search_config: Searcher.SearchConfig.t }

val create :
     string
  -> string
  -> Frontend.FrontendConfig.t
  -> call_with_cache_t
  -> Searcher.SearchConfig.t
  -> (t, string) Core.Result.t

val bench_of_id : t -> int -> string option
