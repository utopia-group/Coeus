val rule_of_feature :
  Util.Feature.ActionFeature.t -> (Rule.t, string) Core.Result.t

val encode_action : int -> Util.Feature.ActionFeature.t

val encode_actions : int list -> Util.Feature.ActionList.t
