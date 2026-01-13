let List/map = https://prelude.dhall-lang.org/v15.0.0/List/map


let DATE = { mon : Integer, day: Integer, year : Integer }
let TIME = { hrs : Integer, min: Integer, sec : Integer }


let version = +2 : Integer


let PROC =
    < TODO
    | DOING
    | DONE
    | WAIT
    | CANCELED
    | NOW
    | LATER
    | LOCKED
    >


let LevelD =
    { maximum : Double
    , name : Text
    , date : Optional DATE
    }


let LevelI =
    { maximum : Integer
    , name : Text
    , date : Optional DATE
    }


let LevelIO =
    { maximum : Optional Integer
    , name : Text
    , date : Optional DATE
    }


let LevelS =
    { gives : Text
    , date : Optional DATE
    }


let LevelP =
    { name : Text
    , proc : PROC
    , date : Optional DATE
    }


let PI  = { total : Integer, done : Integer }
let PD  = { total : Double , done : Double  }
let MESI = { i : Integer, measure : Text }
let MESD = { d : Double, measure : Text }
let MESX = { d : Double, measure : Text, sign : Integer }
let PERI = { i : Integer, per : Text }
let PERD = { d : Double, per : Text }
let RNGI = { from : Integer, to : Integer }
let RNGD = { from : Double, to : Double }
let LVLD = { reached : Double,  levels : List LevelD }
let LVLI = { reached : Integer, levels : List LevelI }
let LVLIO = { reached : Integer, levels : List LevelIO }
let LVLS = { reached : Integer, levels : List LevelS }
let LVLC = { reached : Integer, levels : Integer, current : Integer, maxcurrent : Integer }
let LVLP = { levels : List LevelP }
let PCTX = { sign : Integer, pct : Double }


let Relation =
    \(t : Type) ->
    < MoreThan : t
    | LessThan : t
    | Exact : t
    >


let Value =
    < E
    | UNK
    | T : Text
    | N : Natural
    | I : Integer
    | F : Double
    | D : Bool
    | PI : PI -- total/done : int
    | PD : PD -- total/done : double
    | PCT : Double -- percents (0..1)
    | PCTI : Integer  -- percents (0..100)
    | PCTX : PCTX -- percents with sign
    | MESI : MESI -- int / measure
    | MESD : MESD -- double / measure
    | MESX : MESX -- double / measure with sign
    | PERI : PERI -- int / per
    | PERD : PERD -- double / pre
    | DATE : DATE -- { mon + day + year }
    | TIME : TIME -- { hrs + min + sec }
    | RNGI : RNGI -- from / to : int
    | RNGD : RNGD -- from / to : double
    | LVLD : LVLD -- levels : double
    | LVLI : LVLI -- levels : int
    | LVLS : LVLS -- levels : text
    | LVLC : LVLC -- levels : only current
    | LVLP : LVLP -- levels : total / done
    | LVLIO : LVLIO -- levels : optional int -- TODO: "== only current" sometimes?
    | PROC : PROC
    | RELN : Relation Natural
    | RELI : Relation Integer
    | RELD : Relation Double
    | RELT : Relation TIME
    >


let TaggedValue =
    { v : Value, t : Text }


let tag
    : Value -> TaggedValue
    = \(v : Value)
    -> merge
        { E = { t = "E", v = v }
        , UNK = { t = "UNK", v = v }
        , T = \(x : Text) -> { t = "T", v = v }
        , I = \(x : Integer) -> { t = "I", v = v }
        , N = \(x : Natural) -> { t = "N", v = v }
        , F = \(x : Double) -> { t = "F", v = v }
        , D = \(x : Bool) -> { t = "D", v = v }
        , PI = \(x : PI) -> { t = "PI", v = v }
        , PD = \(x : PD) -> { t = "PD", v = v }
        , PCT = \(x : Double) -> { t = "PCT", v = v }
        , PCTI = \(x : Integer) -> { t = "PCTI", v = v }
        , PCTX = \(x : PCTX) -> { t = "PCTX", v = v }
        , MESI = \(x : MESI) -> { t = "MESI", v = v }
        , MESD = \(x : MESD) -> { t = "MESD", v = v }
        , MESX = \(x : MESX) -> { t = "MESX", v = v }
        , PERI = \(x : PERI) -> { t = "PERI", v = v }
        , PERD = \(x : PERD) -> { t = "PERD", v = v }
        , TIME = \(x : TIME) -> { t = "TIME", v = v }
        , DATE = \(x : DATE) -> { t = "DATE", v = v }
        , RNGI = \(x : RNGI) -> { t = "RNGI", v = v }
        , RNGD = \(x : RNGD) -> { t = "RNGD", v = v }
        , LVLD = \(x : LVLD) -> { t = "LVLD", v = v }
        , LVLI = \(x : LVLI) -> { t = "LVLI", v = v }
        , LVLS = \(x : LVLS) -> { t = "LVLS", v = v }
        , LVLC = \(x : LVLC) -> { t = "LVLC", v = v }
        , LVLP = \(x : LVLP) -> { t = "LVLP", v = v }
        , LVLIO = \(x : LVLIO) -> { t = "LVLIO", v = v }
        , PROC = \(x : PROC) -> { t = "PROC", v = v }
        , RELI = \(x : Relation Integer) -> { t = "RELI", v = v }
        , RELN = \(x : Relation Natural) -> { t = "RELN", v = v }
        , RELD = \(x : Relation Double) -> { t = "RELD", v = v }
        , RELT = \(x : Relation TIME) -> { t = "RELT", v = v }
        }
        v


-- let EncValue =
--     { v : Text
--     , tag : Text
--     }

let Ref =
    < Ref : List Text
    >


let TabularValue
    = TaggedValue


let TabularKVR =
    { key : Text
    , value : TabularValue
    }


let Tabular = List TabularKVR


let Tabular/empty : Tabular
    = ([] : List TabularKVR)


-- let KV = { key : Text, value : Value }
-- let KVD = { key : Text, detailed : Text, value : Value }


let Group/Kind =
    < Transparent -- either statistics or collection
    | Statistics -- don't count total earned
    | Collection -- count total earned
    >


let Stat =
    < EarnedBy : { i : Integer }
    | EarnedByPct : { pct : Double }
    >


let GroupRec =
    { title : Text
    , ref : Ref
    , kind : Group/Kind
    }
let KeyValRec =
    { ref : Ref
    , key : Text
    , detailed : Optional Text
    , value : TaggedValue
    , selfRef : Optional Ref
    , date : Optional DATE
    , stat : Optional Stat
    , count : Optional Integer
    , tags : Optional (List Text)
    , tabular : List TabularKVR
    }
let KVR = KeyValRec


let Property =
    < Group : GroupRec
    | KeyVal : KeyValRec
    >


let Properties =
    { props : List Property
    }


let Group/Empty = List KeyValRec


let KVR/init
    : Text -> Value -> KeyValRec
    = \(key : Text) -> \(value : Value) ->
        { key = key
        , detailed = None Text
        , value = tag value
        , selfRef = None Ref
        , date = None DATE
        , ref = Ref.Ref ([] : List Text)
        , stat = None Stat
        , count = None Integer
        , tags = None (List Text)
        , tabular = Tabular/empty
        }


let KVR/setRef
    : List Text -> KeyValRec -> KeyValRec
    = \(ref : List Text) -> \(kvr : KeyValRec) ->
        kvr // { ref = Ref.Ref ref }


let KVR/setSelfRef
    : List Text -> KeyValRec -> KeyValRec
    = \(selfRef : List Text) -> \(kvr : KeyValRec) ->
        kvr // { selfRef = Some (Ref.Ref selfRef) }


let KVR/addDetails
    : Text -> KeyValRec -> KeyValRec
    = \(details : Text) -> \(kvr : KeyValRec) ->
        kvr // { detailed = Some details }


let KVR/setDate
    : DATE -> KeyValRec -> KeyValRec
    = \(date : DATE) -> \(kvr : KeyValRec) ->
        kvr // { date = Some date }


let inj/date
    : DATE -> { date : Optional DATE }
    = \(date : DATE) -> { date = Some date }


let inj/no_date
    : { date : Optional DATE }
    = { date = None DATE }


let inj/det
    : Text -> { detailed : Optional Text }
    = \(details : Text) -> { detailed = Some details }


let inj/stat_i
    : Integer -> { stat : Optional Stat }
    = \(i : Integer) -> { stat = Some (Stat.EarnedBy { i = i }) }


let inj/stat_pct
    : Double -> { stat : Optional Stat }
    = \(pct : Double) -> { stat = Some (Stat.EarnedByPct { pct = pct }) }


let inj/count
    : Integer -> { count : Optional Integer }
    = \(cnt : Integer) -> { count = Some cnt }


let inj/self
    : List Text -> { selfRef : Optional Ref }
    = \(selfRef : List Text) -> { selfRef = Some (Ref.Ref selfRef) }


let inj/tags
    : List Text -> { tags : Optional (List Text) }
    = \(tags : List Text) -> { tags = Some tags }


let inj/tag
    : Text -> { tags : Optional (List Text) }
    = \(tag : Text) -> inj/tags [ tag ]


let inj/tabs
    : List TabularKVR -> { tabular : List TabularKVR }
    = \(tabs : List TabularKVR) -> { tabular = tabs }


let inj/tab
    : Text -> Value -> { tabular : List TabularKVR }
    = \(key : Text) -> \(value : Value) -> inj/tabs [ { key, value = tag value } ]


let kv_
    : Text -> Value -> KeyValRec
    = KVR/init


let kv
    : Text -> Value -> Property
    = \(key: Text) -> \(val : Value) -> Property.KeyVal (kv_ key val)


let kvd_
    : Text -> Text -> Value -> KeyValRec
    = \(key: Text) -> \(detailed: Text) -> \(val : Value) -> KVR/addDetails detailed (kv_ key val)


let kv_det
    : Text -> KeyValRec -> KeyValRec
    = KVR/addDetails


let kv_self
    : List Text -> KeyValRec -> KeyValRec
    = KVR/setSelfRef


let kv_date
    : DATE -> KeyValRec -> KeyValRec
    = KVR/setDate


let kvd_secret
    : KeyValRec
    = kvd_ "Secret" "Secret" Value.E


let groupK
    : Group/Kind -> Text -> List Text -> List KeyValRec -> List Property
    = \(kind : Group/Kind) -> \(title : Text) -> \(path : List Text) -> \(props : List KeyValRec) ->
    [ Property.Group { title = title, ref = Ref.Ref path, kind = kind } ]
        #
        (List/map KeyValRec Property Property.KeyVal
            (List/map KeyValRec KeyValRec (KVR/setRef path) props)
        )

let groupK_
    : Group/Kind -> Text -> List Text -> List Property
    = \(kind : Group/Kind) -> \(title : Text) -> \(path : List Text) ->
    groupK kind title path ([] : List KeyValRec)


let group
    : Text -> List Text -> List KeyValRec -> List Property
    = groupK Group/Kind.Transparent


let group_
    : Text -> List Text -> List Property
    = \(title : Text) -> \(path : List Text) ->
    group title path ([] : List KeyValRec)


let groupStats
    : Text -> List Text -> List KeyValRec -> List Property
    = groupK Group/Kind.Statistics


let groupStats_
    : Text -> List Text -> List Property
    = groupK_ Group/Kind.Statistics


let groupColl
    : Text -> List Text -> List KeyValRec -> List Property
    = groupK Group/Kind.Collection


let groupColl_
    : Text -> List Text -> List Property
    = groupK_ Group/Kind.Collection


let v_t : Text    -> Value = Value.T
let v_i : Integer -> Value = Value.I
let v_n : Natural -> Value = Value.N
let v_f : Double  -> Value = Value.F
let v_done : Value = Value.D True
let v_none : Value = Value.D False
let v_done_ : Value = Value.PI { done = +1, total = +1 }
let v_none_ : Value = Value.PI { done = +0, total = +1 }
let v_vone_ : Value = Value.PI { done = +0, total = +0 }
let v_pi : PI     -> Value = Value.PI
let v_pd : PD     -> Value = Value.PD
let v_pct : Double -> Value = Value.PCT
let v_pct_done : Value = Value.PCT 1.00
let v_pct_none : Value = Value.PCT 0.00
let v_pcti : Integer -> Value = Value.PCTI
let v_pcti_done : Value = Value.PCTI +100
let v_pcti_none : Value = Value.PCTI +0
let v_proc : PROC -> Value = Value.PROC
let v_rng : RNGI  -> Value = Value.RNGI
let v_rngd : RNGD  -> Value = Value.RNGD
let v_lvld : LVLD -> Value = Value.LVLD
let v_lvli : LVLI -> Value = Value.LVLI
let v_lvls : LVLS -> Value = Value.LVLS
let v_lvlc : LVLC -> Value = Value.LVLC
let v_lvlp : LVLP -> Value = Value.LVLP
let v_time : TIME -> Value = Value.TIME
let v_date : DATE -> Value = Value.DATE
let v_empty : Value = Value.E
let v_lvlio : LVLIO -> Value = Value.LVLIO
let v_pctx : Integer -> Double -> Value = \(sign : Integer) -> \(pct : Double) -> Value.PCTX { pct = pct, sign = sign }
let v_mes  : Integer -> Text   -> Value = \(i : Integer)    -> \(text : Text)  -> Value.MESI { i = i, measure = text }
let v_mesd : Double -> Text    -> Value = \(d : Double)     -> \(text : Text)  -> Value.MESD { d = d, measure = text }
let v_per  : Integer -> Text   -> Value = \(i : Integer)    -> \(text : Text)  -> Value.PERI { i = i, per = text }
let v_perd : Double -> Text    -> Value = \(d : Double)     -> \(text : Text)  -> Value.PERD { d = d, per = text }
let v_pct_mes : Integer -> Value = \(i : Integer) -> v_mes i "%"
let v_pct_mesd : Double -> Value = \(d : Double) -> v_mesd d "%"
let v_mesx : Integer -> Double -> Text -> Value = \(sign : Integer) -> \(d : Double) -> \(text : Text)  -> Value.MESX { d = d, measure = text, sign = sign }
let v_pct_mesx : Integer -> Double -> Value = \(sign : Integer) -> \(d : Double) -> v_mesx sign d "%"
let v_reln : Relation Natural -> Value = Value.RELN
let v_reli : Relation Integer -> Value = Value.RELI
let v_reld : Relation Double -> Value = Value.RELD
let v_relt : Relation TIME -> Value = Value.RELT
let v_unk : Value = Value.UNK


let kv_unk : KeyValRec = kv_ "???" Value.UNK


let rel_more_than
    = \(a : Type) -> \(v : a) -> (Relation a).MoreThan v
let rel_less_than
    = \(a : Type) -> \(v : a) -> (Relation a).LessThan v
let rel_exact
    = \(a : Type) -> \(v : a) -> (Relation a).Exact v

-- let time_more_than : TIME -> Relation TIME = rl_more_than TIME


let p_todo     : Value = Value.PROC PROC.TODO
let p_doing    : Value = Value.PROC PROC.DOING
let p_done     : Value = Value.PROC PROC.DONE
let p_wait     : Value = Value.PROC PROC.WAIT
let p_now      : Value = Value.PROC PROC.NOW
let p_later    : Value = Value.PROC PROC.LATER
let p_canceled : Value = Value.PROC PROC.CANCELED
let p_locked   : Value = Value.PROC PROC.LOCKED


let p_todo_     : PROC = PROC.TODO
let p_doing_    : PROC = PROC.DOING
let p_done_     : PROC = PROC.DONE
let p_wait_     : PROC = PROC.WAIT
let p_now_      : PROC = PROC.NOW
let p_later_    : PROC = PROC.LATER
let p_canceled_ : PROC = PROC.CANCELED
let p_locked_   : PROC = PROC.LOCKED


let Subject =
    { id : Text
    , name : Text
    , properties : List Property
    , tabular : List TabularKVR
    , tags : List Text
    }


let QSubject =
    { id : Text
    , name : Text
    }


let collapse
    : QSubject -> List Property -> Subject
    = \(subject : QSubject) -> \(properties : List Property) ->
        { id = subject.id
        , name = subject.name
        , properties = properties
        , tabular = ([] : List TabularKVR)
        , tags = ([] : List Text)
        }


let introduce
    : QSubject -> Subject
    = \(subject : QSubject) ->
        { id = subject.id
        , name = subject.name
        , properties = ([] : List Property)
        , tabular = ([] : List TabularKVR)
        , tags = ([] : List Text)
        }


in
    { version
    , Value, Ref, Property, Properties, KVR, DATE, TIME, Relation, Group/Empty, Group/Kind, TabularValue, TabularKVR, Tabular, Tabular/empty, Subject, QSubject
    , kv, kv_, kvd_, group, group_, groupK, groupK_, groupStats, groupStats_, groupColl, groupColl_, kv_unk, kvd_secret, kv_det, kv_self, kv_date, tag
    , v_t, v_i, v_n, v_f
    , v_time, v_date
    , v_done, v_none, v_done_, v_none_, v_vone_, v_proc
    , v_rng, v_rngd
    , v_pi, v_pd
    , v_pct, v_pct_done, v_pct_none, v_pcti, v_pcti_done, v_pcti_none, v_pctx
    , v_lvld, v_lvli, v_lvls, v_lvlc, v_lvlp, v_lvlio
    , v_mes, v_mesd, v_pct_mes, v_pct_mesd, v_mesx, v_pct_mesx
    , v_per, v_perd
    , v_reli, v_reld, v_reln, v_relt
    , v_empty, v_unk
    , inj/date, inj/no_date, inj/det, inj/self, inj/stat_i, inj/stat_pct, inj/count, inj/tag, inj/tags, inj/tab, inj/tabs
    , collapse, introduce
    , p_todo, p_doing, p_done, p_now, p_later, p_canceled, p_wait, p_locked
    , p_todo_, p_doing_, p_done_, p_now_, p_later_, p_canceled_, p_wait_, p_locked_
    , rel_more_than, rel_less_than, rel_exact
    }
