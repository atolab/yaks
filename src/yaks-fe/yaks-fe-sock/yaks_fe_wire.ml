[%%cstruct 
type header = 
  { mid : uint16_t
  ; flags : uint16_t
  ; corr_id : uint64_t
  ; length : uint64_t  
} [@@little_endian]]

