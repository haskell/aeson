#if __GLASGOW_HASKELL__ >= 710
#define OVERLAPPABLE_ {-# OVERLAPPABLE #-}
#define OVERLAPPING_  {-# OVERLAPPING #-}
#ifdef NEEDS_INCOHERENT
#define INCOHERENT_ {-# INCOHERENT #-}
#endif
#else
{-# LANGUAGE OverlappingInstances #-}
#define OVERLAPPABLE_
#define OVERLAPPING_
#ifdef NEEDS_INCOHERENT
{-# LANGUAGE IncoherentInstances #-}
#define INCOHERENT_
#endif
#endif
