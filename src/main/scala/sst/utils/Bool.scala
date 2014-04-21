package sst.utils

sealed trait Bool
sealed trait True extends Bool
sealed trait False extends Bool
sealed trait Maybe extends Bool
