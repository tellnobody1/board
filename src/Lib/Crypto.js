"use strict"

export const crypto_ = () => crypto

export const randomUUID = crypto_ => () => crypto_.randomUUID()
