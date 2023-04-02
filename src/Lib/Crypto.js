"use strict"

export const crypto = window => () => window.crypto

export const randomUUID = crypto => () => crypto.randomUUID()

export const getRandomValues = xs => crypto => () => crypto.getRandomValues(xs)
