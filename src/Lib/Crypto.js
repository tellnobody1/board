"use strict"

export const crypto = window => () => window.crypto

export const randomUUID = crypto => () => crypto.randomUUID()
