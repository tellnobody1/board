"use strict"

export const indexedDB = window => () => window.indexedDB

export const open = name => db => () => db.open(name)

export const onsuccess = openRequest => f => () => openRequest.onsuccess = f

export const result = openRequest => () => openRequest.result
