"use strict"

export const indexedDB = window => () => window.indexedDB

export const open = name => version => factory => () => factory.open(name, version)

export const onupgradeneeded = openRequest => f => () => openRequest.onupgradeneeded = event => f(event.oldVersion)()

export const onsuccess = openRequest => f => () => openRequest.onsuccess = f

export const result = openRequest => () => openRequest.result

export const createObjectStore = name => db => () => db.createObjectStore(name, { autoIncrement: true })

export const deleteObjectStore = name => db => () => db.deleteObjectStore(name)

export const transaction_ = mode => name => db => () => db.transaction(name, mode)

export const objectStore = name => tx => () => tx.objectStore(name)

export const add = value => store => () => store.add(value)

export const getAll = store => () => store.getAll()

export const getAllKeys = store => () => store.getAllKeys()

export const delete_ = store => key => () => store.delete(key)
