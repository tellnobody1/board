"use strict"

export const hasConnection = navigator => () => navigator.connection !== undefined

export const connection = navigator => () => navigator.connection

export const downlink = connection => () => connection.downlink
