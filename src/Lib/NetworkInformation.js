"use strict"

export const connection = navigator => () => navigator.connection

export const downlink = connection => () => connection.downlink
