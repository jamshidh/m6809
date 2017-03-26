{-# LANGUAGE TemplateHaskell #-}

module Options where

import HFlags

defineFlag "debug" False "Turn on debugging."
defineFlag "debugqq" False "Turn on debugging."
