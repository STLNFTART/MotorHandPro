#!/usr/bin/env python3
"""
LAM Multi-Language Support (i18n)
Internationalization for global reach
"""
from typing import Dict, Any
import json

TRANSLATIONS = {
    "en": {
        "welcome": "Welcome to LAM",
        "plan_trip": "Plan a Trip",
        "make_reservation": "Make Reservation",
        "order_food": "Order Food",
        "status": "System Status",
        "quantum_stable": "Quantum Resonance Stable",
        "error": "An error occurred",
        "success": "Operation successful"
    },
    "es": {
        "welcome": "Bienvenido a LAM",
        "plan_trip": "Planificar Viaje",
        "make_reservation": "Hacer Reservación",
        "order_food": "Ordenar Comida",
        "status": "Estado del Sistema",
        "quantum_stable": "Resonancia Cuántica Estable",
        "error": "Ocurrió un error",
        "success": "Operación exitosa"
    },
    "fr": {
        "welcome": "Bienvenue à LAM",
        "plan_trip": "Planifier un Voyage",
        "make_reservation": "Faire une Réservation",
        "order_food": "Commander de la Nourriture",
        "status": "État du Système",
        "quantum_stable": "Résonance Quantique Stable",
        "error": "Une erreur s'est produite",
        "success": "Opération réussie"
    },
    "de": {
        "welcome": "Willkommen bei LAM",
        "plan_trip": "Reise Planen",
        "make_reservation": "Reservierung Machen",
        "order_food": "Essen Bestellen",
        "status": "Systemstatus",
        "quantum_stable": "Quantenresonanz Stabil",
        "error": "Ein Fehler ist aufgetreten",
        "success": "Operation erfolgreich"
    },
    "zh": {
        "welcome": "欢迎使用 LAM",
        "plan_trip": "规划旅行",
        "make_reservation": "预订",
        "order_food": "订餐",
        "status": "系统状态",
        "quantum_stable": "量子共振稳定",
        "error": "发生错误",
        "success": "操作成功"
    },
    "ja": {
        "welcome": "LAMへようこそ",
        "plan_trip": "旅行を計画",
        "make_reservation": "予約する",
        "order_food": "食べ物を注文",
        "status": "システムステータス",
        "quantum_stable": "量子共鳴安定",
        "error": "エラーが発生しました",
        "success": "操作成功"
    }
}


class I18n:
    """Internationalization manager"""

    def __init__(self, default_lang: str = "en"):
        self.default_lang = default_lang
        self.current_lang = default_lang

    def set_language(self, lang: str):
        """Set current language"""
        if lang in TRANSLATIONS:
            self.current_lang = lang
        else:
            print(f"Warning: Language '{lang}' not supported, using '{self.default_lang}'")

    def t(self, key: str, lang: str = None) -> str:
        """Translate a key"""
        language = lang or self.current_lang

        if language not in TRANSLATIONS:
            language = self.default_lang

        return TRANSLATIONS[language].get(key, key)

    def get_available_languages(self) -> list:
        """Get list of available languages"""
        return list(TRANSLATIONS.keys())


i18n = I18n()


if __name__ == "__main__":
    print("=== LAM Multi-Language Support ===\n")

    for lang in ["en", "es", "fr", "de", "zh", "ja"]:
        i18n.set_language(lang)
        print(f"{lang.upper()}: {i18n.t('welcome')}")
