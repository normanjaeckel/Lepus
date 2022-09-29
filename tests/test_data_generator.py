import json
import random

CLASSES = ["1a", "1b", "2a", "2b"]

EVENTS = [
    {"name": "Aquarium", "capacity": 12},
    {"name": "Meeresverschmutzung", "capacity": 12},
    {"name": "Wilde Tiere des Meeres", "capacity": 12},
    {"name": "Wattenmeer", "capacity": 12},
    {"name": "Wasserkraftwerk", "capacity": 12},
    {"name": "Nachhaltige Fischerei", "capacity": 12},
    {"name": "Wasser in der Bibel", "capacity": 12},
]

NAMES_MALE = [
    "Laurence Bock",
    "Fynn Knobel",
    "Ömer Kubera",
    "Joscha Hertel",
    "Kenan Slotta",
    "Daniel Hornung",
    "Dennis Wehrsen",
    "Jayson Koob",
    "Mads Bussmann",
    "Mehmet Linden",
    "Alessio Bos",
    "Ecrin Poschmann",
    "Jannek Siemon",
    "Ilyas Stöckert",
    "Enno Balnuweit",
    "Andrew Heinke",
    "Nino Otte",
    "Markus Laack",
    "Moritz Newton",
    "Janek Bartels",
    "Santino Mebold",
    "Artur Assmus",
    "James Weyel",
    "Emre Dauer",
    "Gustav Rangen",
    "Steven Hold",
    "Bruno Hannecker",
    "Tillmann Kinzel",
    "Chris Mühleis",
    "Adam Sussmann",
    "Malte Bauschinger",
    "Marlon Knorscheidt",
    "Lasse Blochwitz",
    "Lorenzo Jaros",
    "Juan Jagusch",
    "Luis Kleiss",
    "Klemens Kölotzei",
    "Lutz Bremser",
    "Timo Loska",
    "Davin Knippel",
]


NAMES_FEMALE = [
    "Miray Hoppe",
    "Milena Muckenthaler",
    "Maira Gatzka",
    "Marina Brinkmann",
    "Jolie Spelmeyer",
    "Lotta Görmer",
    "Tamina Newton",
    "Maria Ehrig",
    "Alma Berger",
    "Luzie Gadschiew",
    "Denise Figura",
    "Hanna Balzer",
    "Amanda Görlich",
    "Felicia Grube",
    "Amalia Kaufmann",
    "Paulina Dethloff",
    "Anna Thyssen",
    "Lotta Kedzierski",
    "Maya Bruckmann",
    "Annabelle Friedrich",
    "Ashley Kulma",
    "Malina Wiese",
    "Leona Werle",
    "Laura Dreher",
    "Sienna Leist",
    "Lilia Möllinger",
    "Sanja Seidel",
    "Lotta Bourrouag",
    "Mailin Verniest",
    "Inka Minah",
    "Meike Rumpf",
    "Evelina Böhm",
    "Stephanie Holinski",
    "Nika Tischmann",
    "Mia Thomas",
    "Paulina Pfeiffer",
    "Adriana Dauer",
    "Florentine Balzer",
    "Jana Meissner",
    "Vanessa Goldkühle",
]


def names():
    m = iter(NAMES_MALE)
    f = iter(NAMES_FEMALE)
    for i in range(40):
        yield next(m)
        yield next(f)


def pupils():
    n = iter(names())
    for c in CLASSES:
        for i in range(20):
            yield {
                "name": next(n),
                "class": c,
                "choices": list(random_choices()),
            }


def random_choices():
    colors = ["green", "green", "green", "yellow", "yellow", "yellow", "red"]
    assert len(colors) == len(EVENTS)
    random.shuffle(colors)
    for i, e in enumerate(EVENTS):
        yield {"event": e, "type": colors[i]}


def main():
    assert len(CLASSES) == 4
    assert len(EVENTS) == 7
    assert len(NAMES_MALE) == len(NAMES_FEMALE) == 40

    data = {"classes": CLASSES, "events": EVENTS, "pupils": list(pupils())}
    result = json.dumps(data, indent=4)
    print(result)


if __name__ == "__main__":
    main()
