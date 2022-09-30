import json
import random
import copy

COLORS = ["green", "green", "green", "yellow", "yellow", "yellow", "red"]
# COLORS = ["green", "green", "yellow", "yellow", "yellow", "yellow", "red"]

CLASSES = ["1a", "1b", "2a", "2b"]

EVENTS = {
    "1": {"name": "Aquarium", "capacity": 12},
    "2": {"name": "Meeresverschmutzung", "capacity": 12},
    "3": {"name": "Wilde Tiere des Meeres", "capacity": 12},
    "4": {"name": "Wattenmeer", "capacity": 12},
    "5": {"name": "Wasserkraftwerk", "capacity": 12},
    "6": {"name": "Nachhaltige Fischerei", "capacity": 12},
    "7": {"name": "Wasser in der Bibel", "capacity": 12},
}

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
    for cl in CLASSES:
        for i in range(20):
            choices = {}
            for i, ch in random_choices():
                choices[i] = ch
            yield {
                "name": next(n),
                "class": cl,
                "choices": choices,
            }


def random_choices():
    assert len(COLORS) == len(EVENTS)
    cols = copy.deepcopy(COLORS)
    random.shuffle(cols)
    for i, e in enumerate(EVENTS.keys()):
        yield (e, cols[i])


def main():
    assert len(CLASSES) == 4
    assert len(EVENTS) == 7
    assert len(NAMES_MALE) == len(NAMES_FEMALE) == 40

    data = {"classes": CLASSES, "events": EVENTS, "pupils": list(pupils())}
    result = json.dumps(data, indent=4)
    print(result)


if __name__ == "__main__":
    main()
