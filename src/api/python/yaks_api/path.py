class Path(object):
    def __init__(self, path):
        self.validate_selector_path(path)
        self.path = path

    @staticmethod
    def validate_selector_path(p):
        return True

    def get_query(self):
        pass

    def is_prefix(self, prefix):
        self.path.startswith(prefix)
