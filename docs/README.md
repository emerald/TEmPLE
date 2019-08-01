For documentation, we use
[Sphinx](http://www.sphinx-doc.org/en/master/), a Python-based
documentation generator.

You can browse pre-built documentation online at
[ReadTheDocs](https://emerald-temple.readthedocs.io/en/latest/).

## Manually Build the Documentation

In the end, all you need to do is:

```
$ make html
```

However, to be able to do this, you will need to have Python 3, `pip`
(a package manager) for Python 3, and a couple Python 3 packages
installed. Install Python 3 and `pip` using your favourite package
manager. (If you also have Python 2 installed, `pip` may end up being
called `pip3`.)

The formal list of required Python 3 packages is in
[requirements.txt](requirements.txt). Although you can go ahead and
install these packages globally, we **do not recommend doing so**:

```
$ python3 install -r requirements.txt
```

Instead, we can recommend using
[`virtualenv`](https://virtualenv.pypa.io/en/stable/), to create a
dedicated [Virtual
Envionment](https://packaging.python.org/tutorials/installing-packages/#creating-virtual-environments)
for building (this) Sphinx documentation. This way, you avoid
littering your global Python installation with our mundane local
requirements!

The workflow with `virtualenv` is roughly as follows:

  * Install `virtualenv`:
    ```
    $ pip3 install virtualenv
    ```
  * Each time you need a dedicated Python environment, create it
    using:
    ```
    $ virtualenv ~/.venvs/<env>
    ```
    where you replace `<env>` with a suitable name for your
    environment (e.g., `sphinx`, `TEmPLE`).

    This creates a directory `~/.venvs/<env>` with some binaries,
    scripts, and configuration files. Hypothetically, this directory
    can go anywhere, but `~/.venvs/` is the cannonical location.

  * Each time you need a shell within that environment:
    ```
    $ source ~/.venvs/<env>/bin/active
    ```
  * To leave this shell:
    ```
    $ deactivate
    ```

For example, the first time you build this documentation:

```
$ pip3 install virtualenv
$ virtualenv ~/.venvs/TEmPLE
$ source ~/.venvs/TEmPLE/bin/activate
$ pip3 install -r requirements.txt
$ make html
$ deactivate
```

The second time, the following suffices:

```
$ source ~/.venvs/TEmPLE/bin/activate
$ make html
$ deactivate
```

Of course, if you intend to repeatedly build the documentation (e.g.,
as you write a new section), there is no need to repeatedly leave and
enter the virtual environment.
