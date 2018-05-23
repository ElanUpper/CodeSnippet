# the single-asterisk form of *args can be used as a parameter to
#    send a non-keyworded variable-length argument list to functions
# The double asterisk form of **kwargs is used to pass a keyworded,
#    variable-length argument dictionary to a function.

# def example(arg_1, arg_2, *args, **kwargs):

def func(*argv, **kwargv):

    print('argv: ', *argv, ' kwargv: ', kwargv)

    # print list
    for item in argv:
        print(type(item), item)

    # print dict parameter: kwargv
    for (kwkey, kwval) in kwargv.items():
        print("the value of {} is {}".format(kwkey, kwval))


func('elan', 'wang', 21, ('elan', 'wang'), {'name':'elan', 'age':23},
          name = 'elan', age = 23 )