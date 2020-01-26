#ifndef ITERBASE_HPP_
#define ITERBASE_HPP_

// This file consists of utilities used for the generic nature of the
// iterable wrapper classes.  As such, the contents of this file should be
// considered UNDOCUMENTED and is subject to change without warning.  This
// also applies to the name of the file.  No user code should include
// this file directly.

#include <cassert>
#include <cstddef>
#include <functional>
#include <iterator>
#include <optional>
#include <tuple>
#include <type_traits>
#include <utility>

// see gcc bug 87651
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=87651
#ifdef __GNUC__
#define NO_GCC_FRIEND_ERROR __GNUC__ < 8
#else
#define NO_GCC_FRIEND_ERROR 1
#endif

namespace iter {
  namespace impl {
    namespace get_iters {
      // begin() for C arrays
      template <typename T, std::size_t N>
      T* get_begin_impl(T (&array)[N], int) {
        return array;
      }

      // Prefer member begin().
      template <typename T, typename I = decltype(std::declval<T&>().begin())>
      I get_begin_impl(T& r, int) {
        return r.begin();
      }

      // Use ADL otherwises.
      template <typename T, typename I = decltype(begin(std::declval<T&>()))>
      I get_begin_impl(T& r, long) {
        return begin(r);
      }

      template <typename T>
      auto get_begin(T& t) -> decltype(get_begin_impl(std::declval<T&>(), 42)) {
        return get_begin_impl(t, 42);
      }

      // end() for C arrays
      template <typename T, std::size_t N>
      T* get_end_impl(T (&array)[N], int) {
        return array + N;
      }

      // Prefer member end().
      template <typename T, typename I = decltype(std::declval<T&>().end())>
      I get_end_impl(T& r, int) {
        return r.end();
      }

      // Use ADL otherwise.
      template <typename T, typename I = decltype(end(std::declval<T&>()))>
      I get_end_impl(T& r, long) {
        return end(r);
      }

      template <typename T>
      auto get_end(T& t) -> decltype(get_end_impl(std::declval<T&>(), 42)) {
        return get_end_impl(t, 42);
      }
    }
    using get_iters::get_begin;
    using get_iters::get_end;

    template <typename T>
    struct type_is {
      using type = T;
    };

    template <typename T>
    using AsConst = decltype(std::as_const(std::declval<T&>()));

    // iterator_type<C> is the type of C's iterator
    // TODO: See bug
    // https://developercommunity.visualstudio.com/content/problem/252157/sfinae-error-depends-on-name-of-template-parameter.html
    // for why we use T instead of Container.  Should be
    // changed back to Container when that bug is fixed in
    // MSVC.
    template <typename T>
    using iterator_type = decltype(get_begin(std::declval<T&>()));

    // iterator_type<C> is the type of C's iterator
    template <typename Container>
    using const_iterator_type = decltype(
        get_begin(std::declval<const std::remove_reference_t<Container>&>()));

    // iterator_deref<C> is the type obtained by dereferencing an iterator
    // to an object of type C
    template <typename Container>
    using iterator_deref = decltype(*std::declval<iterator_type<Container>&>());

    // const_iteator_deref is the type obtained through dereferencing
    // a const iterator& (note: not a const_iterator).  ie: the result
    // of Container::iterator::operator*() const
    template <typename Container>
    using const_iterator_deref =
        decltype(*std::declval<const iterator_type<Container>&>());

    // the type of dereferencing a const_iterator
    template <typename Container>
    using const_iterator_type_deref =
        decltype(*std::declval<const_iterator_type<Container>&>());

    template <typename Container>
    using iterator_traits_deref =
        std::remove_reference_t<iterator_deref<Container>>;

    template <typename T, typename = void>
    struct IsIterable : std::false_type {};

    // Assuming that if a type works with begin, it is an iterable.
    template <typename T>
    struct IsIterable<T, std::void_t<iterator_type<T>>> : std::true_type {};

    template <typename T>
    constexpr bool is_iterable = IsIterable<T>::value;

    namespace detail {
      template <typename T, typename = void>
      struct ArrowHelper {
        using type = void;
        void operator()(T&) const noexcept {}
      };

      template <typename T>
      struct ArrowHelper<T*, void> {
        using type = T*;
        constexpr type operator()(T* t) const noexcept {
          return t;
        }
      };

      template <typename T>
      struct ArrowHelper<T,
          std::void_t<decltype(std::declval<T&>().operator->())>> {
        using type = decltype(std::declval<T&>().operator->());
        type operator()(T& t) const {
          return t.operator->();
        }
      };

      template <typename T>
      using arrow = typename detail::ArrowHelper<T>::type;
    }

    // type of C::iterator::operator->, also works with pointers
    // void if the iterator has no operator->
    template <typename C>
    using iterator_arrow = detail::arrow<iterator_type<C>>;

    // applys the -> operator to an object, if the object is a pointer,
    // it returns the pointer
    template <typename T>
    detail::arrow<T> apply_arrow(T& t) {
      return detail::ArrowHelper<T>{}(t);
    }

    // For iterators that have an operator* which returns a value
    // they can return this type from their operator-> instead, which will
    // wrap an object and allow it to be used with arrow
    template <typename T>
    class ArrowProxy {
     private:
      using TPlain = typename std::remove_reference<T>::type;
      T obj;

     public:
      constexpr ArrowProxy(T&& in_obj) : obj(std::forward<T>(in_obj)) {}

      TPlain* operator->() {
        return &obj;
      }
    };

    template <typename, typename = void>
    struct is_random_access_iter : std::false_type {};

    template <typename T>
    struct is_random_access_iter<T,
        std::enable_if_t<
            std::is_same<typename std::iterator_traits<T>::iterator_category,
                std::random_access_iterator_tag>::value>> : std::true_type {};

    template <typename T>
    using has_random_access_iter = is_random_access_iter<iterator_type<T>>;
    // because std::advance assumes a lot and is actually smart, I need a dumb

    // version that will work with most things
    template <typename InputIt, typename Distance = std::size_t>
    void dumb_advance_unsafe(InputIt& iter, Distance distance) {
      for (Distance i(0); i < distance; ++i) {
        ++iter;
      }
    }

    template <typename Iter, typename EndIter, typename Distance>
    void dumb_advance_impl(
        Iter& iter, const EndIter& end, Distance distance, std::false_type) {
      for (Distance i(0); i < distance && iter != end; ++i) {
        ++iter;
      }
    }

    template <typename Iter, typename EndIter, typename Distance>
    void dumb_advance_impl(
        Iter& iter, const EndIter& end, Distance distance, std::true_type) {
      if (static_cast<Distance>(end - iter) < distance) {
        iter = end;
      } else {
        iter += distance;
      }
    }

    // iter will not be incremented past end
    template <typename Iter, typename EndIter, typename Distance = std::size_t>
    void dumb_advance(Iter& iter, const EndIter& end, Distance distance) {
      dumb_advance_impl(iter, end, distance, is_random_access_iter<Iter>{});
    }

    template <typename ForwardIt, typename Distance = std::size_t>
    ForwardIt dumb_next(ForwardIt it, Distance distance = 1) {
      dumb_advance_unsafe(it, distance);
      return it;
    }

    template <typename ForwardIt, typename Distance = std::size_t>
    ForwardIt dumb_next(
        ForwardIt it, const ForwardIt& end, Distance distance = 1) {
      dumb_advance(it, end, distance);
      return it;
    }

    template <typename Container, typename Distance = std::size_t>
    Distance dumb_size(Container&& container) {
      Distance d{0};
      auto end_it = get_end(container);
      for (auto it = get_begin(container); it != end_it; ++it) {
        ++d;
      }
      return d;
    }

    template <typename... Ts>
    struct are_same : std::true_type {};

    template <typename T, typename U, typename... Ts>
    struct are_same<T, U, Ts...>
        : std::integral_constant<bool,
              std::is_same<T, U>::value && are_same<T, Ts...>::value> {};

    // DerefHolder holds the value gotten from an iterator dereference
    // if the iterate dereferences to an lvalue references, a pointer to the
    //     element is stored
    // if it does not, a value is stored instead
    // get() returns a reference to the held item
    // get_ptr() returns a pointer to the held item
    // reset() replaces the currently held item
    template <typename T>
    class DerefHolder {
     private:
      static_assert(!std::is_lvalue_reference<T>::value,
          "Non-lvalue-ref specialization used for lvalue ref type");
      // it could still be an rvalue reference
      using TPlain = std::remove_reference_t<T>;

      std::optional<TPlain> item_p_;

     public:
      using reference = TPlain&;
      using pointer = TPlain*;

      static constexpr bool stores_value = true;

      DerefHolder() = default;

      reference get() {
        assert(item_p_.has_value());
        return *item_p_;
      }

      pointer get_ptr() {
        assert(item_p_.has_value());
        return &item_p_.value();
      }

      void reset(T&& item) {
        item_p_.emplace(std::move(item));
      }

      explicit operator bool() const {
        return static_cast<bool>(item_p_);
      }
    };

    // Specialization for when T is an lvalue ref
    template <typename T>
    class DerefHolder<T&> {
     public:
      using reference = T&;
      using pointer = T*;

     private:
      pointer item_p_{};

     public:
      static constexpr bool stores_value = false;

      DerefHolder() = default;

      reference get() {
        assert(item_p_);
        return *item_p_;
      }

      pointer get_ptr() {
        assert(item_p_);
        return item_p_;
      }

      void reset(reference item) {
        item_p_ = &item;
      }

      explicit operator bool() const {
        return item_p_ != nullptr;
      }
    };

    // allows f(x) to be 'called' as x | f
    // let the record show I dislike adding yet another syntactical mess to
    // this clown car of a language.
    template <typename ItTool>
    struct Pipeable {
      template <typename T>
      friend decltype(auto) operator|(T&& x, const Pipeable& p) {
        return static_cast<const ItTool&>(p)(std::forward<T>(x));
      }
    };

    // Pipeable Callable generator, where ItImpl is templated on the first
    // argument to the call.
    template <template <typename> class ItImpl>
    struct IterToolFn : Pipeable<IterToolFn<ItImpl>> {
      template <typename T, typename... Ts>
      ItImpl<T> operator()(T&& t, Ts... ts) const {
        return {std::forward<T>(t), std::move(ts)...};
      }
    };

    // Pipeable callable which allows binding of the first argument
    // f(a, b) is the same as b | f(a)
    template <typename F>
    struct PipeableAndBindFirst : Pipeable<F> {
     protected:
      template <typename T>
      struct FnPartial : Pipeable<FnPartial<T>> {
        mutable T stored_arg;
        constexpr FnPartial(T in_t) : stored_arg(in_t) {}

        template <typename Container>
        auto operator()(Container&& container) const {
          return F{}(stored_arg, std::forward<Container>(container));
        }
      };

     public:
      template <typename T, typename = std::enable_if_t<!is_iterable<T>>>
      FnPartial<std::decay_t<T>> operator()(T&& t) const {
        return {std::forward<T>(t)};
      }
    };

    // This is a complicated class to generate a callable that can work:
    //  (1) with just a single (iterable) passed, and DefaultT substituted
    //  (2) with an iterable and a callable
    //  (3) with just a callable, to have the iterable passed later via pipe
    template <template <typename, typename> class ItImpl, typename DefaultT>
    struct IterToolFnOptionalBindFirst
        : PipeableAndBindFirst<IterToolFnOptionalBindFirst<ItImpl, DefaultT>> {
     private:
      using Base =
          PipeableAndBindFirst<IterToolFnOptionalBindFirst<ItImpl, DefaultT>>;

     protected:
      template <typename Container>
      auto operator()(Container&& container, std::false_type) const {
        return static_cast<const Base&>(*this)(
            std::forward<Container>(container));
      }

      template <typename Container>
      auto operator()(Container&& container, std::true_type) const {
        return (*this)(DefaultT{}, std::forward<Container>(container));
      }

     public:
      template <typename T>
      auto operator()(T&& t) const {
        return (*this)(std::forward<T>(t), IsIterable<T>{});
      }

      template <typename T, typename Container,
          typename = std::enable_if_t<is_iterable<Container>>>
      ItImpl<T, Container> operator()(T func, Container&& container) const {
        return {std::move(func), std::forward<Container>(container)};
      }
    };

    template <template <typename, typename> class ItImpl, typename DefaultT>
    struct IterToolFnOptionalBindSecond
        : Pipeable<IterToolFnOptionalBindSecond<ItImpl, DefaultT>> {
     private:
      // T is whatever is being held for later use
      template <typename T>
      struct FnPartial : Pipeable<FnPartial<T>> {
        mutable T stored_arg;
        constexpr FnPartial(T in_t) : stored_arg(in_t) {}

        template <typename Container>
        auto operator()(Container&& container) const {
          return IterToolFnOptionalBindSecond{}(
              std::forward<Container>(container), stored_arg);
        }
      };

     public:
      template <typename Container, typename T>
      ItImpl<Container, T> operator()(Container&& container, T func) const {
        return {std::forward<Container>(container), std::move(func)};
      }

      template <typename T, typename = std::enable_if_t<!is_iterable<T>>>
      FnPartial<std::decay_t<T>> operator()(T&& func) const {
        return {std::forward<T>(func)};
      }

      template <typename Container,
          typename = std::enable_if_t<is_iterable<Container>>>
      auto operator()(Container&& container) const {
        return (*this)(std::forward<Container>(container), DefaultT{});
      }
    };

    template <template <typename> class ItImpl>
    struct IterToolFnBindSizeTSecond {  // NOTE not pipeable
     private:
      using Size = std::size_t;
      struct FnPartial : Pipeable<FnPartial> {
        Size sz{};
        constexpr FnPartial(Size in_sz) : sz{in_sz} {}

        template <typename Container>
        auto operator()(Container&& container) const {
          return IterToolFnBindSizeTSecond{}(
              std::forward<Container>(container), sz);
        }
      };

     public:
      FnPartial operator()(Size sz) const {
        return {sz};
      }

      template <typename Container,
          typename = std::enable_if_t<is_iterable<Container>>>
      ItImpl<Container> operator()(Container&& container, Size sz) const {
        return {std::forward<Container>(container), sz};
      }
    };
  }
}

#endif
#ifndef ITERTOOLS_ITERATOR_WRAPPER_HPP_
#define ITERTOOLS_ITERATOR_WRAPPER_HPP_

#include <variant>

namespace iter {
  namespace impl {
    // iterator_end_type<C> is the type of C's end iterator
    template <typename Container>
    using iterator_end_type = decltype(get_end(std::declval<Container&>()));

    template <typename SubIter, typename SubEnd>
    class IteratorWrapperImpl;

    // If begin and end return the same type, type will be
    // iterator_type<Container>
    // If begin and end return different types, type will be IteratorWrapperImpl
    template <typename Container, bool same_types>
    struct IteratorWrapperImplType;

    template <typename Container>
    struct IteratorWrapperImplType<Container, true>
        : type_is<iterator_type<Container>> {};

    template <typename Container>
    struct IteratorWrapperImplType<Container, false>
        : type_is<IteratorWrapperImpl<iterator_type<Container>,
              iterator_end_type<Container>>> {};

    template <typename Container>
    using IteratorWrapper = typename IteratorWrapperImplType<Container,
        std::is_same_v<impl::iterator_type<Container>,
            impl::iterator_end_type<Container>>>::type;
  }
}

template <typename SubIter, typename SubEnd>
class iter::impl::IteratorWrapperImpl {
 private:
  static_assert(!std::is_same_v<SubIter, SubEnd>);
  SubIter& sub_iter() {
    auto* sub = std::get_if<SubIter>(&sub_iter_or_end_);
    assert(sub);
    return *sub;
  }

  const SubIter& sub_iter() const {
    auto* sub = std::get_if<SubIter>(&sub_iter_or_end_);
    assert(sub);
    return *sub;
  }

  std::variant<SubIter, SubEnd> sub_iter_or_end_;

 public:
  IteratorWrapperImpl() : IteratorWrapperImpl(SubIter{}) {}

  IteratorWrapperImpl(SubIter&& it) : sub_iter_or_end_{std::move(it)} {}

  IteratorWrapperImpl(SubEnd&& it) : sub_iter_or_end_(std::move(it)) {}

  IteratorWrapperImpl& operator++() {
    ++sub_iter();
    return *this;
  }

  decltype(auto) operator*() {
    return *sub_iter();
  }

  decltype(auto) operator*() const {
    return *sub_iter();
  }

  decltype(auto) operator-> () {
    return apply_arrow(sub_iter());
  }

  decltype(auto) operator-> () const {
    return apply_arrow(sub_iter());
  }

  bool operator!=(const IteratorWrapperImpl& other) const {
    constexpr static struct : std::not_equal_to<void> {
      // specially compare Ends because rangev3 sentinels are not equality
      // comparable
      bool operator()(const SubEnd&, const SubEnd&) const {
        return false;
      }
      using std::not_equal_to<void>::operator();
    } not_equal;
    return std::visit(not_equal, sub_iter_or_end_, other.sub_iter_or_end_);
  }

  bool operator==(const IteratorWrapperImpl& other) const {
    return !(*this != other);
  }
};

#endif
#ifndef ITERTOOLS_ITER_TUPLES_HPP_
#define ITERTOOLS_ITER_TUPLES_HPP_



namespace iter {
  namespace impl {
    namespace detail {
      template <typename... Ts>
      std::tuple<iterator_deref<Ts>...> iterator_tuple_deref_helper(
          const std::tuple<Ts...>&);

      template <typename... Ts>
      std::tuple<IteratorWrapper<Ts>...> iterator_tuple_type_helper(
          const std::tuple<Ts...>&);

      template <typename... Ts>
      std::tuple<iterator_deref<const std::remove_reference_t<Ts>>...>
      const_iterator_tuple_deref_helper(const std::tuple<Ts...>&);

      template <typename... Ts>
      std::tuple<IteratorWrapper<const std::remove_reference_t<Ts>>...>
      const_iterator_tuple_type_helper(const std::tuple<Ts...>&);
    }
    // Given a tuple template argument, evaluates to a tuple of iterators
    // for the template argument's contained types.
    template <typename TupleType>
    using iterator_tuple_type =
        decltype(detail::iterator_tuple_type_helper(std::declval<TupleType>()));

    template <typename TupleType>
    using const_iterator_tuple_type = decltype(
        detail::const_iterator_tuple_type_helper(std::declval<TupleType>()));

    // Given a tuple template argument, evaluates to a tuple of
    // what the iterators for the template argument's contained types
    // dereference to
    template <typename TupleType>
    using iterator_deref_tuple = decltype(
        detail::iterator_tuple_deref_helper(std::declval<TupleType>()));

    template <typename TupleType>
    using const_iterator_deref_tuple = decltype(
        detail::const_iterator_tuple_deref_helper(std::declval<TupleType>()));

    // function absorbing all arguments passed to it. used when
    // applying a function to a parameter pack but not passing the evaluated
    // results anywhere
    template <typename... Ts>
    void absorb(Ts&&...) {}
  }
}

#endif
#ifndef ITERATOR_ITERATOR_HPP_
#define ITERATOR_ITERATOR_HPP_


// IterIterWrapper and IteratorIterator provide a means to have a container
// of iterators act like a container of the pointed to objects. This is useful
// for combinatorics and similar itertools which need to keep track of
// more than one element at a time.
// an IterIterWrapper<some_collection_type<collection<T>::iterator>>
// behave like some_collection<T> when iterated over or indexed

namespace iter {
  namespace impl {
    template <typename T, typename = void>
    struct HasConstDeref : std::false_type {};

    template <typename T>
    struct HasConstDeref<T, std::void_t<decltype(*std::declval<const T&>())>>
        : std::true_type {};

    template <typename TopIter>
    class IteratorIterator {
      template <typename> friend class IteratorIterator;
      using Diff = std::ptrdiff_t;
      static_assert(
          std::is_same<
              typename std::iterator_traits<TopIter>::iterator_category,
              std::random_access_iterator_tag>::value,
          "IteratorIterator only works with random access iterators");

     private:
      TopIter sub_iter;

     public:
      using iterator_category = std::random_access_iterator_tag;
      using value_type = std::remove_reference_t<decltype(**std::declval<TopIter>())>;
      using difference_type = std::ptrdiff_t;
      using pointer = value_type*;
      using reference = value_type&;
      IteratorIterator() = default;
      IteratorIterator(const TopIter& it) : sub_iter{it} {}

      const TopIter& get() const {
        return sub_iter;
      }

      template <typename T>
      bool operator==(const IteratorIterator<T>& other) const {
        return !(*this != other);
      }

      template <typename T>
      bool operator!=(const IteratorIterator<T>& other) const {
        return this->sub_iter != other.sub_iter;
      }

      IteratorIterator& operator++() {
        ++this->sub_iter;
        return *this;
      }

      IteratorIterator operator++(int) {
        auto ret = *this;
        ++*this;
        return ret;
      }

      IteratorIterator& operator--() {
        --this->sub_iter;
        return *this;
      }

      IteratorIterator operator--(int) {
        auto ret = *this;
        --*this;
        return ret;
      }

      auto operator*() const -> decltype(**sub_iter) {
        return **this->sub_iter;
      }

      auto operator-> () const -> decltype(*sub_iter) {
        return *this->sub_iter;
      }

      IteratorIterator& operator+=(Diff n) {
        this->sub_iter += n;
        return *this;
      }

      IteratorIterator operator+(Diff n) const {
        auto it = *this;
        it += n;
        return it;
      }

      friend IteratorIterator operator+(Diff n, IteratorIterator it) {
        it += n;
        return it;
      }

      IteratorIterator& operator-=(Diff n) {
        this->sub_iter -= n;
        return *this;
      }

      IteratorIterator operator-(Diff n) const {
        auto it = *this;
        it -= n;
        return it;
      }

      Diff operator-(const IteratorIterator& rhs) const {
        return this->sub_iter - rhs.sub_iter;
      }

      auto operator[](Diff idx) const -> decltype(*sub_iter[idx]) {
        return *sub_iter[idx];
      }

      bool operator<(const IteratorIterator& rhs) const {
        return this->sub_iter < rhs.sub_iter;
      }

      bool operator>(const IteratorIterator& rhs) const {
        return this->sub_iter > rhs.sub_iter;
      }

      bool operator<=(const IteratorIterator& rhs) const {
        return this->sub_iter <= rhs.sub_iter;
      }

      bool operator>=(const IteratorIterator& rhs) const {
        return this->sub_iter >= rhs.sub_iter;
      }
    };

    template <typename Container>
    class IterIterWrapper {
     private:
      Container container;

      using contained_iter = typename Container::value_type;
      using size_type = typename Container::size_type;
      using iterator = IteratorIterator<typename Container::iterator>;
      using const_iterator =
          IteratorIterator<typename Container::const_iterator>;
      using reverse_iterator =
          IteratorIterator<typename Container::reverse_iterator>;
      using const_reverse_iterator =
          IteratorIterator<typename Container::const_reverse_iterator>;

      template <typename U = Container, typename = void>
      struct ConstAtTypeOrVoid : type_is<void> {};

      template <typename U>
      struct ConstAtTypeOrVoid<U,
          std::void_t<decltype(*std::declval<const U&>().at(0))>>
          : type_is<decltype(*std::declval<const U&>().at(0))> {};

      using const_at_type_or_void_t = typename ConstAtTypeOrVoid<>::type;

      template <typename U = Container, typename = void>
      struct ConstIndexTypeOrVoid : type_is<void> {};

      template <typename U>
      struct ConstIndexTypeOrVoid<U,
          std::void_t<decltype(*std::declval<const U&>()[0])>>
          : type_is<decltype(*std::declval<const U&>()[0])> {};

      using const_index_type_or_void_t = typename ConstIndexTypeOrVoid<>::type;

     public:
      IterIterWrapper() = default;

      explicit IterIterWrapper(size_type sz) : container(sz) {}

      IterIterWrapper(size_type sz, const contained_iter& val)
          : container(sz, val) {}

      auto at(size_type pos) -> decltype(*container.at(pos)) {
        return *container.at(pos);
      }

      auto at(size_type pos) const -> const_at_type_or_void_t {
        return *container.at(pos);
      }

      auto operator[](size_type pos) noexcept(noexcept(*container[pos]))
          -> decltype(*container[pos]) {
        return *container[pos];
      }

      auto operator[](size_type pos) const noexcept(noexcept(*container[pos]))
          -> const_index_type_or_void_t {
        return *container[pos];
      }

      bool empty() const noexcept {
        return container.empty();
      }

      size_type size() const noexcept {
        return container.size();
      }

      iterator begin() noexcept {
        return {container.begin()};
      }

      iterator end() noexcept {
        return {container.end()};
      }

      const_iterator begin() const noexcept {
        return {container.begin()};
      }

      const_iterator end() const noexcept {
        return {container.end()};
      }

      const_iterator cbegin() const noexcept {
        return {container.cbegin()};
      }

      const_iterator cend() const noexcept {
        return {container.cend()};
      }

      reverse_iterator rbegin() noexcept {
        return {container.rbegin()};
      }

      reverse_iterator rend() noexcept {
        return {container.rend()};
      }

      const_reverse_iterator rbegin() const noexcept {
        return {container.rbegin()};
      }

      const_reverse_iterator rend() const noexcept {
        return {container.rend()};
      }

      const_reverse_iterator crbegin() const noexcept {
        return {container.rbegin()};
      }

      const_reverse_iterator crend() const noexcept {
        return {container.rend()};
      }

      // get() exposes the underlying container.  this allows the
      // itertools to manipulate the iterators in the container
      // and should not be depended on anywhere else.
      Container& get() noexcept {
        return container;
      }

      const Container& get() const noexcept {
        return container;
      }
    };
  }
}

#endif
#ifndef ITER_FILTER_H_
#define ITER_FILTER_H_


#include <initializer_list>

namespace iter {
  namespace impl {
    template <typename FilterFunc, typename Container>
    class Filtered;

    struct BoolTester {
      template <typename T>
      constexpr bool operator()(const T& item_) const {
        return bool(item_);
      }
    };

    using FilterFn = IterToolFnOptionalBindFirst<Filtered, BoolTester>;
  }

  constexpr impl::FilterFn filter{};
}

template <typename FilterFunc, typename Container>
class iter::impl::Filtered {
 private:
  Container container_;
  mutable FilterFunc filter_func_;

  friend FilterFn;

 protected:
  // Value constructor for use only in the filter function
  Filtered(FilterFunc filter_func, Container&& container)
      : container_(std::forward<Container>(container)),
        filter_func_(filter_func) {}

 public:
  Filtered(Filtered&&) = default;

  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    using Holder = DerefHolder<iterator_deref<ContainerT>>;
    mutable IteratorWrapper<ContainerT> sub_iter_;
    IteratorWrapper<ContainerT> sub_end_;
    mutable Holder item_;
    FilterFunc* filter_func_;

    // All of these are marked const because the sub_iter_ is lazily
    // initialized. The morality of this is questionable.
    void inc_sub_iter() const {
      ++sub_iter_;
      if (sub_iter_ != sub_end_) {
        item_.reset(*sub_iter_);
      }
    }

    // increment until the iterator points to is true on the
    // predicate.  Called by constructor and operator++
    void skip_failures() const {
      while (
          sub_iter_ != sub_end_ && !std::invoke(*filter_func_, item_.get())) {
        inc_sub_iter();
      }
    }

    void init_if_first_use() const {
      if (!item_ && sub_iter_ != sub_end_) {
        item_.reset(*sub_iter_);
        skip_failures();
      }
    }

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = iterator_traits_deref<ContainerT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(IteratorWrapper<ContainerT>&& sub_iter,
        IteratorWrapper<ContainerT>&& sub_end, FilterFunc& filter_func)
        : sub_iter_{std::move(sub_iter)},
          sub_end_{std::move(sub_end)},
          filter_func_(&filter_func) {}

    typename Holder::reference operator*() {
      init_if_first_use();
      return item_.get();
    }

    typename Holder::pointer operator->() {
      init_if_first_use();
      return item_.get_ptr();
    }

    Iterator& operator++() {
      init_if_first_use();
      inc_sub_iter();
      skip_failures();
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      init_if_first_use();
      other.init_if_first_use();
      return sub_iter_ != other.sub_iter_;
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return !(*this != other);
    }
  };

  Iterator<Container> begin() {
    return {get_begin(container_), get_end(container_), filter_func_};
  }

  Iterator<Container> end() {
    return {get_end(container_), get_end(container_), filter_func_};
  }

  Iterator<AsConst<Container>> begin() const {
    return {get_begin(std::as_const(container_)),
        get_end(std::as_const(container_)), filter_func_};
  }

  Iterator<AsConst<Container>> end() const {
    return {get_end(std::as_const(container_)),
        get_end(std::as_const(container_)), filter_func_};
  }
};

#endif
#ifndef ITER_STARMAP_H_
#define ITER_STARMAP_H_


#include <array>
#include <memory>

namespace iter {
  namespace impl {
    template <typename Func, typename Container>
    class StarMapper;

    template <typename Func, typename TupType, std::size_t... Is>
    class TupleStarMapper;

    struct StarMapFn;
  }
}

// NOTE I don't know why, but clang gets very confused by having  in the
// Iterators' member functions for these classes

// starmap with a container_<T> where T is one of tuple, pair, array
template <typename Func, typename Container>
class iter::impl::StarMapper {
 private:
  mutable Func func_;
  Container container_;

  using StarIterDeref = std::remove_reference_t<decltype(
      std::apply(func_, std::declval<iterator_deref<Container>>()))>;

  StarMapper(Func f, Container&& c)
      : func_(std::move(f)), container_(std::forward<Container>(c)) {}

  friend StarMapFn;

 public:
  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    Func* func_;
    IteratorWrapper<ContainerT> sub_iter_;

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = StarIterDeref;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(Func& f, IteratorWrapper<ContainerT>&& sub_iter)
        : func_(&f), sub_iter_(std::move(sub_iter)) {}

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      return sub_iter_ != other.sub_iter_;
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return !(*this != other);
    }

    Iterator& operator++() {
      ++sub_iter_;
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    decltype(auto) operator*() {
      return std::apply(*func_, *sub_iter_);
    }

    auto operator-> () -> ArrowProxy<decltype(**this)> {
      return {**this};
    }
  };

  Iterator<Container> begin() {
    return {func_, get_begin(container_)};
  }

  Iterator<Container> end() {
    return {func_, get_end(container_)};
  }

  Iterator<AsConst<Container>> begin() const {
    return {func_, get_begin(std::as_const(container_))};
  }

  Iterator<AsConst<Container>> end() const {
    return {func_, get_end(std::as_const(container_))};
  }
};

// starmap for a tuple or pair of tuples or pairs
template <typename Func, typename TupType, std::size_t... Is>
class iter::impl::TupleStarMapper {
 private:
  mutable Func func_;
  TupType tup_;

 private:
  static_assert(sizeof...(Is) == std::tuple_size<std::decay_t<TupType>>::value,
      "tuple size doesn't match size of Is");

  friend StarMapFn;

  TupleStarMapper(Func f, TupType t)
      : func_(std::move(f)), tup_(std::forward<TupType>(t)) {}

  // this is a wrapper class to hold the aliases and functions needed for the
  // Iterator.
  template <typename TupTypeT>
  class IteratorData {
   public:
    template <std::size_t Idx>
    static auto get_and_call_with_tuple(Func& f, TupTypeT& t) -> decltype(std::apply(f, std::get<Idx>(t))) { //TODO: Remove duplicated expression in decltype, using decltype(auto) as return type, when all compilers correctly deduce type (i.e. MSVC cl 19.15 does not do it).
      return std::apply(f, std::get<Idx>(t));
    }

    using ResultType = decltype(get_and_call_with_tuple<0>(func_, tup_));
    using CallerFunc = ResultType (*)(Func&, TupTypeT&);

    constexpr static std::array<CallerFunc, sizeof...(Is)> callers{
        {get_and_call_with_tuple<Is>...}};

    using TraitsValue = std::remove_reference_t<ResultType>;

    IteratorData() = delete;
  };

 public:
  template <typename TupTypeT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    Func* func_;
    std::remove_reference_t<TupTypeT>* tup_;
    std::size_t index_;

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = typename IteratorData<TupTypeT>::TraitsValue;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(Func& f, TupTypeT& t, std::size_t i)
        : func_{&f}, tup_{&t}, index_{i} {}

    decltype(auto) operator*() {
      return IteratorData<TupTypeT>::callers[index_](*func_, *tup_);
    }

    auto operator-> () {
      return ArrowProxy<decltype(**this)>{**this};
    }

    Iterator& operator++() {
      ++index_;
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      return index_ != other.index_;
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return !(*this != other);
    }
  };

  Iterator<TupType> begin() {
    return {func_, tup_, 0};
  }

  Iterator<TupType> end() {
    return {func_, tup_, sizeof...(Is)};
  }

  Iterator<AsConst<TupType>> begin() const {
    return {func_, std::as_const(tup_), 0};
  }

  Iterator<AsConst<TupType>> end() const {
    return {func_, std::as_const(tup_), sizeof...(Is)};
  }
};

struct iter::impl::StarMapFn : PipeableAndBindFirst<StarMapFn> {
 private:
  template <typename Func, typename TupType, std::size_t... Is>
  TupleStarMapper<Func, TupType, Is...> helper_with_tuples(
      Func func, TupType&& tup, std::index_sequence<Is...>) const {
    return {std::move(func), std::forward<TupType>(tup)};
  }

  template <typename T, typename = void>
  struct is_tuple_like : std::false_type {};

  template <typename T>
  struct is_tuple_like<T,
      std::void_t<decltype(std::tuple_size<std::decay_t<T>>::value)>>
      : std::true_type {};

 public:
  template <typename Func, typename Seq>
  auto operator()(Func func, Seq&& sequence) const {
    if constexpr (is_tuple_like<Seq>{}) {
      return helper_with_tuples(std::move(func), std::forward<Seq>(sequence),
          std::make_index_sequence<
              std::tuple_size<std::decay_t<Seq>>::value>{});
    } else {
      return StarMapper<Func, Seq>{
          std::move(func), std::forward<Seq>(sequence)};
    }
  }

  using PipeableAndBindFirst<StarMapFn>::operator();
};

namespace iter {
  constexpr impl::StarMapFn starmap{};
}

#endif
#ifndef ITER_ZIP_HPP_
#define ITER_ZIP_HPP_


#include <algorithm>

namespace iter {
  namespace impl {
    template <typename TupleType, std::size_t... Is>
    class Zipped;

    template <typename TupleType, std::size_t... Is>
    Zipped<TupleType, Is...> zip_impl(TupleType&&, std::index_sequence<Is...>);
  }

  template <typename... Containers>
  auto zip(Containers&&... containers);
}

template <typename TupleType, std::size_t... Is>
class iter::impl::Zipped {
 private:
  TupleType containers_;
  friend Zipped iter::impl::zip_impl<TupleType, Is...>(
      TupleType&&, std::index_sequence<Is...>);

  Zipped(TupleType&& containers) : containers_(std::move(containers)) {}

 public:
  Zipped(Zipped&&) = default;

  // template templates here because I need to defer evaluation in the const
  // iteration case for types that don't have non-const begin() and end(). If I
  // passed in the actual types of the tuples of iterators and the type for
  // deref they'd need to be known in the function declarations below.
  template <typename TupleTypeT, template <typename> class IteratorTuple,
      template <typename> class TupleDeref>
  class Iterator {
    // see gcc bug 87651
#if NO_GCC_FRIEND_ERROR
   private:
    template <typename, template <typename> class, template <typename> class>
    friend class Iterator;
#else
   public:
#endif
    IteratorTuple<TupleTypeT> iters_;

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = TupleDeref<TupleTypeT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(IteratorTuple<TupleTypeT>&& iters) : iters_(std::move(iters)) {}

    Iterator& operator++() {
      absorb(++std::get<Is>(iters_)...);
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T, template <typename> class IT,
        template <typename> class TD>
    bool operator!=(const Iterator<T, IT, TD>& other) const {
      if constexpr (sizeof...(Is) == 0) {
        return false;
      } else {
        return (... && (std::get<Is>(iters_) != std::get<Is>(other.iters_)));
      }
    }

    template <typename T, template <typename> class IT,
        template <typename> class TD>
    bool operator==(const Iterator<T, IT, TD>& other) const {
      return !(*this != other);
    }

    TupleDeref<TupleTypeT> operator*() {
      return {(*std::get<Is>(iters_))...};
    }

    auto operator-> () -> ArrowProxy<decltype(**this)> {
      return {**this};
    }
  };

  Iterator<TupleType, iterator_tuple_type, iterator_deref_tuple> begin() {
    return {{get_begin(std::get<Is>(containers_))...}};
  }

  Iterator<TupleType, iterator_tuple_type, iterator_deref_tuple> end() {
    return {{get_end(std::get<Is>(containers_))...}};
  }

  Iterator<AsConst<TupleType>, const_iterator_tuple_type,
      const_iterator_deref_tuple>
  begin() const {
    return {{get_begin(std::as_const(std::get<Is>(containers_)))...}};
  }

  Iterator<AsConst<TupleType>, const_iterator_tuple_type,
      const_iterator_deref_tuple>
  end() const {
    return {{get_end(std::as_const(std::get<Is>(containers_)))...}};
  }
};

template <typename TupleType, std::size_t... Is>
iter::impl::Zipped<TupleType, Is...> iter::impl::zip_impl(
    TupleType&& containers, std::index_sequence<Is...>) {
  return {std::move(containers)};
}

template <typename... Containers>
auto iter::zip(Containers&&... containers) {
  return impl::zip_impl(
      std::tuple<Containers...>{std::forward<Containers>(containers)...},
      std::index_sequence_for<Containers...>{});
}

#endif
#ifndef ITER_ACCUMULATE_H_
#define ITER_ACCUMULATE_H_



namespace iter {
  namespace impl {
    template <typename Container, typename AccumulateFunc>
    class Accumulator;

    using AccumulateFn = IterToolFnOptionalBindSecond<Accumulator, std::plus<>>;
  }
  constexpr impl::AccumulateFn accumulate{};
}

template <typename Container, typename AccumulateFunc>
class iter::impl::Accumulator {
 private:
  Container container_;
  mutable AccumulateFunc accumulate_func_;

  friend AccumulateFn;

  using AccumVal = std::remove_reference_t<std::invoke_result_t<AccumulateFunc,
      iterator_deref<Container>, iterator_deref<Container>>>;

  Accumulator(Container&& container, AccumulateFunc accumulate_func)
      : container_(std::forward<Container>(container)),
        accumulate_func_(accumulate_func) {}

 public:
  Accumulator(Accumulator&&) = default;

  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    IteratorWrapper<ContainerT> sub_iter_;
    IteratorWrapper<ContainerT> sub_end_;
    AccumulateFunc* accumulate_func_;
    std::optional<AccumVal> acc_val_;

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = AccumVal;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(IteratorWrapper<ContainerT>&& sub_iter,
        IteratorWrapper<ContainerT>&& sub_end, AccumulateFunc& accumulate_fun)
        : sub_iter_{std::move(sub_iter)},
          sub_end_{std::move(sub_end)},
          accumulate_func_(&accumulate_fun),
          // only get first value if not an end iterator
          acc_val_{!(sub_iter_ != sub_end_)
                       ? std::nullopt
                       : std::make_optional<AccumVal>(*sub_iter_)} {}

    const AccumVal& operator*() const {
      return *acc_val_;
    }

    const AccumVal* operator->() const {
      return &*acc_val_;
    }

    Iterator& operator++() {
      ++sub_iter_;
      if (sub_iter_ != sub_end_) {
        *acc_val_ = std::invoke(*accumulate_func_, *acc_val_, *sub_iter_);
      }
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      return sub_iter_ != other.sub_iter_;
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return !(*this != other);
    }
  };

  Iterator<Container> begin() {
    return {get_begin(container_), get_end(container_), accumulate_func_};
  }

  Iterator<Container> end() {
    return {get_end(container_), get_end(container_), accumulate_func_};
  }
  Iterator<AsConst<Container>> begin() const {
    return {get_begin(std::as_const(container_)),
        get_end(std::as_const(container_)), accumulate_func_};
  }

  Iterator<AsConst<Container>> end() const {
    return {get_end(std::as_const(container_)),
        get_end(std::as_const(container_)), accumulate_func_};
  }
};

#endif
#ifndef ITER_BATCHED_HPP_
#define ITER_BATCHED_HPP_


#include <vector>

namespace iter {
  namespace impl {
    template <typename Container>
    class Batcher;

    using BatchedFn = IterToolFnBindSizeTSecond<Batcher>;
  }
  constexpr impl::BatchedFn batched{};
}

template <typename Container>
class iter::impl::Batcher {
 private:
  Container container_;
  std::size_t num_batches_;

  Batcher(Container&& container, std::size_t const num_batches)
      : container_(std::forward<Container>(container)), num_batches_{num_batches} {}

  friend BatchedFn;

  template <typename T>
  using IndexVector = std::vector<IteratorWrapper<T>>;
  template <typename T>
  using DerefVec = IterIterWrapper<IndexVector<T>>;

 public:
  Batcher(Batcher&&) = default;
  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    std::shared_ptr<DerefVec<ContainerT>> batch_ =
        std::make_shared<DerefVec<ContainerT>>();
    IteratorWrapper<ContainerT> sub_iter_;
    IteratorWrapper<ContainerT> sub_end_;
    std::size_t num_batches_;
    std::size_t size_;
    std::size_t count_;

    bool done() const {
      return batch_->empty();
    }

    void refill_batch() {
      batch_->get().clear();
      if (count_ < num_batches_) {
        std::size_t const batch_size(size_ / num_batches_ + std::min<std::size_t>(1, (size_ % num_batches_) / (count_ + 1)));
        batch_->get().reserve(batch_size);
        for (std::size_t i = 0; i < batch_size; ++i) {
          batch_->get().push_back(sub_iter_);
          ++sub_iter_;
        }
        ++count_;
      }
    }

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = DerefVec<ContainerT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    template <typename Iter1, typename Iter2, typename = void>
    struct distance_helper {
      static constexpr difference_type distance(Iter1 it1, Iter2 it2) {
        difference_type dist(0);
        for (; it1 != it2; ++it1)
          ++dist;
        return dist;
      }
    };

    template <typename Iter1, typename Iter2>
    struct distance_helper<Iter1, Iter2, typename std::enable_if_t<std::is_arithmetic_v<typename Iter1::difference_type> && std::is_arithmetic_v<typename Iter2::difference_type>>> {
      static constexpr difference_type distance(Iter1 it1, Iter2 it2) {
        return std::distance(it1, it2);
      }
    };

    template <typename Iter1, typename Iter2>
    difference_type distance(Iter1 it1, Iter2 it2) const {
      return distance_helper<Iter1, Iter2>::distance(it1, it2);
    }

    Iterator(IteratorWrapper<ContainerT>&& sub_iter,
        IteratorWrapper<ContainerT>&& sub_end, std::size_t num_batches)
        : sub_iter_{std::move(sub_iter)},
          sub_end_{std::move(sub_end)},
          num_batches_{num_batches},
          size_{static_cast<std::size_t>(distance(sub_iter_, sub_end_))},
          count_{0} {
      refill_batch();
    }

    Iterator& operator++() {
      refill_batch();
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      return !(*this == other);
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return done() == other.done()
             && (done() || !(sub_iter_ != other.sub_iter_));
    }

    DerefVec<ContainerT>& operator*() {
      return *batch_;
    }

    DerefVec<ContainerT>* operator->() {
      return batch_.get();
    }
  };

  Iterator<Container> begin() {
    return {get_begin(container_), get_end(container_), num_batches_};
  }

  Iterator<Container> end() {
    return {get_end(container_), get_end(container_), num_batches_};
  }

  Iterator<AsConst<Container>> begin() const {
    return {get_begin(std::as_const(container_)),
        get_end(std::as_const(container_)), num_batches_};
  }

  Iterator<AsConst<Container>> end() const {
    return {get_end(std::as_const(container_)),
        get_end(std::as_const(container_)), num_batches_};
  }
};

#endif
#ifndef ITER_CHAIN_HPP_
#define ITER_CHAIN_HPP_



namespace iter {
  namespace impl {
    template <typename TupType, std::size_t... Is>
    class Chained;

    template <typename Container>
    class ChainedFromIterable;

    using ChainFromIterableFn = IterToolFn<ChainedFromIterable>;

    // rather than a chain function, use a callable object to support
    // from_iterable
    class ChainMaker;

    template <typename>
    struct AsTupleOfConstImpl;

    template <typename... Ts>
    struct AsTupleOfConstImpl<std::tuple<Ts...>>
        : type_is<std::tuple<AsConst<Ts>...>> {};

    template <typename T>
    using AsTupleOfConst = typename AsTupleOfConstImpl<T>::type;
  }
}

template <typename TupType, std::size_t... Is>
class iter::impl::Chained {
 private:
  friend ChainMaker;

  template <typename TupTypeT>
  class IteratorData {
    IteratorData() = delete;
    static_assert(
        std::tuple_size<std::decay_t<TupTypeT>>::value == sizeof...(Is),
        "tuple size != sizeof Is");

    static_assert(
        are_same<iterator_deref<std::tuple_element_t<Is, TupTypeT>>...>::value,
        "All chained iterables must have iterators that "
        "dereference to the same type, including cv-qualifiers "
        "and references.");

   public:
    using IterTupType = iterator_tuple_type<TupTypeT>;
    using DerefType = iterator_deref<std::tuple_element_t<0, TupTypeT>>;
    using ArrowType = iterator_arrow<std::tuple_element_t<0, TupTypeT>>;

    template <std::size_t Idx>
    static DerefType get_and_deref(IterTupType& iters) {
      return *std::get<Idx>(iters);
    }

    template <std::size_t Idx>
    static ArrowType get_and_arrow(IterTupType& iters) {
      return apply_arrow(std::get<Idx>(iters));
    }

    template <std::size_t Idx>
    static void get_and_increment(IterTupType& iters) {
      ++std::get<Idx>(iters);
    }

    template <std::size_t Idx>
    static bool get_and_check_not_equal(
        const IterTupType& lhs, const IterTupType& rhs) {
      return std::get<Idx>(lhs) != std::get<Idx>(rhs);
    }

    using DerefFunc = DerefType (*)(IterTupType&);
    using ArrowFunc = ArrowType (*)(IterTupType&);
    using IncFunc = void (*)(IterTupType&);
    using NeqFunc = bool (*)(const IterTupType&, const IterTupType&);

    constexpr static std::array<DerefFunc, sizeof...(Is)> derefers{
        {get_and_deref<Is>...}};

    constexpr static std::array<ArrowFunc, sizeof...(Is)> arrowers{
        {get_and_arrow<Is>...}};

    constexpr static std::array<IncFunc, sizeof...(Is)> incrementers{
        {get_and_increment<Is>...}};

    constexpr static std::array<NeqFunc, sizeof...(Is)> neq_comparers{
        {get_and_check_not_equal<Is>...}};

    using TraitsValue =
        iterator_traits_deref<std::tuple_element_t<0, TupTypeT>>;
  };

  Chained(TupType&& t) : tup_(std::move(t)) {}
  TupType tup_;

 public:
  Chained(Chained&&) = default;

  template <typename TupTypeT>
  class Iterator {
   private:
    using IterData = IteratorData<TupTypeT>;
    std::size_t index_;
    typename IterData::IterTupType iters_;
    typename IterData::IterTupType ends_;

    void check_for_end_and_adjust() {
      while (index_ < sizeof...(Is)
             && !(IterData::neq_comparers[index_](iters_, ends_))) {
        ++index_;
      }
    }

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = typename IteratorData<TupTypeT>::TraitsValue;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(std::size_t i, typename IterData::IterTupType&& iters,
        typename IterData::IterTupType&& ends)
        : index_{i}, iters_(std::move(iters)), ends_(std::move(ends)) {
      check_for_end_and_adjust();
    }

    decltype(auto) operator*() {
      return IterData::derefers[index_](iters_);
    }

    decltype(auto) operator-> () {
      return IterData::arrowers[index_](iters_);
    }

    Iterator& operator++() {
      IterData::incrementers[index_](iters_);
      check_for_end_and_adjust();
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    // TODO make const and non-const iterators comparable
    bool operator!=(const Iterator& other) const {
      return index_ != other.index_
             || (index_ != sizeof...(Is)
                    && IterData::neq_comparers[index_](iters_, other.iters_));
    }

    bool operator==(const Iterator& other) const {
      return !(*this != other);
    }
  };

  Iterator<TupType> begin() {
    return {0, {get_begin(std::get<Is>(tup_))...},
        {get_end(std::get<Is>(tup_))...}};
  }

  Iterator<TupType> end() {
    return {sizeof...(Is), {get_end(std::get<Is>(tup_))...},
        {get_end(std::get<Is>(tup_))...}};
  }

  Iterator<AsTupleOfConst<TupType>> begin() const {
    return {0, {get_begin(std::as_const(std::get<Is>(tup_)))...},
        {get_end(std::as_const(std::get<Is>(tup_)))...}};
  }

  Iterator<AsTupleOfConst<TupType>> end() const {
    return {sizeof...(Is), {get_end(std::as_const(std::get<Is>(tup_)))...},
        {get_end(std::as_const(std::get<Is>(tup_)))...}};
  }
};

template <typename Container>
class iter::impl::ChainedFromIterable {
 private:
  friend ChainFromIterableFn;
  Container container_;
  ChainedFromIterable(Container&& container)
      : container_(std::forward<Container>(container)) {}

 public:
  ChainedFromIterable(ChainedFromIterable&&) = default;
  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    using SubContainer = iterator_deref<ContainerT>;
    using SubIter = IteratorWrapper<SubContainer>;

    IteratorWrapper<ContainerT> top_level_iter_;
    IteratorWrapper<ContainerT> top_level_end_;
    DerefHolder<SubContainer> sub_iterable_;
    std::optional<SubIter> sub_iter_p_;
    std::optional<SubIter> sub_end_p_;

    void advance_while_empty_sub_iterable() {
      while (top_level_iter_ != top_level_end_ && sub_iter_p_ == sub_end_p_) {
        ++top_level_iter_;
        update_sub_iterable();
      }
    }

    void update_sub_iterable() {
      if (top_level_iter_ != top_level_end_) {
        sub_iterable_.reset(*top_level_iter_);
        sub_iter_p_ =
            std::make_optional<SubIter>(get_begin(sub_iterable_.get()));
        sub_end_p_ = std::make_optional<SubIter>(get_end(sub_iterable_.get()));
      } else {
        sub_iter_p_.reset();
        sub_end_p_.reset();
      }
    }

    void next_sub_iterable() {
      update_sub_iterable();
      advance_while_empty_sub_iterable();
    }

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = iterator_traits_deref<iterator_deref<ContainerT>>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(IteratorWrapper<ContainerT>&& top_iter,
        IteratorWrapper<ContainerT>&& top_end)
        : top_level_iter_{std::move(top_iter)},
          top_level_end_{std::move(top_end)} {
      next_sub_iterable();
    }

    Iterator& operator++() {
      ++*sub_iter_p_;
      if (!(*sub_iter_p_ != *sub_end_p_)) {
        ++top_level_iter_;
        next_sub_iterable();
      }
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      return top_level_iter_ != other.top_level_iter_
             || sub_iter_p_ != other.sub_iter_p_;
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return !(*this != other);
    }

    iterator_deref<iterator_deref<ContainerT>> operator*() {
      return **sub_iter_p_;
    }

    iterator_arrow<iterator_deref<ContainerT>> operator->() {
      return apply_arrow(*sub_iter_p_);
    }
  };

  Iterator<Container> begin() {
    return {get_begin(container_), get_end(container_)};
  }

  Iterator<Container> end() {
    return {get_end(container_), get_end(container_)};
  }

  Iterator<AsConst<Container>> begin() const {
    return {get_begin(std::as_const(container_)),
        get_end(std::as_const(container_))};
  }

  Iterator<AsConst<Container>> end() const {
    return {
        get_end(std::as_const(container_)), get_end(std::as_const(container_))};
  }
};

class iter::impl::ChainMaker {
 private:
  template <typename TupleType, std::size_t... Is>
  Chained<TupleType, Is...> chain_impl(
      TupleType&& containers, std::index_sequence<Is...>) const {
    return {std::move(containers)};
  }

 public:
  // expose regular call operator to provide usual chain()
  template <typename... Containers>
  auto operator()(Containers&&... cs) const {
    return chain_impl(
        std::tuple<Containers...>{std::forward<Containers>(cs)...},
        std::index_sequence_for<Containers...>{});
  }

  ChainFromIterableFn from_iterable;
};

namespace iter {
  namespace {
    constexpr auto chain = iter::impl::ChainMaker{};
  }
}

#endif
#ifndef ITER_CHUNKED_HPP_
#define ITER_CHUNKED_HPP_



namespace iter {
  namespace impl {
    template <typename Container>
    class Chunker;

    using ChunkedFn = IterToolFnBindSizeTSecond<Chunker>;
  }
  constexpr impl::ChunkedFn chunked{};
}

template <typename Container>
class iter::impl::Chunker {
 private:
  Container container_;
  std::size_t chunk_size_;

  Chunker(Container&& container, std::size_t sz)
      : container_(std::forward<Container>(container)), chunk_size_{sz} {}

  friend ChunkedFn;

  template <typename T>
  using IndexVector = std::vector<IteratorWrapper<T>>;
  template <typename T>
  using DerefVec = IterIterWrapper<IndexVector<T>>;

 public:
  Chunker(Chunker&&) = default;
  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    std::shared_ptr<DerefVec<ContainerT>> chunk_ =
        std::make_shared<DerefVec<ContainerT>>();
    IteratorWrapper<ContainerT> sub_iter_;
    IteratorWrapper<ContainerT> sub_end_;
    std::size_t chunk_size_ = 0;

    bool done() const {
      return chunk_->empty();
    }

    void refill_chunk() {
      chunk_->get().clear();
      std::size_t i{0};
      while (i < chunk_size_ && sub_iter_ != sub_end_) {
        chunk_->get().push_back(sub_iter_);
        ++sub_iter_;
        ++i;
      }
    }

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = DerefVec<ContainerT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(IteratorWrapper<ContainerT>&& sub_iter,
        IteratorWrapper<ContainerT>&& sub_end, std::size_t s)
        : sub_iter_{std::move(sub_iter)},
          sub_end_{std::move(sub_end)},
          chunk_size_{s} {
      chunk_->get().reserve(chunk_size_);
      refill_chunk();
    }

    Iterator& operator++() {
      refill_chunk();
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      return !(*this == other);
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return done() == other.done()
             && (done() || !(sub_iter_ != other.sub_iter_));
    }

    DerefVec<ContainerT>& operator*() {
      return *chunk_;
    }

    DerefVec<ContainerT>* operator->() {
      return chunk_.get();
    }
  };

  Iterator<Container> begin() {
    return {get_begin(container_), get_end(container_), chunk_size_};
  }

  Iterator<Container> end() {
    return {get_end(container_), get_end(container_), chunk_size_};
  }

  Iterator<AsConst<Container>> begin() const {
    return {get_begin(std::as_const(container_)),
        get_end(std::as_const(container_)), chunk_size_};
  }

  Iterator<AsConst<Container>> end() const {
    return {get_end(std::as_const(container_)),
        get_end(std::as_const(container_)), chunk_size_};
  }
};

#endif
#ifndef ITER_COMBINATIONS_HPP_
#define ITER_COMBINATIONS_HPP_



namespace iter {
  namespace impl {
    template <typename Container>
    class Combinator;

    using CombinationsFn = IterToolFnBindSizeTSecond<Combinator>;
  }
  constexpr impl::CombinationsFn combinations{};
}

template <typename Container>
class iter::impl::Combinator {
 private:
  Container container_;
  std::size_t length_;

  friend CombinationsFn;

  Combinator(Container&& container, std::size_t length)
      : container_(std::forward<Container>(container)), length_{length} {}

  template <typename T>
  using IndexVector = std::vector<iterator_type<T>>;
  template <typename T>
  using CombIteratorDeref = IterIterWrapper<IndexVector<T>>;

 public:
  Combinator(Combinator&&) = default;
  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    constexpr static const int COMPLETE = -1;
    std::remove_reference_t<ContainerT>* container_p_;
    CombIteratorDeref<ContainerT> indices_;
    int steps_{};

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = CombIteratorDeref<ContainerT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(ContainerT& container, std::size_t n)
        : container_p_{&container}, indices_{n} {
      if (n == 0) {
        steps_ = COMPLETE;
        return;
      }
      size_t inc = 0;
      for (auto& iter : indices_.get()) {
        auto it = get_begin(*container_p_);
        dumb_advance(it, get_end(*container_p_), inc);
        if (it != get_end(*container_p_)) {
          iter = it;
          ++inc;
        } else {
          steps_ = COMPLETE;
          break;
        }
      }
    }

    CombIteratorDeref<ContainerT>& operator*() {
      return indices_;
    }

    CombIteratorDeref<ContainerT>* operator->() {
      return &indices_;
    }

    Iterator& operator++() {
      for (auto iter = indices_.get().rbegin(); iter != indices_.get().rend();
           ++iter) {
        ++(*iter);

        // what we have to check here is if the distance between
        // the index and the end of indices_ is >= the distance
        // between the item and end of item
        auto dist = std::distance(indices_.get().rbegin(), iter);

        if (!(dumb_next(*iter, dist) != get_end(*container_p_))) {
          if ((iter + 1) != indices_.get().rend()) {
            size_t inc = 1;
            for (auto down = iter; ; --down) {
              (*down) = dumb_next(*(iter + 1), 1 + inc);
              ++inc;
              if (down == indices_.get().rbegin())
                break;
            }
          } else {
            steps_ = COMPLETE;
            break;
          }
        } else {
          break;
        }
        // we break because none of the rest of the items need
        // to be incremented
      }
      if (steps_ != COMPLETE) {
        ++steps_;
      }
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      return !(*this == other);
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return steps_ == other.steps_;
    }
  };

  Iterator<Container> begin() {
    return {container_, length_};
  }

  Iterator<Container> end() {
    return {container_, 0};
  }

  Iterator<AsConst<Container>> begin() const {
    return {std::as_const(container_), length_};
  }

  Iterator<AsConst<Container>> end() const {
    return {std::as_const(container_), 0};
  }
};

#endif
#ifndef ITER_COMBINATIONS_WITH_REPLACEMENT_HPP_
#define ITER_COMBINATIONS_WITH_REPLACEMENT_HPP_



namespace iter {
  namespace impl {
    template <typename Container>
    class CombinatorWithReplacement;
    using CombinationsWithReplacementFn =
        IterToolFnBindSizeTSecond<CombinatorWithReplacement>;
  }
  constexpr impl::CombinationsWithReplacementFn combinations_with_replacement{};
}

template <typename Container>
class iter::impl::CombinatorWithReplacement {
 private:
  Container container_;
  std::size_t length_;

  friend CombinationsWithReplacementFn;

  CombinatorWithReplacement(Container&& container, std::size_t n)
      : container_(std::forward<Container>(container)), length_{n} {}

  template <typename T>
  using IndexVector = std::vector<iterator_type<T>>;
  template <typename T>
  using CombIteratorDeref = IterIterWrapper<IndexVector<T>>;

 public:
  CombinatorWithReplacement(CombinatorWithReplacement&&) = default;
  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    constexpr static const int COMPLETE = -1;
    std::remove_reference_t<ContainerT>* container_p_;
    CombIteratorDeref<ContainerT> indices_;
    int steps_;

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = CombIteratorDeref<ContainerT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(ContainerT& in_container, std::size_t n)
        : container_p_{&in_container},
          indices_(n, get_begin(in_container)),
          steps_{(get_begin(in_container) != get_end(in_container) && n)
                     ? 0
                     : COMPLETE} {}

    CombIteratorDeref<ContainerT>& operator*() {
      return indices_;
    }

    CombIteratorDeref<ContainerT>* operator->() {
      return &indices_;
    }

    Iterator& operator++() {
      for (auto iter = indices_.get().rbegin(); iter != indices_.get().rend();
           ++iter) {
        ++(*iter);
        if (!(*iter != get_end(*container_p_))) {
          if ((iter + 1) != indices_.get().rend()) {
            for (auto down = iter; ; --down) {
              (*down) = dumb_next(*(iter + 1));
              if (down == indices_.get().rbegin())
                break;
            }
          } else {
            steps_ = COMPLETE;
            break;
          }
        } else {
          // we break because none of the rest of the items
          // need to be incremented
          break;
        }
      }
      if (steps_ != COMPLETE) {
        ++steps_;
      }
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      return !(*this == other);
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return steps_ == other.steps_;
    }
  };

  Iterator<Container> begin() {
    return {container_, length_};
  }

  Iterator<Container> end() {
    return {container_, 0};
  }

  Iterator<AsConst<Container>> begin() const {
    return {std::as_const(container_), length_};
  }

  Iterator<AsConst<Container>> end() const {
    return {std::as_const(container_), 0};
  }
};

#endif
#ifndef ITER_COMPRESS_H_
#define ITER_COMPRESS_H_



namespace iter {
  namespace impl {
    template <typename Container, typename Selector>
    class Compressed;
  }

  template <typename Container, typename Selector>
  impl::Compressed<Container, Selector> compress(Container&&, Selector&&);
}

template <typename Container, typename Selector>
class iter::impl::Compressed {
 private:
  Container container_;
  Selector selectors_;

  friend Compressed iter::compress<Container, Selector>(
      Container&&, Selector&&);

  Compressed(Container&& in_container, Selector&& in_selectors)
      : container_(std::forward<Container>(in_container)),
        selectors_(std::forward<Selector>(in_selectors)) {}

 public:
  Compressed(Compressed&&) = default;
  template <typename ContainerT, typename SelectorT>
  class Iterator {
   private:
    template <typename, typename>
    friend class Iterator;
    IteratorWrapper<ContainerT> sub_iter_;
    IteratorWrapper<ContainerT> sub_end_;

    IteratorWrapper<SelectorT> selector_iter_;
    IteratorWrapper<SelectorT> selector_end_;

    void increment_iterators() {
      ++sub_iter_;
      ++selector_iter_;
    }

    void skip_failures() {
      while (sub_iter_ != sub_end_ && selector_iter_ != selector_end_
             && !*selector_iter_) {
        increment_iterators();
      }
    }

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = iterator_traits_deref<ContainerT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(IteratorWrapper<ContainerT>&& cont_iter,
        IteratorWrapper<ContainerT>&& cont_end,
        IteratorWrapper<SelectorT>&& sel_iter,
        IteratorWrapper<SelectorT>&& sel_end)
        : sub_iter_{std::move(cont_iter)},
          sub_end_{std::move(cont_end)},
          selector_iter_{std::move(sel_iter)},
          selector_end_{std::move(sel_end)} {
      skip_failures();
    }

    iterator_deref<ContainerT> operator*() {
      return *sub_iter_;
    }

    iterator_arrow<ContainerT> operator->() {
      return apply_arrow(sub_iter_);
    }

    Iterator& operator++() {
      increment_iterators();
      skip_failures();
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T, typename U>
    bool operator!=(const Iterator<T, U>& other) const {
      return sub_iter_ != other.sub_iter_
             && selector_iter_ != other.selector_iter_;
    }

    template <typename T, typename U>
    bool operator==(const Iterator<T, U>& other) const {
      return !(*this != other);
    }
  };

  Iterator<Container, Selector> begin() {
    return {get_begin(container_), get_end(container_), get_begin(selectors_),
        get_end(selectors_)};
  }

  Iterator<Container, Selector> end() {
    return {get_end(container_), get_end(container_), get_end(selectors_),
        get_end(selectors_)};
  }

  Iterator<AsConst<Container>, AsConst<Selector>> begin() const {
    return {get_begin(std::as_const(container_)),
        get_end(std::as_const(container_)),
        get_begin(std::as_const(selectors_)),
        get_end(std::as_const(selectors_))};
  }

  Iterator<AsConst<Container>, AsConst<Selector>> end() const {
    return {get_end(std::as_const(container_)),
        get_end(std::as_const(container_)),
        get_end(std::as_const(selectors_)),
        get_end(std::as_const(selectors_))};
  }
};

template <typename Container, typename Selector>
iter::impl::Compressed<Container, Selector> iter::compress(
    Container&& container_, Selector&& selectors_) {
  return {
      std::forward<Container>(container_), std::forward<Selector>(selectors_)};
}

#endif
#ifndef ITER_COUNT_H_
#define ITER_COUNT_H_


#include <limits>

namespace iter {
  template <typename T>
  constexpr auto count(T start, T step) noexcept {
    T stop = step < T(0) ? std::numeric_limits<T>::lowest()
                         : std::numeric_limits<T>::max();
    return range(start, stop, step);
  }

  template <typename T = long>
  constexpr auto count(T start = T(0)) noexcept {
    return count(start, T(1));
  }
}

#endif
#ifndef ITER_CYCLE_H_
#define ITER_CYCLE_H_



namespace iter {
  namespace impl {
    template <typename Container>
    class Cycler;

    using CycleFn = IterToolFn<Cycler>;
  }
  constexpr impl::CycleFn cycle{};
}

template <typename Container>
class iter::impl::Cycler {
 private:
  friend CycleFn;

  Container container_;

  Cycler(Container&& container)
      : container_(std::forward<Container>(container)) {}

 public:
  Cycler(Cycler&&) = default;
  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    IteratorWrapper<ContainerT> sub_iter_;
    IteratorWrapper<ContainerT> sub_begin_;
    IteratorWrapper<ContainerT> sub_end_;

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = iterator_traits_deref<ContainerT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(IteratorWrapper<ContainerT>&& sub_iter,
        IteratorWrapper<ContainerT>&& sub_end)
        : sub_iter_{sub_iter},
          sub_begin_{sub_iter},
          sub_end_{std::move(sub_end)} {}

    iterator_deref<ContainerT> operator*() {
      return *sub_iter_;
    }

    iterator_arrow<ContainerT> operator->() {
      return apply_arrow(sub_iter_);
    }

    Iterator& operator++() {
      ++sub_iter_;
      // reset to beginning upon reaching the sub_end_
      if (!(sub_iter_ != sub_end_)) {
        sub_iter_ = sub_begin_;
      }
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      return sub_iter_ != other.sub_iter_;
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return !(*this != other);
    }
  };

  Iterator<Container> begin() {
    return {get_begin(container_), get_end(container_)};
  }

  Iterator<Container> end() {
    return {get_end(container_), get_end(container_)};
  }

  Iterator<AsConst<Container>> begin() const {
    return {get_begin(std::as_const(container_)),
        get_end(std::as_const(container_))};
  }

  Iterator<AsConst<Container>> end() const {
    return {get_end(std::as_const(container_)),
        get_end(std::as_const(container_))};
  }
};

#endif
#ifndef ITER_DROPWHILE_H_
#define ITER_DROPWHILE_H_



namespace iter {
  namespace impl {
    template <typename FilterFunc, typename Container>
    class Dropper;

    using DropWhileFn = IterToolFnOptionalBindFirst<Dropper, BoolTester>;
  }
  constexpr impl::DropWhileFn dropwhile{};
}

template <typename FilterFunc, typename Container>
class iter::impl::Dropper {
 private:
  Container container_;
  mutable FilterFunc filter_func_;

  friend DropWhileFn;

  Dropper(FilterFunc filter_func, Container&& container)
      : container_(std::forward<Container>(container)),
        filter_func_(filter_func) {}

 public:
  Dropper(Dropper&&) = default;
  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    using Holder = DerefHolder<iterator_deref<ContainerT>>;
    mutable IteratorWrapper<ContainerT> sub_iter_;
    IteratorWrapper<ContainerT> sub_end_;
    mutable Holder item_;
    FilterFunc* filter_func_;

    // see comments from filter about mutability
    void inc_sub_iter() const {
      ++sub_iter_;
      if (sub_iter_ != sub_end_) {
        item_.reset(*sub_iter_);
      }
    }

    // skip all values for which the predicate is true
    void skip_passes() const {
      while (sub_iter_ != sub_end_ && std::invoke(*filter_func_, item_.get())) {
        inc_sub_iter();
      }
    }

    void init_if_first_use() const {
      if (!item_ && sub_iter_ != sub_end_) {
        item_.reset(*sub_iter_);
        skip_passes();
      }
    }

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = iterator_traits_deref<ContainerT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(IteratorWrapper<ContainerT>&& sub_iter,
        IteratorWrapper<ContainerT>&& sub_end, FilterFunc& filter_func)
        : sub_iter_{std::move(sub_iter)},
          sub_end_{std::move(sub_end)},
          filter_func_(&filter_func) {}

    typename Holder::reference operator*() {
      init_if_first_use();
      return item_.get();
    }

    typename Holder::pointer operator->() {
      init_if_first_use();
      return item_.get_ptr();
    }

    Iterator& operator++() {
      init_if_first_use();
      inc_sub_iter();
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      init_if_first_use();
      other.init_if_first_use();
      return sub_iter_ != other.sub_iter_;
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return !(*this != other);
    }
  };

  Iterator<Container> begin() {
    return {get_begin(container_), get_end(container_), filter_func_};
  }

  Iterator<Container> end() {
    return {get_end(container_), get_end(container_), filter_func_};
  }

  Iterator<AsConst<Container>> begin() const {
    return {get_begin(std::as_const(container_)),
        get_end(std::as_const(container_)), filter_func_};
  }

  Iterator<AsConst<Container>> end() const {
    return {get_end(std::as_const(container_)),
        get_end(std::as_const(container_)), filter_func_};
  }
};

#endif
#ifndef ITER_ENUMERATE_H_
#define ITER_ENUMERATE_H_



namespace iter {
  namespace impl {
    template <typename Index, typename Elem>
    using EnumBasePair = std::pair<Index, Elem>;

    // "yielded" by the Enumerable::Iterator.  Has a .index, and a
    // .element referencing the value yielded by the subiterator
    template <typename Index, typename Elem>
    class EnumIterYield : public EnumBasePair<Index, Elem> {
      using BasePair = EnumBasePair<Index, Elem>;
      using BasePair::BasePair;

     public:
      typename BasePair::first_type index = BasePair::first;
      typename BasePair::second_type element = BasePair::second;
    };

    template <typename Container, typename Index>
    class Enumerable;

    using EnumerateFn = IterToolFnOptionalBindSecond<Enumerable, std::size_t>;
  }
  constexpr impl::EnumerateFn enumerate{};
}

namespace std {
  template <typename Index, typename Elem>
  class tuple_size<iter::impl::EnumIterYield<Index, Elem>>
      : public tuple_size<iter::impl::EnumBasePair<Index, Elem>> {};

  template <std::size_t N, typename Index, typename Elem>
  class tuple_element<N, iter::impl::EnumIterYield<Index, Elem>>
      : public tuple_element<N, iter::impl::EnumBasePair<Index, Elem>> {};
}

template <typename Container, typename Index>
class iter::impl::Enumerable {
 private:
  Container container_;
  const Index start_;

  friend EnumerateFn;

  // Value constructor for use only in the enumerate function
  Enumerable(Container&& container, Index start)
      : container_(std::forward<Container>(container)), start_{start} {}

 public:
  Enumerable(Enumerable&&) = default;

  template <typename T>
  using IterYield = EnumIterYield<Index, iterator_deref<T>>;

  //  Holds an iterator of the contained type and an Index for the
  //  index_.  Each call to ++ increments both of these data members.
  //  Each dereference returns an IterYield.
  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    IteratorWrapper<ContainerT> sub_iter_;
    Index index_;

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = IterYield<ContainerT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(IteratorWrapper<ContainerT>&& sub_iter, Index start)
        : sub_iter_{std::move(sub_iter)}, index_{start} {}

    IterYield<ContainerT> operator*() {
      return {index_, *sub_iter_};
    }

    ArrowProxy<IterYield<ContainerT>> operator->() {
      return {**this};
    }

    Iterator& operator++() {
      ++sub_iter_;
      ++index_;
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      return sub_iter_ != other.sub_iter_;
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return !(*this != other);
    }
  };

  Iterator<Container> begin() {
    return {get_begin(container_), start_};
  }

  Iterator<Container> end() {
    return {get_end(container_), start_};
  }

  Iterator<AsConst<Container>> begin() const {
    return {get_begin(std::as_const(container_)), start_};
  }

  Iterator<AsConst<Container>> end() const {
    return {get_end(std::as_const(container_)), start_};
  }
};
#endif
#ifndef ITER_FILTER_FALSE_HPP_
#define ITER_FILTER_FALSE_HPP_



namespace iter {
  namespace impl {
    // Callable object that reverses the boolean result of another
    // callable, taking the object in a Container's iterator
    template <typename FilterFunc>
    class PredicateFlipper {
     private:
      FilterFunc filter_func_;

     public:
      PredicateFlipper(FilterFunc filter_func)
          : filter_func_(std::move(filter_func)) {}

      // Calls the filter_func_
      template <typename T>
      bool operator()(const T& item) const {
        return !bool(std::invoke(filter_func_, item));
      }

      // with non-const incase FilterFunc::operator() is non-const
      template <typename T>
      bool operator()(const T& item) {
        return !bool(std::invoke(filter_func_, item));
      }
    };

    template <typename FilterFunc, typename Container>
    class FilterFalsed;

    using FilterFalseFn = IterToolFnOptionalBindFirst<FilterFalsed, BoolTester>;
  }
  constexpr impl::FilterFalseFn filterfalse{};
}

// Delegates to Filtered with PredicateFlipper<FilterFunc>
template <typename FilterFunc, typename Container>
class iter::impl::FilterFalsed
    : public Filtered<PredicateFlipper<FilterFunc>, Container> {
  friend FilterFalseFn;
  FilterFalsed(FilterFunc in_filter_func, Container&& in_container)
      : Filtered<PredicateFlipper<FilterFunc>, Container>(
            {in_filter_func}, std::forward<Container>(in_container)) {}
};

#endif
#ifndef ITER_GROUP_BY_HPP_
#define ITER_GROUP_BY_HPP_

// this is easily the most functionally complex itertool



namespace iter {
  namespace impl {
    template <typename Container, typename KeyFunc>
    class GroupProducer;

    struct Identity {
      template <typename T>
      const T& operator()(const T& t) const {
        return t;
      }
    };

    using GroupByFn = IterToolFnOptionalBindSecond<GroupProducer, Identity>;
  }
  constexpr impl::GroupByFn groupby{};
}

template <typename Container, typename KeyFunc>
class iter::impl::GroupProducer {
 private:
  Container container_;
  mutable KeyFunc key_func_;

  friend GroupByFn;

  template <typename T>
  using key_func_ret = std::invoke_result_t<KeyFunc, iterator_deref<T>>;

  GroupProducer(Container&& container, KeyFunc key_func)
      : container_(std::forward<Container>(container)), key_func_(key_func) {}

 public:
  GroupProducer(GroupProducer&&) = default;

  template <typename T>
  class Iterator;
  template <typename T>
  class Group;

 private:
  template <typename T>
  using KeyGroupPair = std::pair<key_func_ret<T>, Group<T>>;
  template <typename T>
  using Holder = DerefHolder<iterator_deref<T>>;

 public:
  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    IteratorWrapper<ContainerT> sub_iter_;
    IteratorWrapper<ContainerT> sub_end_;
    Holder<ContainerT> item_;
    KeyFunc* key_func_;
    std::optional<KeyGroupPair<ContainerT>> current_key_group_pair_;

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = KeyGroupPair<ContainerT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(IteratorWrapper<ContainerT>&& sub_iter,
        IteratorWrapper<ContainerT>&& sub_end, KeyFunc& key_func)
        : sub_iter_{std::move(sub_iter)},
          sub_end_{std::move(sub_end)},
          key_func_(&key_func) {
      if (sub_iter_ != sub_end_) {
        item_.reset(*sub_iter_);
      }
    }

    Iterator(const Iterator& other)
        : sub_iter_{other.sub_iter_},
          sub_end_{other.sub_end_},
          item_{other.item_},
          key_func_{other.key_func_} {}

    Iterator& operator=(const Iterator& other) {
      if (this == &other) {
        return *this;
      }
      sub_iter_ = other.sub_iter_;
      sub_end_ = other.sub_end_;
      item_ = other.item_;
      key_func_ = other.key_func_;
      current_key_group_pair_.reset();
      return *this;
    }

    ~Iterator() = default;

    // NOTE the implicitly generated move constructor would
    // be wrong

    KeyGroupPair<ContainerT>& operator*() {
      set_key_group_pair();
      return *current_key_group_pair_;
    }

    KeyGroupPair<ContainerT>* operator->() {
      set_key_group_pair();
      return &*current_key_group_pair_;
    }

    Iterator& operator++() {
      if (!current_key_group_pair_) {
        set_key_group_pair();
      }
      current_key_group_pair_.reset();
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      return sub_iter_ != other.sub_iter_;
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return !(*this != other);
    }

    void increment_iterator() {
      if (sub_iter_ != sub_end_) {
        ++sub_iter_;
        if (sub_iter_ != sub_end_) {
          item_.reset(*sub_iter_);
        }
      }
    }

    bool exhausted() const {
      return !(sub_iter_ != sub_end_);
    }

    typename Holder<ContainerT>::reference get() {
      return item_.get();
    }

    typename Holder<ContainerT>::pointer get_ptr() {
      return item_.get_ptr();
    }

    key_func_ret<ContainerT> next_key() {
      return std::invoke(*key_func_, item_.get());
    }

    void set_key_group_pair() {
      if (!current_key_group_pair_) {
        current_key_group_pair_.emplace(std::invoke(*key_func_, item_.get()),
            Group<ContainerT>{*this, next_key()});
      }
    }
  };

  template <typename ContainerT>
  class Group {
   private:
    template <typename>
    friend class Iterator;
    friend class GroupIterator;
    Iterator<ContainerT>& owner_;
    key_func_ret<ContainerT> key_;

    // completed is set if a Group is iterated through
    // completely.  It is checked in the destructor, and
    // if the Group has not been completed, the destructor
    // exhausts it.  This ensures that the next Group starts
    // at the correct position when the user short-circuits
    // iteration over a Group.
    // The move constructor sets the rvalue's completed
    // attribute to true, so its destructor doesn't do anything
    // when called.
    bool completed = false;

    Group(Iterator<ContainerT>& owner, key_func_ret<ContainerT> key)
        : owner_(owner), key_(key) {}

   public:
    ~Group() {
      if (!completed) {
        for (auto iter = begin(), end_it = end(); iter != end_it; ++iter) {
        }
      }
    }

    // move-constructible, non-copy-constructible, non-assignable
    Group(Group&& other) noexcept
        : owner_(other.owner_), key_{other.key_}, completed{other.completed} {
      other.completed = true;
    }

    class GroupIterator {
     private:
      std::remove_reference_t<key_func_ret<ContainerT>>* key_;
      Group* group_p_;

      bool not_at_end() {
        return !group_p_->owner_.exhausted()
               && group_p_->owner_.next_key() == *key_;
      }

     public:
      using iterator_category = std::input_iterator_tag;
      using value_type = iterator_traits_deref<ContainerT>;
      using difference_type = std::ptrdiff_t;
      using pointer = value_type*;
      using reference = value_type&;

      // TODO template this? idk if it's relevant here
      GroupIterator(Group* group_p, key_func_ret<ContainerT>& key)
          : key_{&key}, group_p_{group_p} {}

      bool operator!=(const GroupIterator& other) const {
        return !(*this == other);
      }

      bool operator==(const GroupIterator& other) const {
        return group_p_ == other.group_p_;
      }

      GroupIterator& operator++() {
        group_p_->owner_.increment_iterator();
        if (!not_at_end()) {
          group_p_->completed = true;
          group_p_ = nullptr;
        }
        return *this;
      }

      GroupIterator operator++(int) {
        auto ret = *this;
        ++*this;
        return ret;
      }

      iterator_deref<ContainerT> operator*() {
        return group_p_->owner_.get();
      }

      typename Holder<ContainerT>::pointer operator->() {
        return group_p_->owner_.get_ptr();
      }
    };

    GroupIterator begin() {
      return {this, key_};
    }

    GroupIterator end() {
      return {nullptr, key_};
    }
  };

  Iterator<Container> begin() {
    return {get_begin(container_), get_end(container_), key_func_};
  }

  Iterator<Container> end() {
    return {get_end(container_), get_end(container_), key_func_};
  }

  Iterator<AsConst<Container>> begin() const {
    return {get_begin(std::as_const(container_)),
        get_end(std::as_const(container_)), key_func_};
  }

  Iterator<AsConst<Container>> end() const {
    return {get_end(std::as_const(container_)),
        get_end(std::as_const(container_)), key_func_};
  }
};

#endif
#ifndef ITER_IMAP_H_
#define ITER_IMAP_H_



namespace iter {
  namespace impl {
    struct IMapFn : PipeableAndBindFirst<IMapFn> {
      template <typename MapFunc, typename... Containers>
      auto operator()(MapFunc map_func, Containers&&... containers) const
          // explicitly specifying type here to allow more expressions that only
          // care about the type, and don't need a valid implementation.
          // See #66
          -> StarMapper<MapFunc,
              decltype(zip(std::forward<Containers>(containers)...))> {
        return starmap(map_func, zip(std::forward<Containers>(containers)...));
      }
      using PipeableAndBindFirst<IMapFn>::operator();
    };
  }
  constexpr impl::IMapFn imap{};
}

#endif
#ifndef ITERTOOLS_ALL_HPP_
#define ITERTOOLS_ALL_HPP_


// zip_longest is the only itertool with a boost depedency, so it must be
// included explicitly

#endif
#ifndef ITER_PERMUTATIONS_HPP_
#define ITER_PERMUTATIONS_HPP_



namespace iter {
  namespace impl {
    template <typename Container>
    class Permuter;
    using PermutationsFn = IterToolFn<Permuter>;
  }
  constexpr impl::PermutationsFn permutations{};
}

template <typename Container>
class iter::impl::Permuter {
 private:
  friend PermutationsFn;
  Container container_;

  template <typename T>
  using IndexVector = std::vector<IteratorWrapper<T>>;
  template <typename T>
  using Permutable = IterIterWrapper<IndexVector<T>>;

  Permuter(Container&& container)
      : container_(std::forward<Container>(container)) {}

 public:
  Permuter(Permuter&&) = default;

  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    static constexpr const int COMPLETE = -1;
    static bool cmp_iters(IteratorWrapper<ContainerT> lhs,
        IteratorWrapper<ContainerT> rhs) noexcept {
      return *lhs < *rhs;
    }

    Permutable<ContainerT> working_set_;
    int steps_{};

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = Permutable<ContainerT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(IteratorWrapper<ContainerT>&& sub_iter,
        IteratorWrapper<ContainerT>&& sub_end)
        : steps_{sub_iter != sub_end ? 0 : COMPLETE} {
      // done like this instead of using vector ctor with
      // two iterators because that causes a substitution
      // failure when the iterator is minimal
      while (sub_iter != sub_end) {
        working_set_.get().push_back(sub_iter);
        ++sub_iter;
      }
      std::sort(get_begin(working_set_.get()), get_end(working_set_.get()),
          cmp_iters);
    }

    Permutable<ContainerT>& operator*() {
      return working_set_;
    }

    Permutable<ContainerT>* operator->() {
      return &working_set_;
    }

    Iterator& operator++() {
      ++steps_;
      if (!std::next_permutation(get_begin(working_set_.get()),
              get_end(working_set_.get()), cmp_iters)) {
        steps_ = COMPLETE;
      }
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      return !(*this == other);
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return steps_ == other.steps_;
    }
  };

  Iterator<Container> begin() {
    return {get_begin(container_), get_end(container_)};
  }

  Iterator<Container> end() {
    return {get_end(container_), get_end(container_)};
  }

  Iterator<AsConst<Container>> begin() const {
    return {get_begin(std::as_const(container_)),
        get_end(std::as_const(container_))};
  }

  Iterator<AsConst<Container>> end() const {
    return {get_end(std::as_const(container_)),
        get_end(std::as_const(container_))};
  }
};

#endif
#ifndef ITER_POWERSET_HPP_
#define ITER_POWERSET_HPP_



namespace iter {
  namespace impl {
    template <typename Container>
    class Powersetter;

    using PowersetFn = IterToolFn<Powersetter>;
  }
  constexpr impl::PowersetFn powerset{};
}

template <typename Container>
class iter::impl::Powersetter {
 private:
  Container container_;
  template <typename T>
  using CombinatorType = decltype(combinations(std::declval<T&>(), 0));

  friend PowersetFn;

  Powersetter(Container&& container)
      : container_(std::forward<Container>(container)) {}

 public:
  Powersetter(Powersetter&&) = default;

  template <typename ContainerT>
  class Iterator {
   private:
#if 0
    template <typename> friend class Iterator;
#endif
    std::remove_reference_t<ContainerT>* container_p_;
    std::size_t set_size_{};
    std::shared_ptr<CombinatorType<ContainerT>> comb_;
    iterator_type<CombinatorType<ContainerT>> comb_iter_;
    iterator_type<CombinatorType<ContainerT>> comb_end_;

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = CombinatorType<ContainerT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(ContainerT& container, std::size_t sz)
        : container_p_{&container},
          set_size_{sz},
          comb_{std::make_shared<CombinatorType<ContainerT>>(
              combinations(container, sz))},
          comb_iter_{get_begin(*comb_)},
          comb_end_{get_end(*comb_)} {}

    Iterator& operator++() {
      ++comb_iter_;
      if (comb_iter_ == comb_end_) {
        ++set_size_;
        comb_ = std::make_shared<CombinatorType<ContainerT>>(
            combinations(*container_p_, set_size_));

        comb_iter_ = get_begin(*comb_);
        comb_end_ = get_end(*comb_);
      }
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    iterator_deref<CombinatorType<ContainerT>> operator*() {
      return *comb_iter_;
    }

    iterator_arrow<CombinatorType<ContainerT>> operator->() {
      apply_arrow(comb_iter_);
    }

    bool operator!=(const Iterator& other) const {
      return !(*this == other);
    }

    bool operator==(const Iterator& other) const {
      return set_size_ == other.set_size_ && comb_iter_ == other.comb_iter_;
    }
#if 0
    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      return !(*this == other);
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return set_size_ == other.set_size_ && comb_iter_ == other.comb_iter_;
    }
#endif
  };

  Iterator<Container> begin() {
    return {container_, 0};
  }

  Iterator<Container> end() {
    return {container_, dumb_size(container_) + 1};
  }

  Iterator<AsConst<Container>> begin() const {
    return {std::as_const(container_), 0};
  }

  Iterator<AsConst<Container>> end() const {
    return {
        std::as_const(container_), dumb_size(std::as_const(container_)) + 1};
  }
};

#endif
#ifndef ITER_PRODUCT_HPP_
#define ITER_PRODUCT_HPP_



namespace iter {
  namespace impl {
    template <typename TupleType, std::size_t... Is>
    class Productor;

    template <typename TupleType, std::size_t... Is>
    Productor<TupleType, Is...> product_impl(
        TupleType&& containers, std::index_sequence<Is...>);
  }
}

template <typename TupleType, std::size_t... Is>
class iter::impl::Productor {
  friend Productor iter::impl::product_impl<TupleType, Is...>(
      TupleType&&, std::index_sequence<Is...>);

 private:
  TupleType containers_;

  Productor(TupleType&& containers) : containers_(std::move(containers)) {}

 public:
  Productor(Productor&&) = default;

 private:
  template <typename IterTupType>
  class IteratorData {
    IteratorData() = delete;
    static_assert(
        std::tuple_size<std::decay_t<IterTupType>>::value == sizeof...(Is),
        "tuple size != sizeof Is");

   public:
    template <std::size_t Idx>
    static bool equal(const IterTupType& lhs, const IterTupType& rhs) {
      return !(std::get<Idx>(lhs) != std::get<Idx>(rhs));
    }

    // returns true if incremented, false if wrapped around
    template <std::size_t Idx>
    static bool get_and_increment_with_wraparound(IterTupType& iters,
        const IterTupType& begin_iters, const IterTupType& end_iters) {
      // if already at the end, we're looking at an empty container
      if (equal<Idx>(iters, end_iters)) {
        return false;
      }

      ++std::get<Idx>(iters);

      if (equal<Idx>(iters, end_iters)) {
        std::get<Idx>(iters) = std::get<Idx>(begin_iters);
        return false;
      }

      return true;
    }
    using IncFunc = bool (*)(
        IterTupType&, const IterTupType&, const IterTupType&);

    constexpr static std::array<IncFunc, sizeof...(Is)> incrementers{
        {get_and_increment_with_wraparound<Is>...}};
  };

  // template templates here because I need to defer evaluation in the const
  // iteration case for types that don't have non-const begin() and end(). If I
  // passed in the actual types of the tuples of iterators and the type for
  // deref they'd need to be known in the function declarations below.
  template <typename TupleTypeT, template <typename> class IteratorTuple,
      template <typename> class TupleDeref>
  class IteratorTempl {
#if NO_GCC_FRIEND_ERROR
   private:
    template <typename, template <typename> class, template <typename> class>
    friend class IteratorTempl;
#else
   public:
#endif

    using IterTupType = IteratorTuple<TupleTypeT>;
    IterTupType iters_;
    IterTupType begin_iters_;
    IterTupType end_iters_;

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = TupleDeref<TupleTypeT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    IteratorTempl(IteratorTuple<TupleTypeT>&& iters,
        IteratorTuple<TupleTypeT>&& end_iters)
        : iters_(std::move(iters)),
          begin_iters_(iters_),
          end_iters_(std::move(end_iters)) {}

    IteratorTempl& operator++() {
      static constexpr int NUM_ELEMENTS = sizeof...(Is);
      bool performed_increment = false;
      for (int i = NUM_ELEMENTS - 1; i >= 0; --i) {
        if (IteratorData<IterTupType>::incrementers[i](
                iters_, begin_iters_, end_iters_)) {
          performed_increment = true;
          break;
        }
      }
      if (!performed_increment) {
        iters_ = end_iters_;
      }
      return *this;
    }

    IteratorTempl operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T, template <typename> class IT,
        template <typename> class TD>
    bool operator!=(const IteratorTempl<T, IT, TD>& other) const {
      if constexpr (sizeof...(Is) == 0) {
        return false;
      } else {
        return (... && (std::get<Is>(iters_) != std::get<Is>(other.iters_)));
      }
    }

    template <typename T, template <typename> class IT,
        template <typename> class TD>
    bool operator==(const IteratorTempl<T, IT, TD>& other) const {
      return !(*this != other);
    }

    TupleDeref<TupleTypeT> operator*() {
      return {(*std::get<Is>(iters_))...};
    }

    auto operator-> () -> ArrowProxy<decltype(**this)> {
      return {**this};
    }
  };

  using Iterator =
      IteratorTempl<TupleType, iterator_tuple_type, iterator_deref_tuple>;
  using ConstIterator = IteratorTempl<AsConst<TupleType>,
      const_iterator_tuple_type, const_iterator_deref_tuple>;

 public:
  Iterator begin() {
    return {{get_begin(std::get<Is>(containers_))...},
        {get_end(std::get<Is>(containers_))...}};
  }

  Iterator end() {
    return {{get_end(std::get<Is>(containers_))...},
        {get_end(std::get<Is>(containers_))...}};
  }

  ConstIterator begin() const {
    return {{get_begin(std::as_const(std::get<Is>(containers_)))...},
        {get_end(std::as_const(std::get<Is>(containers_)))...}};
  }

  ConstIterator end() const {
    return {{get_end(std::as_const(std::get<Is>(containers_)))...},
        {get_end(std::as_const(std::get<Is>(containers_)))...}};
  }
};

namespace iter::impl {
  template <typename TupleType, std::size_t... Is>
  Productor<TupleType, Is...> product_impl(
      TupleType&& containers, std::index_sequence<Is...>) {
    return {std::move(containers)};
  }
}

namespace iter {
  template <typename... Containers>
  decltype(auto) product(Containers&&... containers) {
    return impl::product_impl(
        std::tuple<Containers...>(std::forward<Containers>(containers)...),
        std::index_sequence_for<Containers...>{});
  }

  constexpr std::array<std::tuple<>, 1> product() {
    return {{}};
  }
}

namespace iter::impl {
  // rvalue must be copied, lvalue and const lvalue references can be bound
  template <std::size_t... Is, typename Container>
  decltype(auto) product_repeat(
      std::index_sequence<Is...>, Container&& container) {
    return product(((void)Is, Container(container))...);
  }

  template <std::size_t... Is, typename Container>
  decltype(auto) product_repeat(
      std::index_sequence<Is...>, Container& container) {
    return product(((void)Is, container)...);
  }

  template <std::size_t... Is, typename Container>
  decltype(auto) product_repeat(
      std::index_sequence<Is...>, const Container& container) {
    return product(((void)Is, container)...);
  }
}

namespace iter {
  template <std::size_t N, typename Container>
  decltype(auto) product(Container&& container) {
    return impl::product_repeat(
        std::make_index_sequence<N>{}, std::forward<Container>(container));
  }
}

#endif
#ifndef ITER_RANGE_H_
#define ITER_RANGE_H_


#include <exception>

namespace iter {
  namespace impl {
    template <typename T>
    class Range;
  }

  template <typename T>
  constexpr impl::Range<T> range(T) noexcept;
  template <typename T>
  constexpr impl::Range<T> range(T, T) noexcept;
  template <typename T>
  constexpr impl::Range<T> range(T, T, T) noexcept;
}

namespace iter {
  namespace detail {
    template <typename T, bool IsFloat = std::is_floating_point<T>::value>
    class RangeIterData;

    // everything except floats
    template <typename T>
    class RangeIterData<T, false> {
     private:
      T value_{};
      T step_{};

     public:
      constexpr RangeIterData() noexcept = default;
      constexpr RangeIterData(T in_value, T in_step) noexcept
          : value_{in_value}, step_{in_step} {}

      constexpr T value() const noexcept {
        return value_;
      }

      constexpr T step() const noexcept {
        return step_;
      }

      void inc() noexcept {
        value_ += step_;
      }

      constexpr bool operator==(const RangeIterData& other) const noexcept {
        return value_ == other.value_;
      }

      constexpr bool operator!=(const RangeIterData& other) const noexcept {
        return !(*this == other);
      }
    };

    // float data
    template <typename T>
    class RangeIterData<T, true> {
     private:
      T start_{};
      T value_{};
      T step_{};
      std::size_t steps_taken_{};

     public:
      constexpr RangeIterData() noexcept = default;
      constexpr RangeIterData(T in_start, T in_step) noexcept
          : start_{in_start}, value_{in_start}, step_{in_step} {}

      constexpr T value() const noexcept {
        return value_;
      }

      constexpr T step() const noexcept {
        return step_;
      }

      void inc() noexcept {
        ++steps_taken_;
        value_ = start_ + (step_ * steps_taken_);
      }

      constexpr bool operator==(const RangeIterData& other) const noexcept {
        // if the difference between the two values is less than the
        // step_ size, they are considered equal
        return (value_ < other.value_ ? other.value_ - value_
                                      : value_ - other.value_)
               < step_;
      }

      constexpr bool operator!=(const RangeIterData& other) const noexcept {
        return !(*this == other);
      }
    };
  }
}

template <typename T>
class iter::impl::Range {
  // see stackoverflow.com/questions/32174186 about why only specializations
  // aren't marked as friend
  template <typename U>
  friend constexpr Range<U> iter::range(U) noexcept;
  template <typename U>
  friend constexpr Range<U> iter::range(U, U) noexcept;
  template <typename U>
  friend constexpr Range<U> iter::range(U, U, U) noexcept;

 private:
  const T start_;
  const T stop_;
  const T step_;

  constexpr Range(T stop) noexcept : start_{0}, stop_{stop}, step_{1} {}

  constexpr Range(T start, T stop, T step = 1) noexcept
      : start_{start}, stop_{stop}, step_{step} {}

  // if val is "before" the stopping point.
  static constexpr bool is_within_range(
      T val, T stop_val, [[maybe_unused]] T step_val) {
    if constexpr (std::is_unsigned<T>{}) {
      return val < stop_val;
    } else {
      return !(step_val > 0 && val >= stop_val)
             && !(step_val < 0 && val <= stop_val);
    }
  }

 public:
  constexpr T start() const noexcept {
    return start_;
  }

  constexpr T stop() const noexcept {
    return stop_;
  }

  constexpr T step() const noexcept {
    return step_;
  }

  constexpr T operator[](std::size_t index) const noexcept {
    return start() + (step() * index);
  }

  constexpr std::size_t size() const noexcept {
    static_assert(!std::is_floating_point_v<T>,
        "range size() not supperted with floating point types");
    if (!is_within_range(start(), stop(), step())) {
      return 0;
    }

    auto diff = stop() - start();
    auto res = diff / step();
    assert(res >= 0);
    auto result = static_cast<std::size_t>(res);
    if (diff % step()) {
      ++result;
    }
    return result;
  }

  // the reference type here is T, which doesn't strictly follow all
  // of the rules, but std::vector<bool>::iterator::reference isn't
  // a reference type either, this isn't any worse

  class Iterator {
   private:
    iter::detail::RangeIterData<T> data;
    bool is_end{};

    // first argument must be regular iterator
    // second argument must be end iterator
    static bool not_equal_to_impl(
        const Iterator& lhs, const Iterator& rhs) noexcept {
      assert(!lhs.is_end);
      assert(rhs.is_end);
      return is_within_range(
          lhs.data.value(), rhs.data.value(), lhs.data.step());
    }

    static bool not_equal_to_end(
        const Iterator& lhs, const Iterator& rhs) noexcept {
      if (rhs.is_end) {
        return not_equal_to_impl(lhs, rhs);
      }
      return not_equal_to_impl(rhs, lhs);
    }

   public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = T;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type;

    constexpr Iterator() noexcept = default;

    constexpr Iterator(T in_value, T in_step, bool in_is_end) noexcept
        : data(in_value, in_step), is_end{in_is_end} {}

    constexpr T operator*() const noexcept {
      return data.value();
    }

    constexpr ArrowProxy<T> operator->() const noexcept {
      return {**this};
    }

    Iterator& operator++() noexcept {
      data.inc();
      return *this;
    }

    Iterator operator++(int) noexcept {
      auto ret = *this;
      ++*this;
      return ret;
    }

    // This operator would more accurately read as "in bounds"
    // or "incomplete" because exact comparison with the end
    // isn't good enough for the purposes of this Iterator.
    // There are two odd cases that need to be handled
    //
    // 1) The Range is infinite, such as
    // Range (-1, 0, -1) which would go forever down toward
    // infinitely (theoretically).  If this occurs, the Range
    // will instead effectively be empty
    //
    // 2) (stop_ - start_) % step_ != 0.  For
    // example Range(1, 10, 2).  The iterator will never be
    // exactly equal to the stop_ value.
    //
    // Another way to think about it is that the "end"
    // iterator represents the range of values that are invalid
    // So, if an iterator is not equal to that, it is valid
    //
    // Two end iterators will compare equal
    //
    // Two non-end iterators will compare by their stored values
    bool operator!=(const Iterator& other) const noexcept {
      if (is_end && other.is_end) {
        return false;
      }

      if (!is_end && !other.is_end) {
        return data != other.data;
      }
      return not_equal_to_end(*this, other);
    }

    bool operator==(const Iterator& other) const noexcept {
      return !(*this != other);
    }
  };

  constexpr Iterator begin() const noexcept {
    return {start_, step_, false};
  }

  constexpr Iterator end() const noexcept {
    return {stop_, step_, true};
  }
};

template <typename T>
constexpr iter::impl::Range<T> iter::range(T stop_) noexcept {
  return {stop_};
}

template <typename T>
constexpr iter::impl::Range<T> iter::range(T start_, T stop_) noexcept {
  return {start_, stop_};
}

template <typename T>
constexpr iter::impl::Range<T> iter::range(
    T start_, T stop_, T step_) noexcept {
  return step_ == T(0) ? impl::Range<T>{0}
                       : impl::Range<T>{start_, stop_, step_};
}

#endif
#ifndef ITER_REPEAT_HPP_
#define ITER_REPEAT_HPP_


namespace iter {
  namespace impl {
    template <typename T>
    class RepeaterWithCount;
  }

  template <typename T>
  constexpr impl::RepeaterWithCount<T> repeat(T&&, int);
}

template <typename T>
class iter::impl::RepeaterWithCount {
  // see stackoverflow.com/questions/32174186/ about why this isn't
  // declaring just a specialization as friend
  template <typename U>
  friend constexpr RepeaterWithCount<U> iter::repeat(U&&, int);

 private:
  T elem_;
  int count_;

  constexpr RepeaterWithCount(T e, int c)
      : elem_(std::forward<T>(e)), count_{c} {}

  using TPlain = typename std::remove_reference<T>::type;

 public:
  RepeaterWithCount(RepeaterWithCount&&) = default;

  class Iterator {
   private:
    const TPlain* elem_;
    int count_;

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = const TPlain;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    constexpr Iterator(const TPlain* e, int c) : elem_{e}, count_{c} {}

    Iterator& operator++() {
      --this->count_;
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    constexpr bool operator!=(const Iterator& other) const {
      return !(*this == other);
    }

    constexpr bool operator==(const Iterator& other) const {
      return this->count_ == other.count_;
    }

    constexpr const TPlain& operator*() const {
      return *this->elem_;
    }

    constexpr const TPlain* operator->() const {
      return this->elem_;
    }
  };

  constexpr Iterator begin() const {
    return {&this->elem_, this->count_};
  }

  constexpr Iterator end() const {
    return {&this->elem_, 0};
  }
};

template <typename T>
constexpr iter::impl::RepeaterWithCount<T> iter::repeat(T&& e, int count_) {
  return {std::forward<T>(e), count_ < 0 ? 0 : count_};
}

namespace iter {
  namespace impl {
    template <typename T>
    class Repeater;
  }

  template <typename T>
  constexpr impl::Repeater<T> repeat(T&&);
}

template <typename T>
class iter::impl::Repeater {
  template <typename U>
  friend constexpr Repeater<U> iter::repeat(U&&);

 private:
  using TPlain = typename std::remove_reference<T>::type;
  T elem_;

  constexpr Repeater(T e) : elem_(std::forward<T>(e)) {}

 public:
  Repeater(Repeater&&) = default;

  class Iterator {
   private:
    const TPlain* elem_;

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = const TPlain;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    constexpr Iterator(const TPlain* e) : elem_{e} {}

    constexpr const Iterator& operator++() const {
      return *this;
    }

    constexpr Iterator operator++(int)const {
      return *this;
    }

    constexpr bool operator!=(const Iterator&) const {
      return true;
    }

    constexpr bool operator==(const Iterator&) const {
      return false;
    }

    constexpr const TPlain& operator*() const {
      return *this->elem_;
    }

    constexpr const TPlain* operator->() const {
      return this->elem_;
    }
  };

  constexpr Iterator begin() const {
    return {&this->elem_};
  }

  constexpr Iterator end() const {
    return {nullptr};
  }
};

template <typename T>
constexpr iter::impl::Repeater<T> iter::repeat(T&& e) {
  return {std::forward<T>(e)};
}

#endif
#ifndef ITER_REVERSE_HPP_
#define ITER_REVERSE_HPP_



namespace iter {
  namespace impl {
    template <typename Container>
    using reverse_iterator_type =
        decltype(std::rbegin(std::declval<Container&>()));
    template <typename Container>
    using reverse_iterator_end_type =
        decltype(std::rend(std::declval<Container&>()));

    // If rbegin and rend return the same type, type will be
    // reverse_iterator_type<Container>
    // If rbegin and rend return different types, type will be
    // IteratorWrapperImpl
    template <typename Container, bool same_types>
    struct ReverseIteratorWrapperImplType;

    template <typename Container>
    struct ReverseIteratorWrapperImplType<Container, true>
        : type_is<reverse_iterator_type<Container>> {};

    template <typename Container>
    struct ReverseIteratorWrapperImplType<Container, false>
        : type_is<IteratorWrapperImpl<reverse_iterator_type<Container>,
              reverse_iterator_end_type<Container>>> {};

    template <typename Container>
    using ReverseIteratorWrapper =
        typename ReverseIteratorWrapperImplType<Container,
            std::is_same_v<impl::reverse_iterator_type<Container>,
                                                    impl::
                                                        reverse_iterator_end_type<Container>>>::
            type;

    template <typename Container>
    class Reverser;

    using ReversedFn = IterToolFn<Reverser>;
  }
  constexpr impl::ReversedFn reversed{};
}

template <typename Container>
class iter::impl::Reverser {
 private:
  Container container_;
  friend ReversedFn;

  Reverser(Container&& container)
      : container_(std::forward<Container>(container)) {}

  template <typename T>
  using reverse_iterator_deref =
      decltype(*std::declval<reverse_iterator_type<T>&>());

  template <typename T>
  using reverse_iterator_traits_deref =
      std::remove_reference_t<reverse_iterator_deref<T>>;

  template <typename T>
  using reverse_iterator_arrow = detail::arrow<reverse_iterator_type<T>>;

 public:
  Reverser(Reverser&&) = default;
  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    ReverseIteratorWrapper<ContainerT> sub_iter_;

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = reverse_iterator_traits_deref<ContainerT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(ReverseIteratorWrapper<ContainerT>&& sub_iter)
        : sub_iter_{std::move(sub_iter)} {}

    reverse_iterator_deref<ContainerT> operator*() {
      return *sub_iter_;
    }

    reverse_iterator_arrow<ContainerT> operator->() {
      return apply_arrow(sub_iter_);
    }

    Iterator& operator++() {
      ++sub_iter_;
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      return sub_iter_ != other.sub_iter_;
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return !(*this != other);
    }
  };

  Iterator<Container> begin() {
    return {std::rbegin(container_)};
  }

  Iterator<Container> end() {
    return {std::rend(container_)};
  }

  Iterator<AsConst<Container>> begin() const {
    return {std::rbegin(std::as_const(container_))};
  }

  Iterator<AsConst<Container>> end() const {
    return {std::rend(std::as_const(container_))};
  }
};

#endif
#ifndef ITER_SLICE_HPP_
#define ITER_SLICE_HPP_



namespace iter {
  namespace impl {
    template <typename Container, typename DifferenceType>
    class Sliced;

    struct SliceFn;
  }
}

template <typename Container, typename DifferenceType>
class iter::impl::Sliced {
 private:
  Container container_;
  DifferenceType start_;
  DifferenceType stop_;
  DifferenceType step_;

  friend SliceFn;

  Sliced(Container&& container, DifferenceType start, DifferenceType stop,
      DifferenceType step)
      : container_(std::forward<Container>(container)),
        start_{start < stop && step > 0 ? start : stop},
        stop_{stop},
        step_{step} {}

 public:
  Sliced(Sliced&&) = default;
  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    IteratorWrapper<ContainerT> sub_iter_;
    IteratorWrapper<ContainerT> sub_end_;
    DifferenceType current_;
    DifferenceType stop_;
    DifferenceType step_;

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = iterator_traits_deref<ContainerT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(IteratorWrapper<ContainerT>&& sub_iter,
        IteratorWrapper<ContainerT>&& sub_end, DifferenceType start,
        DifferenceType stop, DifferenceType step)
        : sub_iter_{std::move(sub_iter)},
          sub_end_{std::move(sub_end)},
          current_{start},
          stop_{stop},
          step_{step} {}

    iterator_deref<ContainerT> operator*() {
      return *sub_iter_;
    }

    iterator_arrow<ContainerT> operator->() {
      return apply_arrow(sub_iter_);
    }

    Iterator& operator++() {
      dumb_advance(sub_iter_, sub_end_, step_);
      current_ += step_;
      if (stop_ < current_) {
        current_ = stop_;
      }
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      return sub_iter_ != other.sub_iter_ && current_ != other.current_;
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return !(*this != other);
    }
  };

  Iterator<Container> begin() {
    auto it = get_begin(container_);
    dumb_advance(it, get_end(container_), start_);
    return {std::move(it), get_end(container_), start_, stop_, step_};
  }

  Iterator<Container> end() {
    return {get_end(container_), get_end(container_), stop_, stop_, step_};
  }

  Iterator<AsConst<Container>> begin() const {
    auto it = get_begin(std::as_const(container_));
    dumb_advance(it, get_end(std::as_const(container_)), start_);
    return {std::move(it), get_end(std::as_const(container_)), start_, stop_,
        step_};
  }

  Iterator<AsConst<Container>> end() const {
    return {get_end(std::as_const(container_)),
        get_end(std::as_const(container_)), stop_, stop_, step_};
  }
};

struct iter::impl::SliceFn {
 private:
  template <typename DifferenceType>
  class FnPartial : public Pipeable<FnPartial<DifferenceType>> {
   public:
    template <typename Container>
    Sliced<Container, DifferenceType> operator()(Container&& container) const {
      return {std::forward<Container>(container), start_, stop_, step_};
    }

   private:
    friend SliceFn;
    constexpr FnPartial(DifferenceType start, DifferenceType stop,
        DifferenceType step) noexcept : start_{start},
                                        stop_{stop},
                                        step_{step} {}
    DifferenceType start_;
    DifferenceType stop_;
    DifferenceType step_;
  };

 public:
  template <typename Container, typename DifferenceType,
      typename = std::enable_if_t<is_iterable<Container>>>
  Sliced<Container, DifferenceType> operator()(Container&& container,
      DifferenceType start, DifferenceType stop,
      DifferenceType step = 1) const {
    return {std::forward<Container>(container), start, stop, step};
  }

  // only given the end, assume step_ is 1 and begin is 0
  template <typename Container, typename DifferenceType,
      typename = std::enable_if_t<is_iterable<Container>>>
  iter::impl::Sliced<Container, DifferenceType> operator()(
      Container&& container, DifferenceType stop) const {
    return {std::forward<Container>(container), 0, stop, 1};
  }

  template <typename DifferenceType,
      typename = std::enable_if_t<!is_iterable<DifferenceType>>>
  constexpr FnPartial<DifferenceType> operator()(DifferenceType stop) const
      noexcept {
    return {0, stop, 1};
  }

  template <typename DifferenceType,
      typename = std::enable_if_t<!is_iterable<DifferenceType>>>
  constexpr FnPartial<DifferenceType> operator()(DifferenceType start,
      DifferenceType stop, DifferenceType step = 1) const noexcept {
    return {start, stop, step};
  }
};

namespace iter {
  constexpr impl::SliceFn slice{};
}

#endif
#ifndef ITER_SLIDING_WINDOW_HPP_
#define ITER_SLIDING_WINDOW_HPP_


#include <deque>

namespace iter {
  namespace impl {
    template <typename Container>
    class WindowSlider;
    using SlidingWindowFn = IterToolFnBindSizeTSecond<WindowSlider>;
  }
  constexpr impl::SlidingWindowFn sliding_window{};
}

template <typename Container>
class iter::impl::WindowSlider {
 private:
  Container container_;
  std::size_t window_size_;

  friend SlidingWindowFn;

  WindowSlider(Container&& container, std::size_t win_sz)
      : container_(std::forward<Container>(container)), window_size_{win_sz} {}

  template <typename T>
  using IndexVector = std::deque<IteratorWrapper<T>>;
  template <typename T>
  using DerefVec = IterIterWrapper<IndexVector<T>>;

 public:
  WindowSlider(WindowSlider&&) = default;
  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    std::shared_ptr<DerefVec<ContainerT>> window_ =
        std::make_shared<DerefVec<ContainerT>>();
    IteratorWrapper<ContainerT> sub_iter_;

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = DerefVec<ContainerT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(IteratorWrapper<ContainerT>&& sub_iter,
        IteratorWrapper<ContainerT>&& sub_end, std::size_t window_sz)
        : sub_iter_(std::move(sub_iter)) {
      std::size_t i{0};
      while (i < window_sz && sub_iter_ != sub_end) {
        window_->get().push_back(sub_iter_);
        ++i;
        if (i != window_sz) {
          ++sub_iter_;
        }
      }
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      return sub_iter_ != other.sub_iter_;
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return !(*this != other);
    }

    DerefVec<ContainerT>& operator*() {
      return *window_;
    }

    DerefVec<ContainerT>* operator->() {
      return window_.get();
    }

    Iterator& operator++() {
      ++sub_iter_;
      window_->get().pop_front();
      window_->get().push_back(sub_iter_);
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }
  };

  Iterator<Container> begin() {
    return {
        (window_size_ != 0 ? IteratorWrapper<Container>{get_begin(container_)}
                           : IteratorWrapper<Container>{get_end(container_)}),
        get_end(container_), window_size_};
  }

  Iterator<Container> end() {
    return {get_end(container_), get_end(container_), window_size_};
  }

  Iterator<AsConst<Container>> begin() const {
    return {(window_size_ != 0 ? IteratorWrapper<AsConst<Container>>{get_begin(
                                     std::as_const(container_))}
                               : IteratorWrapper<AsConst<Container>>{get_end(
                                     std::as_const(container_))}),
        get_end(std::as_const(container_)), window_size_};
  }

  Iterator<AsConst<Container>> end() const {
    return {get_end(std::as_const(container_)),
        get_end(std::as_const(container_)), window_size_};
  }
};

#endif
#ifndef ITER_SORTED_HPP_
#define ITER_SORTED_HPP_



namespace iter {
  namespace impl {
    template <typename Container, typename CompareFunc>
    class SortedView;
    using SortedFn = IterToolFnOptionalBindSecond<SortedView, std::less<>>;
  }
  constexpr impl::SortedFn sorted{};
}

template <typename Container, typename CompareFunc>
class iter::impl::SortedView {
 private:
  template <typename ContainerT, typename = void>
  class SortedItersHolder {
   public:
    using IterIterWrap =
        IterIterWrapper<std::vector<iterator_type<ContainerT>>>;
    using ItIt = iterator_type<IterIterWrap>;
    using ConstItIt = void;

   private:
    ContainerT container_;
    IterIterWrap sorted_iters_;

   public:
    SortedItersHolder(ContainerT&& container, CompareFunc compare_func)
        : container_(std::forward<ContainerT>(container)) {
      // Fill the sorted_iters_ vector with an iterator to each
      // element in the container_
      for (auto iter = get_begin(container_); iter != get_end(container_);
           ++iter) {
        sorted_iters_.get().push_back(iter);
      }

      // sort by comparing the elements that the iterators point to
      std::sort(get_begin(sorted_iters_.get()), get_end(sorted_iters_.get()),
          [compare_func](
              iterator_type<Container> it1, iterator_type<Container> it2) {
            return std::invoke(compare_func, *it1, *it2);
          });
    }

    ItIt begin() {
      return sorted_iters_.begin();
    }

    ItIt end() {
      return sorted_iters_.end();
    }
  };

  template <typename ContainerT>
  class SortedItersHolder<ContainerT,
      std::void_t<decltype(std::begin(std::declval<AsConst<ContainerT>&>()))>> {
   public:
    using IterIterWrap =
        IterIterWrapper<std::vector<iterator_type<ContainerT>>>;
    using ItIt = iterator_type<IterIterWrap>;

    using ConstIterIterWrap =
        IterIterWrapper<std::vector<iterator_type<AsConst<ContainerT>>>>;
    using ConstItIt = iterator_type<ConstIterIterWrap>;

   private:
    ContainerT container_;
    mutable CompareFunc compare_func_;
    IterIterWrap sorted_iters_;
    mutable ConstIterIterWrap const_sorted_iters_;

    void populate_sorted_iters() const = delete;
    void populate_sorted_iters() {
      if (!sorted_iters_.empty()) {
        return;
      }
      // Fill the sorted_iters_ vector with an iterator to each
      // element in the container_
      for (auto iter = get_begin(container_); iter != get_end(container_);
           ++iter) {
        sorted_iters_.get().push_back(iter);
      }

      // sort by comparing the elements that the iterators point to
      std::sort(get_begin(sorted_iters_.get()), get_end(sorted_iters_.get()),
          [this](iterator_type<ContainerT> it1, iterator_type<ContainerT> it2) {
            return std::invoke(compare_func_, *it1, *it2);
          });
    }

    void populate_const_sorted_iters() = delete;
    void populate_const_sorted_iters() const {
      if (!const_sorted_iters_.empty()) {
        return;
      }
      for (auto iter = get_begin(std::as_const(container_));
           iter != get_end(std::as_const(container_)); ++iter) {
        const_sorted_iters_.get().push_back(iter);
      }

      // sort by comparing the elements that the iterators point to
      std::sort(get_begin(const_sorted_iters_.get()),
          get_end(const_sorted_iters_.get()),
          [this](iterator_type<AsConst<ContainerT>> it1,
              iterator_type<AsConst<ContainerT>> it2) {
            return compare_func_(*it1, *it2);
          });
    }

   public:
    SortedItersHolder(ContainerT&& container, CompareFunc compare_func)
        : container_(std::forward<ContainerT>(container)),
          compare_func_(std::move(compare_func)) {}

    ItIt begin() {
      populate_sorted_iters();
      return sorted_iters_.begin();
    }

    ItIt end() {
      populate_sorted_iters();
      return sorted_iters_.end();
    }

    ConstItIt begin() const {
      populate_const_sorted_iters();
      return const_sorted_iters_.begin();
    }

    ConstItIt end() const {
      populate_const_sorted_iters();
      return const_sorted_iters_.end();
    }
  };

  friend SortedFn;

  SortedItersHolder<Container> sorted_iters_holder_;

  SortedView(Container&& container, CompareFunc compare_func)
      : sorted_iters_holder_{
            std::forward<Container>(container), std::move(compare_func)} {}

 public:
  SortedView(SortedView&&) = default;

  typename SortedItersHolder<Container>::ItIt begin() {
    return sorted_iters_holder_.begin();
  }

  typename SortedItersHolder<Container>::ItIt end() {
    return sorted_iters_holder_.end();
  }

  typename SortedItersHolder<Container>::ConstItIt begin() const {
    return sorted_iters_holder_.begin();
  }

  typename SortedItersHolder<Container>::ConstItIt end() const {
    return sorted_iters_holder_.end();
  }
};

#endif
#ifndef ITER_TAKEWHILE_H_
#define ITER_TAKEWHILE_H_



namespace iter {
  namespace impl {
    template <typename FilterFunc, typename Container>
    class Taker;

    using TakeWhileFn = IterToolFnOptionalBindFirst<Taker, BoolTester>;
  }
  constexpr impl::TakeWhileFn takewhile{};
}

template <typename FilterFunc, typename Container>
class iter::impl::Taker {
 private:
  Container container_;
  mutable FilterFunc filter_func_;

  friend TakeWhileFn;

  Taker(FilterFunc filter_func, Container&& container)
      : container_(std::forward<Container>(container)),
        filter_func_(filter_func) {}

 public:
  Taker(Taker&&) = default;

  template <typename ContainerT>
  class Iterator {
   private:
    template <typename>
    friend class Iterator;
    using Holder = DerefHolder<iterator_deref<ContainerT>>;
    // I want this mutable so I can use operator* reliably in the const
    // context of init_if_first_use
    mutable IteratorWrapper<ContainerT> sub_iter_;
    IteratorWrapper<ContainerT> sub_end_;
    mutable Holder item_;
    FilterFunc* filter_func_;

    // see comments from filter about mutability
    void inc_sub_iter() {
      ++sub_iter_;
      if (sub_iter_ != sub_end_) {
        item_.reset(*sub_iter_);
      }
    }

    void check_current() const {
      if (sub_iter_ != sub_end_ && !std::invoke(*filter_func_, item_.get())) {
        sub_iter_ = sub_end_;
      }
    }

    void init_if_first_use() const {
      if (!item_ && sub_iter_ != sub_end_) {
        item_.reset(*sub_iter_);
        check_current();
      }
    }

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = iterator_traits_deref<ContainerT>;
    using difference_type = std::ptrdiff_t;
    using pointer = value_type*;
    using reference = value_type&;

    Iterator(IteratorWrapper<ContainerT>&& sub_iter,
        IteratorWrapper<ContainerT>&& sub_end, FilterFunc& filter_func)
        : sub_iter_{std::move(sub_iter)},
          sub_end_{std::move(sub_end)},
          filter_func_(&filter_func) {}

    typename Holder::reference operator*() {
      init_if_first_use();
      return item_.get();
    }

    typename Holder::pointer operator->() {
      init_if_first_use();
      return item_.get_ptr();
    }

    Iterator& operator++() {
      init_if_first_use();
      inc_sub_iter();
      check_current();
      return *this;
    }

    Iterator operator++(int) {
      auto ret = *this;
      ++*this;
      return ret;
    }

    template <typename T>
    bool operator!=(const Iterator<T>& other) const {
      init_if_first_use();
      other.init_if_first_use();
      return sub_iter_ != other.sub_iter_;
    }

    template <typename T>
    bool operator==(const Iterator<T>& other) const {
      return !(*this != other);
    }
  };

  Iterator<Container> begin() {
    return {get_begin(container_), get_end(container_), filter_func_};
  }

  Iterator<Container> end() {
    return {get_end(container_), get_end(container_), filter_func_};
  }

  Iterator<AsConst<Container>> begin() const {
    return {get_begin(std::as_const(container_)),
        get_end(std::as_const(container_)), filter_func_};
  }

  Iterator<AsConst<Container>> end() const {
    return {get_end(std::as_const(container_)),
        get_end(std::as_const(container_)), filter_func_};
  }
};

#endif
#ifndef ITER_UNIQUE_EVERSEEN_HPP_
#define ITER_UNIQUE_EVERSEEN_HPP_


#include <unordered_set>

namespace iter {
  namespace impl {
    struct UniqueEverseenFn : Pipeable<UniqueEverseenFn> {
      template <typename Container>
      auto operator()(Container&& container) const {
        using elem_type = impl::iterator_deref<Container>;
        auto func = [elem_seen = std::unordered_set<std::decay_t<elem_type>>()](
            const std::remove_reference_t<elem_type>& e) mutable {
          return elem_seen.insert(e).second;
        };
        return filter(func, std::forward<Container>(container));
      }
    };
  }

  constexpr impl::UniqueEverseenFn unique_everseen{};
}

#endif
#ifndef ITER_UNIQUE_JUSTSEEN_HPP
#define ITER_UNIQUE_JUSTSEEN_HPP



namespace iter {
  namespace impl {
    struct UniqueJustseenFn : Pipeable<UniqueJustseenFn> {
      template <typename Container>
      auto operator()(Container&& container) const {
        // decltype(auto) return type in lambda so reference types are preserved
        return imap([](auto&& group) -> decltype(
                        auto) { return *get_begin(group.second); },
            groupby(std::forward<Container>(container)));
      }
    };
  }
  constexpr impl::UniqueJustseenFn unique_justseen{};
}

#endif
