import { Component, h } from 'preact'
import { async, Subject } from 'most-subject';

export abstract class SubjectComponent<PropsType, StateType> extends Component<PropsType, StateType> {
  private subjects: {name: string, subject: Subject<any>}[] = [];

  /**
   * Create a new async subject stream and attach an observation handler to the
   * observable subject.
   *
   * Best to call this within componentWillMount()
   *
   * @param name Unique name of stream subject
   * @param f Stream subject's next event handler
   */
  protected mountSubject<T>(name: string, f: (a: T) => any): Promise<any> {
    let subject = async<T>();
    this.subjects.push({
      name: name,
      subject: subject
    });
    return subject.observe(f);
  }

  /**
   * Retrieve a subject mounted by this component given its unique name.
   *
   * @param name Unique name of subject to retrieve
   */
  protected subject<T>(name: string): Subject<T> | null {
    let subjects = this.subjects
      .filter(s => s.name === name);

    return subjects.length
      ? subjects.reduce((prev, current) => current).subject
      : null;
  }

  abstract componentWillMount(): void;

  componentWillUnmount() {
    this.subjects.forEach(s => s.subject.complete());
    this.subjects = [];
  }
}
