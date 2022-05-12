package ru.itmo.wp.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import ru.itmo.wp.domain.Tag;

import java.util.List;

public interface TagRepository extends JpaRepository<Tag, Long> {
    int countByName(String name);

    List<Tag> findAllByOrderByIdDesc();
    Tag findByName(String name);
}
