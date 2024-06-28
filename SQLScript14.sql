 if exists(select * from  information_schema.tables where table_name='fb_authors') 
          drop table fb_authors;
  if exists(select * from information_schema.tables  where table_name='fb_subjects') 
          drop table fb_subjects;
if exists(select * from information_schema.tables  where table_name='fb_bookstore') 
          drop table fb_bookstore;
		   if exists(select * from information_schema.tables  where table_name='fb_publishers') 
          drop table  fb_publishers;



CREATE TABLE fb_authors (
	author varchar (50) NOT NULL  ,
	isbn int   NOT NULL,
	 constraint pk_auth_isbn primary key (author, isbn)
) 


CREATE TABLE fb_bookstore (
	isbn int  NOT NULL primary key ,
	title varchar (255),
	price money NOT NULL,
    pages int NOT NULL ,
	pub_id int NOT NULL 
) 
GO

CREATE TABLE fb_publishers (
	pub_id int NOT NULL primary key ,
	pub_name varchar (255)  NULL ,
	pub_url varchar (255)  NULL 
) 
GO

CREATE TABLE fb_subjects (
	subj varchar (50)  NOT NULL ,
	isbn int  NOT NULL,
	constraint pk_subj_isbn primary key (subj, isbn)
) 
GO

-- foreign key constraints -- 
ALTER TABLE fb_authors
 ADD 
	CONSTRAINT FK_authors_bookstore FOREIGN KEY 
	(isbn) REFERENCES fb_bookstore (isbn)
GO

ALTER TABLE fb_bookstore 
ADD 
	CONSTRAINT FK_bookstore_publishers FOREIGN KEY 
	(pub_id) REFERENCES fb_publishers (pub_id)
GO

ALTER TABLE fb_subjects ADD 
	CONSTRAINT FK_subjects_bookstore FOREIGN KEY 
	(isbn) REFERENCES fb_bookstore (isbn)
GO

--cleaning---
update [dbo].[fudgenbooks_import$] set [Subj1] = 'MySql' where [Subj1] ='MSSQL' or [Subj1] = 'mssql'
update [dbo].[fudgenbooks_import$] set [Subj1] = 'Futureism' where [Subj1] = 'Future-ism'

update [dbo].[fudgenbooks_import$] set [Subj2] = 'MySQL' where [Subj2] = 'mysql'
update [dbo].[fudgenbooks_import$] set [Subj2] = 'PHP' where [Subj2] = 'php'

update [dbo].[fudgenbooks_import$] set [Publisher URL]='http://www.microsoft.com/mspress' where [Publisher_ID]=2
--transforming----

Insert into fb_publishers
select distinct [Publisher_id], [Publisher name], [Publisher URL] from [dbo].[fudgenbooks_import$]
WHERE [Publisher_id] is Not Null

select * from fb_bookstore

INSERT INTO fb_bookstore
select distinct [isbn], [Title],[Price],[Pages],[Publisher_id] from [dbo].[fudgenbooks_import$]
WHERE [isbn] is Not Null

select * from fb_bookstore

INSERT INTO fb_authors
select [Author1], [isbn] from [dbo].[fudgenbooks_import$] where [Author1] Is Not Null
UNION
select [Author2], [isbn] from [dbo].[fudgenbooks_import$] where [Author2] Is Not Null
UNION
select [Author3], [isbn] from [dbo].[fudgenbooks_import$] where [Author3] Is Not Null

INSERT INTO fb_subjects
select [Subj1], [isbn] from [dbo].[fudgenbooks_import$] where [Subj1] is not null
UNION
select [Subj2], [isbn] from [dbo].[fudgenbooks_import$] where [Subj2] is not null

select count(*) from fb_publishers as fb_publishers_rows
select count(*) from fb_bookstore as fb_bookstore_rows
select count (*) from fb_authors as fb_authors_rows
select count (*) from fb_subjects as fb_subjects_rows
