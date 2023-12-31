CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  ^   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:20Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        x  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  @H   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  E�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  G    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  L�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  R   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  Sp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  X�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ZH   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  _�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  e8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  f�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 `  l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  mp   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  r�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    s   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    v   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    y   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  |   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    |D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    |H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    |L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    |P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  |T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    |�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    |�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    |�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         |�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         |�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        |�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    |�Argo profile    3.1 1.2 19500101000000  20181005191720  20181005191720  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$�	�1   @��%j1`n@5n��P�dw�;dZ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBHffBO��BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@�CB  CC�fCE�fCH  CJ  CL  CN  CO�fCR  CT  CU�fCX  CZ  C[�fC^  C`�Cb�Cd  Ce�fCg�fCi�fCk�fCm�fCp  Cr  Cs�fCv  Cx  Cz  C|  C}�fC�  C�  C��3C�  C�  C�  C��3C��3C��fC��fC��3C��3C��3C�  C�  C��C��3C�  C��3C��3C��3C�  C��3C��3C��3C��fC��fC��3C��3C��3C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C��C�  C��C��C�  C��3C��C�  C��3C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��3C��3C��3C�  C�  C�  C�  C�  C��C�  C��fC�  C��C�  C��3C��3C��3C��3C�  C��C�  C��C�  C�  C�  C�  C��3C��3C�  C��C��C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��C�  C�  C�  C�  C��3C��3C��3C��3C��3C��3C�  C��3C�  C��C��3C��3D � D  D�fDfD�fD  Dy�D  D�fDfD� D��D� DfD�fD  D� D	fD	�fD	��D
� D
��Dy�D  Dy�D��Dy�D  D� DfD� D��D� D  D�fD  D� D  D� D  Dy�D��Ds3D��Dy�D  D�fDfD�fDfD� D��Dy�DfD�fDfD� D  D� D  D� D  Dy�D   D � D!  D!� D"fD"�fD#fD#�fD$fD$� D$��D%� D&  D&s3D&��D'� D(  D(� D)fD)�fD*fD*� D+  D+� D,  D,� D,��D-� Dy��D�FfD��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @<(�@�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@�\BH�\BOBWB`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�G�B�{B�{B�{B�{B��HB�{B�{B�{B�{B�{B�{B�{B�G�B�G�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
=C
=C
=C
=C
=C

=C
=C
=C
=C
=C
=C�C
=C
=C
=C
=C 
=C!�C$
=C&
=C(
=C*
=C,
=C.
=C0
=C2
=C4
=C6
=C8
=C:
=C;�C>
=C@#�CB
=CC�CE�CH
=CJ
=CL
=CN
=CO�CR
=CT
=CU�CX
=CZ
=C[�C^
=C`#�Cb#�Cd
=Ce�Cg�Ci�Ck�Cm�Cp
=Cr
=Cs�Cv
=Cx
=Cz
=C|
=C}�C�C�C��RC�C�C�C��RC��RC��C��C��RC��RC��RC�C�C��C��RC�C��RC��RC��RC�C��RC��RC��RC��C��C��RC��RC��RC�C��C�C�C�C�C�C��RC�C�C�C�C�C�C�C��RC�C��RC��C�C��C��C�C��RC��C�C��RC�C�C�C��RC�C�C��RC�C�C�C�C�C�C��C��RC��RC��RC�C�C�C�C�C��C�C��C�C��C�C��RC��RC��RC��RC�C��C�C��C�C�C�C�C��RC��RC�C��C��C�C�C�C�C�C��C�C��RC�C�C��C�C�C�C�C��RC��RC��RC��RC��RC��RC�C��RC�C��C��RC��RD ��D�D��D�D��D�D|)D�D��D�D��D�)D��D�D��D�D��D	�D	��D	�)D
��D
�)D|)D�D|)D�)D|)D�D��D�D��D�)D��D�D��D�D��D�D��D�D|)D�)Du�D�)D|)D�D��D�D��D�D��D�)D|)D�D��D�D��D�D��D�D��D�D|)D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D$�)D%��D&�D&u�D&�)D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D,�)D-��Dy�{D�G�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�E�A�C�A�C�A�C�A�A�A�E�A�E�A�C�A�I�A�E�A�C�A�E�A�G�A�O�A�Q�A�S�A�VA�O�A�O�A�O�A�Q�A�Q�A�M�A�1'Aڴ9Aٟ�A�n�A�bNA�G�A��A͑hA�"�A�p�A�bA�-A�G�A�t�A�ȴAţ�A���A�{A�VA�5?A���A���A��7A���A�|�A�?}A�ffA���A���A�+A��A���A�%A�
=A�I�A��PA���A�5?A��RA��A��DA�{A��
A�  A�/A�1A�x�A��A���A�VA�~�A���A��A�O�A�~�A��A���A��7A��A��/A�`BA��A��FA��yA��A�;dA��mA���A|��Az��Ay%AwK�Au`BAs&�Ao�mAm�Am?}AkƨAi\)Agp�AeXAb�jA^�`A[��AXȴAV�+AR�AO�AM
=AJ��AJv�AIt�AEG�ABȴAAS�A@�!A>�jA=�-A=S�A;�A:�A9p�A8��A6JA3��A2{A1�^A1�PA1`BA0�yA05?A/XA.(�A,Q�A*��A)�A(1'A&�9A%��A$��A$Q�A${A#�A#��A"�RA!C�A�#A|�A�9A�`A�
A`BA��A7LA&�Ar�A;dA�A�A��A�AZA�^A��A�RA{A%A	AĜA��AdZA�jAA�A33A�PA��AVA�A|�A �u@��
@�C�@��@�
=@��@�r�@�C�@���@��\@�@��u@���@�n�@�/@�w@�l�@�o@�@��/@�\)@�X@�1'@���@��#@�1@�o@柾@��#@�7L@���@�I�@߾w@�@�O�@ۮ@���@�l�@�^5@�J@�O�@Լj@��@ҸR@�b@�V@��H@ȼj@� �@���@�S�@�ȴ@�bN@�;d@���@���@�o@¸R@��@�O�@��u@�bN@� �@��F@�"�@�-@��@�(�@�J@�?}@�/@�&�@��9@�z�@��@�hs@��@��@�j@�9X@��@�1@���@��#@�M�@���@�%@��@��w@�\)@���@��@��j@�j@�b@���@�l�@�C�@��@��y@�@��H@��h@�A�@�=q@�5?@�-@�&�@�C�@�O�@��@���@��u@�b@�I�@��u@�/@�Z@�  @�j@�j@���@�l�@�ȴ@�n�@�=q@��@���@���@�`B@��D@�I�@��@�1@��
@��@�C�@�;d@�;d@�o@���@���@�V@�J@�-@���@�o@���@��@���@�-@���@��#@��^@�@���@��@�Ĝ@��@�r�@�Z@�r�@�r�@�bN@�Q�@}�@k�k@[�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�E�A�C�A�C�A�C�A�A�A�E�A�E�A�C�A�I�A�E�A�C�A�E�A�G�A�O�A�Q�A�S�A�VA�O�A�O�A�O�A�Q�A�Q�A�M�A�1'Aڴ9Aٟ�A�n�A�bNA�G�A��A͑hA�"�A�p�A�bA�-A�G�A�t�A�ȴAţ�A���A�{A�VA�5?A���A���A��7A���A�|�A�?}A�ffA���A���A�+A��A���A�%A�
=A�I�A��PA���A�5?A��RA��A��DA�{A��
A�  A�/A�1A�x�A��A���A�VA�~�A���A��A�O�A�~�A��A���A��7A��A��/A�`BA��A��FA��yA��A�;dA��mA���A|��Az��Ay%AwK�Au`BAs&�Ao�mAm�Am?}AkƨAi\)Agp�AeXAb�jA^�`A[��AXȴAV�+AR�AO�AM
=AJ��AJv�AIt�AEG�ABȴAAS�A@�!A>�jA=�-A=S�A;�A:�A9p�A8��A6JA3��A2{A1�^A1�PA1`BA0�yA05?A/XA.(�A,Q�A*��A)�A(1'A&�9A%��A$��A$Q�A${A#�A#��A"�RA!C�A�#A|�A�9A�`A�
A`BA��A7LA&�Ar�A;dA�A�A��A�AZA�^A��A�RA{A%A	AĜA��AdZA�jAA�A33A�PA��AVA�A|�A �u@��
@�C�@��@�
=@��@�r�@�C�@���@��\@�@��u@���@�n�@�/@�w@�l�@�o@�@��/@�\)@�X@�1'@���@��#@�1@�o@柾@��#@�7L@���@�I�@߾w@�@�O�@ۮ@���@�l�@�^5@�J@�O�@Լj@��@ҸR@�b@�V@��H@ȼj@� �@���@�S�@�ȴ@�bN@�;d@���@���@�o@¸R@��@�O�@��u@�bN@� �@��F@�"�@�-@��@�(�@�J@�?}@�/@�&�@��9@�z�@��@�hs@��@��@�j@�9X@��@�1@���@��#@�M�@���@�%@��@��w@�\)@���@��@��j@�j@�b@���@�l�@�C�@��@��y@�@��H@��h@�A�@�=q@�5?@�-@�&�@�C�@�O�@��@���@��u@�b@�I�@��u@�/@�Z@�  @�j@�j@���@�l�@�ȴ@�n�@�=q@��@���@���@�`B@��D@�I�@��@�1@��
@��@�C�@�;d@�;d@�o@���@���@�V@�J@�-@���@�o@���@��@���@�-@���@��#@��^@�@���@��@�Ĝ@��@�r�@�Z@�r�@�r�@�bN@�Q�@}�@k�k@[�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBBBBBBBBBBBBBBBBBBBBBBBBB	7B#�BL�BQ�BVBYB]/Be`BhsBq�By�B�=B��B��B��B�B�!BBǮB��B��B��B��B�B�B�B�B��B�LB�B��B�uBw�Bm�Bl�By�BffB\)BH�B6FB)�BVBB��B�HB��B�qB�B��B��B�JBu�BiyBaHB[#BL�B7LB$�B\BB
�sB
�B
�dB
�%B
gmB
I�B
+B
�B
hB
%B	��B	�sB	�
B	ȴB	B	�RB	��B	��B	�JB	z�B	hsB	S�B	G�B	9XB	$�B	{B	DB	B��B��B�yB�BB�#B�B��B��B��BǮBB�}B�dB�FB�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB��B��B��B��B��B��B��B�DB{�Bv�Bs�Bo�BiyBdZBbNB_;B\)B[#BXBVBQ�BN�BK�BI�BG�BE�BB�BA�B?}B>wB<jB;dB9XB8RB8RB8RB7LB7LB6FB6FB5?B5?B49B49B33B33B2-B2-B1'B1'B0!B/B.B.B-B,B,B,B+B+B+B)�B)�B+B+B+B+B+B,B/B0!B/B0!B0!B0!B/B33B49B2-B7LB9XB<jB?}B@�BI�BR�B\)B`BBhsBk�Bn�Bo�Bp�Bp�Bq�Bp�Br�Bq�Bo�Bm�Bm�Bn�Bo�Bo�Bn�Bn�Bn�Bq�Bq�Bq�Br�Bs�Bs�Bt�Bt�Bs�Bu�B{�B}�B|�B{�B{�B|�B~�B� B�B�B�+B�DB�bB��B��B�'B�FB�9B�9B��BÖBŢBÖB�wB�^B�dB�qB��B��B��B�
B�#B�5B�NB�`B�B�B��B	DB	bB	hB	�B	�B	�B	�B	'�B	)�B	+B	+B	+B	,B	-B	-B	-B	.B	0!B	2-B	49B	7LB	>wB	E�B	J�B	L�B	N�B	O�B	P�B	Q�B	R�B	S�B	S�B	S�B	S�B	S�B	T�B	XB	]/B	`BB	bNB	cTB	cTB	�B
:B
#�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  BBBBBBBBBBBBBBBBBBBBBBBBBB	7B#�BL�BQ�BVBYB]/Be`BhsBq�By�B�=B��B��B��B�B�!BBǮB��B��B��B��B�B�B�B�B��B�LB�B��B�uBw�Bm�Bl�By�BffB\)BH�B6FB)�BVBB��B�HB��B�qB�B��B��B�JBu�BiyBaHB[#BL�B7LB$�B\BB
�sB
�B
�dB
�%B
gmB
I�B
+B
�B
hB
%B	��B	�sB	�
B	ȴB	B	�RB	��B	��B	�JB	z�B	hsB	S�B	G�B	9XB	$�B	{B	DB	B��B��B�yB�BB�#B�B��B��B��BǮBB�}B�dB�FB�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB��B��B��B��B��B��B��B�DB{�Bv�Bs�Bo�BiyBdZBbNB_;B\)B[#BXBVBQ�BN�BK�BI�BG�BE�BB�BA�B?}B>wB<jB;dB9XB8RB8RB8RB7LB7LB6FB6FB5?B5?B49B49B33B33B2-B2-B1'B1'B0!B/B.B.B-B,B,B,B+B+B+B)�B)�B+B+B+B+B+B,B/B0!B/B0!B0!B0!B/B33B49B2-B7LB9XB<jB?}B@�BI�BR�B\)B`BBhsBk�Bn�Bo�Bp�Bp�Bq�Bp�Br�Bq�Bo�Bm�Bm�Bn�Bo�Bo�Bn�Bn�Bn�Bq�Bq�Bq�Br�Bs�Bs�Bt�Bt�Bs�Bu�B{�B}�B|�B{�B{�B|�B~�B� B�B�B�+B�DB�bB��B��B�'B�FB�9B�9B��BÖBŢBÖB�wB�^B�dB�qB��B��B��B�
B�#B�5B�NB�`B�B�B��B	DB	bB	hB	�B	�B	�B	�B	'�B	)�B	+B	+B	+B	,B	-B	-B	-B	.B	0!B	2-B	49B	7LB	>wB	E�B	J�B	L�B	N�B	O�B	P�B	Q�B	R�B	S�B	S�B	S�B	S�B	S�B	T�B	XB	]/B	`BB	bNB	cTB	cTB	�B
:B
#�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.04 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191720                              AO  ARCAADJP                                                                    20181005191720    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191720  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191720  QCF$                G�O�G�O�G�O�8000            