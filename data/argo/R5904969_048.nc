CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:14:09Z creation      
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
resolution        =���   axis      Z        `  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   =�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  >�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   CH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  D`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  H�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   M    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  N8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   R�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  S�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   \p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  ]�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   a�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  c    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  g`   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    g�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    j�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    m�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  p�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    p�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    p�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    p�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    p�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  p�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    q   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    q   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    q    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         q0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         q4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        q8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    q<Argo profile    3.1 1.2 19500101000000  20181024141409  20181024141409  5904969 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               0A   AO  6784                            2B  A   APEX                            7725                            111215                          846 @��$�OU�1   @��%�mP@2vȴ9X�c����+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      0A   A   A   @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dyx�D�*�D��q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�fgA ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�L�B��B��B��B��B��B��C �C�C�C�C�C
�C�C�C�C�C&gC�C�C�C�C�C �C"�C#�3C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ&gCL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3Dy|)D�,{D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A���A���A�  A�A�1A�
=A�
=A�1A�1A�%A�A�%A�A�%A�%A�
=A�1A�JA�A���A���A��yAܕ�A���A�G�A���A���A��A׋DA�-A�  A֥�A��
AՁA��A�ĜAӡ�A��HA�33A�r�A��A͝�A�5?A���ȂhA�1'A��AˮAʑhA�
=A��HA���A��Aȏ\A�jAƝ�A�  A��#A�dZA��yA�n�A�A�5?A�~�A�ȴA�-A�ĜA�XA��A�%A��-A�Q�A��A���A�JA�JA�-A�%A�$�A�l�A�VA��A���A�Q�A�+A���A�x�A���A�^5A�A�1A��FA���A��A�O�A���A��wA�oA��A�?}A�dZA��A��TA�bNA��wA���A���A��A�n�A�Q�A��DA��RA��A��
A~�9A|�A{��Az^5Ay�^Aw��AvAt1Asx�Ar-Ao�;Anz�Al�!Ak��Ag��Ad^5Aa��A_O�A^�\A^5?A\bNAX��AV9XAR^5AQ�APM�ANQ�AG�wAD��AB��AA&�A?�^A?p�A>1A<�A;C�A9l�A8�A8M�A5�
A3p�A1�mA0  A/%A-��A,z�A,$�A+33A)�7A(��A%�FA"�uA A�A�yAM�AA�A�A��A�AK�AVA�A��AA�AdZAVAS�A�`An�A �A�#A?}A�;AhsA��A�#A�A
�+A
ZA
(�A	��A�jA��AbNA ��@�r�@�M�@��@��@�-@��@�;d@@�E�@��T@���@�@��`@��@�Q�@��
@���@�n�@�!@���@��@�\)@���@�+@� �@�F@���@�V@�x�@�Q�@���@�7L@�w@�@�@�I�@�b@ڇ+@ٙ�@�p�@؋D@���@�|�@�V@� �@��m@���@��@�-@�V@�j@�(�@�  @�dZ@�C�@�"�@���@�n�@�J@�{@�5?@�-@��@���@�p�@���@�z�@�Z@�I�@�9X@� �@�(�@� �@��@�1@��;@�Y�@�e�@m�=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A���A���A�  A�A�1A�
=A�
=A�1A�1A�%A�A�%A�A�%A�%A�
=A�1A�JA�A���A���A��yAܕ�A���A�G�A���A���A��A׋DA�-A�  A֥�A��
AՁA��A�ĜAӡ�A��HA�33A�r�A��A͝�A�5?A���ȂhA�1'A��AˮAʑhA�
=A��HA���A��Aȏ\A�jAƝ�A�  A��#A�dZA��yA�n�A�A�5?A�~�A�ȴA�-A�ĜA�XA��A�%A��-A�Q�A��A���A�JA�JA�-A�%A�$�A�l�A�VA��A���A�Q�A�+A���A�x�A���A�^5A�A�1A��FA���A��A�O�A���A��wA�oA��A�?}A�dZA��A��TA�bNA��wA���A���A��A�n�A�Q�A��DA��RA��A��
A~�9A|�A{��Az^5Ay�^Aw��AvAt1Asx�Ar-Ao�;Anz�Al�!Ak��Ag��Ad^5Aa��A_O�A^�\A^5?A\bNAX��AV9XAR^5AQ�APM�ANQ�AG�wAD��AB��AA&�A?�^A?p�A>1A<�A;C�A9l�A8�A8M�A5�
A3p�A1�mA0  A/%A-��A,z�A,$�A+33A)�7A(��A%�FA"�uA A�A�yAM�AA�A�A��A�AK�AVA�A��AA�AdZAVAS�A�`An�A �A�#A?}A�;AhsA��A�#A�A
�+A
ZA
(�A	��A�jA��AbNA ��@�r�@�M�@��@��@�-@��@�;d@@�E�@��T@���@�@��`@��@�Q�@��
@���@�n�@�!@���@��@�\)@���@�+@� �@�F@���@�V@�x�@�Q�@���@�7L@�w@�@�@�I�@�b@ڇ+@ٙ�@�p�@؋D@���@�|�@�V@� �@��m@���@��@�-@�V@�j@�(�@�  @�dZ@�C�@�"�@���@�n�@�J@�{@�5?@�-@��@���@�p�@���@�z�@�Z@�I�@�9X@� �@�(�@� �@��@�1@��;@�Y�@�e�@m�=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�DB
�=B
�DB
�1B
�B
y�B
m�B
e`B
aHB
R�B
>wB
:^B
;dB
5?B
;dB
R�B
=qB
XB
m�B
bNB
W
B
z�B
�B
ŢB
�;BH�Bu�B~�B{�Bv�BW
B&�BR�Br�B�VB��B��B��B�B�B%�B+B7LB\)B�BG�B>wB6FB!�B�BPBJBDBJB\B6FB�BVB�B�BoBbB\BVBPB��B��B��B�B�/B��B��B�^B��B�VB~�BaHBF�B/B!�B�B{B
��B
�B
�fB
��B
ȴB
ŢB
�dB
�B
��B
�DB
w�B
hsB
O�B
+B
JB	��B	��B	�B	�fB	�)B	��B	��B	�qB	�RB	��B	��B	�{B	�PB	|�B	hsB	YB	M�B	F�B	C�B	=qB	(�B	!�B	bB	1B	B��B�sB�#B�B��B��B��B��B��B��BǮBŢBĜBƨBĜBBŢBĜBƨBƨBÖBƨBÖB�}B�9B�B��B��B��B�\B�=B�VB�oB��B�bB{�Bk�BiyB~�B��B�^BǮB��B��B�B�BɺB�3B��B�B�
B��B��B��B��BĜB�!B��B�+Bp�Be`BaHBe`BffB_;Bk�Bk�Bl�Bm�Bn�Bo�Bo�Br�B~�B�1B�VB�bB�oB�PB�JB��B��B��B��B�B�B�-B�3B�-B�9B�}B��B�5B�B�B��BĜBÖBŢBB��B�}B�dB�FB��B��B�PB�DB�JB�VB�\B�\B�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B
�B
�B
*K1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�DB
�=B
�DB
�1B
�B
y�B
m�B
e`B
aHB
R�B
>wB
:^B
;dB
5?B
;dB
R�B
=qB
XB
m�B
bNB
W
B
z�B
�B
ŢB
�;BH�Bu�B~�B{�Bv�BW
B&�BR�Br�B�VB��B��B��B�B�B%�B+B7LB\)B�BG�B>wB6FB!�B�BPBJBDBJB\B6FB�BVB�B�BoBbB\BVBPB��B��B��B�B�/B��B��B�^B��B�VB~�BaHBF�B/B!�B�B{B
��B
�B
�fB
��B
ȴB
ŢB
�dB
�B
��B
�DB
w�B
hsB
O�B
+B
JB	��B	��B	�B	�fB	�)B	��B	��B	�qB	�RB	��B	��B	�{B	�PB	|�B	hsB	YB	M�B	F�B	C�B	=qB	(�B	!�B	bB	1B	B��B�sB�#B�B��B��B��B��B��B��BǮBŢBĜBƨBĜBBŢBĜBƨBƨBÖBƨBÖB�}B�9B�B��B��B��B�\B�=B�VB�oB��B�bB{�Bk�BiyB~�B��B�^BǮB��B��B�B�BɺB�3B��B�B�
B��B��B��B��BĜB�!B��B�+Bp�Be`BaHBe`BffB_;Bk�Bk�Bl�Bm�Bn�Bo�Bo�Br�B~�B�1B�VB�bB�oB�PB�JB��B��B��B��B�B�B�-B�3B�-B�9B�}B��B�5B�B�B��BĜBÖBŢBB��B�}B�dB�FB��B��B�PB�DB�JB�VB�\B�\B�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B
�B
�B
*K1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141409                              AO  ARCAADJP                                                                    20181024141409    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141409  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181024141409  QCF$                G�O�G�O�G�O�0               