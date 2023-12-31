CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:05Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   =�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  >�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   C\   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  Dx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  H�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   MH   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  Nd   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   R�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  XP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   \�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ]�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   b<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  cX   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  g�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    g�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    j�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    m�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  p�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    q   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    q    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    q$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    q(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  q,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ql   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    q|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    q�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         q�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         q�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        q�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    q�Argo profile    3.1 1.2 19500101000000  20181005191705  20181005191705  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               HA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d��1   @��e-��@5=�-V�d��E�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      HA   A   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  C   C  C�C�C  C
  C�fC  C�fC  C  C  C  C�C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCK�fCN  CP  CR  CT  CV�CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr�Ct33Cv�Cx�Cz�C|  C~  C��C��C��C�  C�  C��C��C�  C��3C�  C��3C�  C�  C�  C�  C��3C�  C��C��C��C��C�  C�  C�  C��C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��C��3C��3C�  C�  C�  C��C��3C�  C��3C�  C��C�  C�  C�  C�  C��fC�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C��C��C�  C�  C�  C�  C��C��C��3C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C��C�  C�  C�  C�  C�  D   D y�D ��D� D  Dy�D  D�fD�D� DfD� D��D� D��D� D  Dy�D��D	� D
  D
� D
��Dy�DfD�fDfD� D��D�fDfD� D��Dy�D  D� D��D� D�D�fD�Dy�D  D� D�3D�fDfDy�D�0�D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @J=q@�Q�@�Q�A(�A$(�AD(�Ad(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B	
=B
=Bp�B!
=B)
=B0��B9
=BA
=BI
=BQ
=BY
=Ba
=Bi
=Bq
=Byp�B��B��B��B��B��B��B��B��B�Q�B�Q�B��B��RB�Q�B�Q�B��B��B��BąBȅB̅BЅBԅB؅B�Q�B��B�B�B�B��B�B��RB��C B�CB�C\)C\)CB�C
B�C(�CB�C(�CB�CB�CB�CB�C\)CB�C(�C B�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0B�C2\)C4\)C6B�C8B�C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJ(�CL(�CNB�CPB�CRB�CTB�CV\)CXB�CZB�C\B�C^B�C`(�CbB�CdB�CfB�ChB�CjB�ClB�CnB�Cp\)Cr\)Ctu�Cv\)Cx\)Cz\)C|B�C~B�C�.C�.C�.C�!HC�!HC�.C�.C�!HC�{C�!HC�{C�!HC�!HC�!HC�!HC�{C�!HC�.C�.C�.C�.C�!HC�!HC�!HC�.C�.C�.C�!HC�!HC�!HC�:�C�!HC�!HC�!HC�!HC�!HC�{C�.C�{C�{C�!HC�!HC�!HC�.C�{C�!HC�{C�!HC�.C�!HC�!HC�!HC�!HC��C�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�{C�{C�.C�.C�!HC�!HC�!HC�!HC�.C�.C�{C�{C�{C�!HC�!HC�!HC�!HC�!HC�!HC�.C�!HC�!HC�.C�!HC�.C�!HC�!HC�!HC�!HC�!HD �D �>D
>D��D�D�>D�D�
DqD��D
D��D
>D��D
>D��D�D�>D	
>D	��D
�D
��D
>D�>D
D�
D
D��D
>D�
D
D��D
>D�>D�D��D
>D��DqD�
DqD�>D�D��D�D�
D
Dy��D�8�D�R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AۃAۃAۅAہA�|�AہAۃAہAہA�~�A�v�A�x�A�z�A�x�A�v�A�v�A�dZA�Aڧ�Aٛ�A�C�A�^5A΅A�ZAɣ�A�z�A�  A��A�S�A�ffA�E�A�  AÝ�A¼jA�VA��A�1'A���A��#A��A�=qA��^A���A��jA��`A�p�A���A�(�A�I�A�l�A��A�|�A��!A���A��A�S�A� �A���A���A�(�A�VA���A��#A�I�A��PA�v�A��A��+A��A�bNA�I�A�+A���A���A�E�A��`A��-A�{A���A��-A��
A���A��A���A��A��PA�G�A�jA�?}A�bA���A��A��HA�ȴA��A�n�A��A�bNA�hsA�&�A~jA~Az�yAxM�Av��AuO�As�wAs7LAr(�Ap�Am��Ak�Aj�RAf�HAc;dAa��AaA^ĜA[
=AY�^AVVATn�AQhsAN�jALA�AK�-AH�AE�hADbNAB�`ABr�AB5?AA�TA@~�A>��A<�/A;��A:9XA9O�A8jA8  A7�A7XA6-A4�`A4 �A3�A1x�A1�A0A/`BA-�mA,�!A,�A+G�A*^5A)VA( �A(  A'�A'?}A&��A&$�A%��A#`BA!�A 9XA��AA�AZA�TA�A|�A33A%Al�Ap�A7LA��A�A1A�A/A��AhsAG�AbNA��A��A
�A	XA	%A��A�;A=qA1Ax�AVA�RA(�A��A�A��A"�A z�@�=q@�@�@��@�ȴ@�@�G�@ܴ9@ܣ�@ݡ�@�~�@�@ف@أ�@�r�@�ȴ@���@Չ7@�Ĝ@��@���@��@���@ёh@��`@�Q�@�@�G�@��`@�A�@���@�O�@ț�@�1@ǥ�@��H@�J@�x�@Ĵ9@��
@Ý�@���@¸R@��@��D@�Z@��;@���@���@�x�@�7L@��`@���@��9@�I�@��@�@���@��!@�ff@��@��^@��@���@�z�@�9X@��F@�dZ@�K�@���@�v�@��@��@�V@�%�@wP�@d�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AۃAۃAۅAہA�|�AہAۃAہAہA�~�A�v�A�x�A�z�A�x�A�v�A�v�A�dZA�Aڧ�Aٛ�A�C�A�^5A΅A�ZAɣ�A�z�A�  A��A�S�A�ffA�E�A�  AÝ�A¼jA�VA��A�1'A���A��#A��A�=qA��^A���A��jA��`A�p�A���A�(�A�I�A�l�A��A�|�A��!A���A��A�S�A� �A���A���A�(�A�VA���A��#A�I�A��PA�v�A��A��+A��A�bNA�I�A�+A���A���A�E�A��`A��-A�{A���A��-A��
A���A��A���A��A��PA�G�A�jA�?}A�bA���A��A��HA�ȴA��A�n�A��A�bNA�hsA�&�A~jA~Az�yAxM�Av��AuO�As�wAs7LAr(�Ap�Am��Ak�Aj�RAf�HAc;dAa��AaA^ĜA[
=AY�^AVVATn�AQhsAN�jALA�AK�-AH�AE�hADbNAB�`ABr�AB5?AA�TA@~�A>��A<�/A;��A:9XA9O�A8jA8  A7�A7XA6-A4�`A4 �A3�A1x�A1�A0A/`BA-�mA,�!A,�A+G�A*^5A)VA( �A(  A'�A'?}A&��A&$�A%��A#`BA!�A 9XA��AA�AZA�TA�A|�A33A%Al�Ap�A7LA��A�A1A�A/A��AhsAG�AbNA��A��A
�A	XA	%A��A�;A=qA1Ax�AVA�RA(�A��A�A��A"�A z�@�=q@�@�@��@�ȴ@�@�G�@ܴ9@ܣ�@ݡ�@�~�@�@ف@أ�@�r�@�ȴ@���@Չ7@�Ĝ@��@���@��@���@ёh@��`@�Q�@�@�G�@��`@�A�@���@�O�@ț�@�1@ǥ�@��H@�J@�x�@Ĵ9@��
@Ý�@���@¸R@��@��D@�Z@��;@���@���@�x�@�7L@��`@���@��9@�I�@��@�@���@��!@�ff@��@��^@��@���@�z�@�9X@��F@�dZ@�K�@���@�v�@��@��@�V@�%�@wP�@d�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBÖBB��B�}BĜB�BBBB�B�B%�B0!B=qB@�BD�BL�BaHBr�Bx�Bz�B�+B��B��B�9B�dB�jBÖBĜBŢB��B��B��B��B��BɺB�qB�?B��B��B��B��B�uB�JB�Bz�Bs�Bk�B^5BD�B=qB6FB.B-B)�B$�B�B��B�TB��B�jB�B��B�PB{�Be`BJ�B=qB7LB0!BPB
��B
�B
�B
�B
�B
�B
�BB
��B
�B
��B
�7B
hsB
K�B
G�B
/B
�B
DB
B	��B	�B	�B	�5B	��B	�jB	�'B	��B	�B	x�B	s�B	gmB	VB	K�B	=qB	1'B	#�B	{B	
=B	B��B�B�mB�TB�HB�BB�5B�B��B��BȴBB�}B�jB�^B�RB�FB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�DB�B|�Bu�Bq�Bn�Bm�BjBn�Bp�Bv�B�+B�DB�JB�VB�\B�oB��B��B��B�{B�7B�DB�DB�JB�VB�7B�1B�B�B�JB�JB�7B�+B�JB�oB��B��B��B��B�uBz�Be`BaHBk�Br�Bt�Bw�B�B�PB��B��B��B�!B�3B�?B�^B�qBÖBɺBɺBɺB��B��B��B��B��B��B��B��B�B�B�)B�5B�HB�NB�fB�mB�sB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	%B	1B	1B	
=B	DB	JB	bB	hB	uB	�B	�B	�B	!�B	'�B	)�B	+B	)�B	)�B
	�B
�B
,�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222  BĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBÖBB��B�}BĜB�BBBB�B�B%�B0!B=qB@�BD�BL�BaHBr�Bx�Bz�B�+B��B��B�9B�dB�jBÖBĜBŢB��B��B��B��B��BɺB�qB�?B��B��B��B��B�uB�JB�Bz�Bs�Bk�B^5BD�B=qB6FB.B-B)�B$�B�B��B�TB��B�jB�B��B�PB{�Be`BJ�B=qB7LB0!BPB
��B
�B
�B
�B
�B
�B
�BB
��B
�B
��B
�7B
hsB
K�B
G�B
/B
�B
DB
B	��B	�B	�B	�5B	��B	�jB	�'B	��B	�B	x�B	s�B	gmB	VB	K�B	=qB	1'B	#�B	{B	
=B	B��B�B�mB�TB�HB�BB�5B�B��B��BȴBB�}B�jB�^B�RB�FB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�DB�B|�Bu�Bq�Bn�Bm�BjBn�Bp�Bv�B�+B�DB�JB�VB�\B�oB��B��B��B�{B�7B�DB�DB�JB�VB�7B�1B�B�B�JB�JB�7B�+B�JB�oB��B��B��B��B�uBz�Be`BaHBk�Br�Bt�Bw�B�B�PB��B��B��B�!B�3B�?B�^B�qBÖBɺBɺBɺB��B��B��B��B��B��B��B��B�B�B�)B�5B�HB�NB�fB�mB�sB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	%B	1B	1B	
=B	DB	JB	bB	hB	uB	�B	�B	�B	!�B	'�B	)�B	+B	)�B	)�B
	�B
�B
,�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191705                              AO  ARCAADJP                                                                    20181005191705    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191705  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191705  QCF$                G�O�G�O�G�O�8000            