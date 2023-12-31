CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  ^   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:25Z creation      
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
_FillValue                    |�Argo profile    3.1 1.2 19500101000000  20181005191725  20181005191725  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d���|1   @��effy@5{dZ��d{�;dZ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @333@�  @�  A   A!��A@  Aa��A���A���A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BPffBXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  C   C  C�fC�fC  C
  C  C  C�fC  C�C�C  C  C�fC�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C=�fC?�fCA�fCD  CF  CH  CI�fCL  CN�CP�CR  CT�CV�CX�CZ  C\  C^  C_�fCa�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C}�fC�fC�  C��C�  C�  C��3C�  C�  C��C�  C�  C��3C��C�  C��3C��3C��3C�  C�  C��C�  C��3C��3C��3C�  C��C��C��C�  C�  C��C�  C��3C��3C��C��C��C�  C�  C�  C�  C��3C��3C��3C�  C��C�  C��3C��3C�  C�  C��fC��C�  C�  C�  C�  C�  C��C��3C��3C�  C��C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C��3C��3C�  C��3C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C��3C��3C��3C��3C�  C�  C�  C��C��C�  C��3C�  C��C��C��C��C�  C��3C�  C��C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  D fD � D  D��DfD� D  D� D  D� D  Dy�D  D� D  D� D��D� D��D	� D
  D
� D  Dy�D��D� DfD� D��D� DfD� D  D� D��D� D  D� D  D� D  D� D  D� D  D�fDfD�fD  Dy�D  D� D  D�fDfD�fDfD� D  D� D  D� D  Dy�D   D � D!  D!� D"  D"� D"��D#y�D$  D$�fD%fD%� D&  D&y�D&��D'� D(fD(� D)  D)� D*  D*� D+fD+� D+��D,� D-  D-� Dy�qD�8�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @4z�@���@���A Q�A!�A@Q�Aa�A���A���A�(�A�(�A�(�A�(�A�(�A�(�B {B{Bz�B{B {B({B0{B8{B@{BH{BPz�BXz�B`{Bh{Bp{Bx{B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B��
B�
=B�
=B��
B�
=B�
=B�
=B�=pB�=pB�=pB�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B��
B�
=C CC�C�CC
CCC�CC�C�CCC�C�C C"C$C&C(C*C,C.C0C2C4C6C8C:C<C=�C?�CA�CDCFCHCI�CLCN�CP�CRCT�CV�CX�CZC\C^C_�Ca�CdCfChCjClCnCpCrCtCv�CxCzC|C}�C�C��C�\C��C��C���C��C��C�\C��C��C���C�\C��C���C���C���C��C��C�\C��C���C���C���C��C�\C�\C�\C��C��C�\C��C���C���C�\C�\C�\C��C��C��C��C���C���C���C��C�\C��C���C���C��C��C���C�\C��C��C��C��C��C�\C���C���C��C�\C��C��C��C��C�\C��C���C��C��C��C��C��C��C��C���C���C���C��C���C���C��C���C��C��C��C��C���C��C��C���C���C��C��C���C���C���C���C��C��C��C�\C�\C��C���C��C�\C�\C�\C�\C��C���C��C�\C��C���C��C�\C��C���C��C��C��C��C��C��D �D �HDHD�D�D�HDHD�HDHD�HDHDz�DHD�HDHD�HD��D�HD��D	�HD
HD
�HDHDz�D��D�HD�D�HD��D�HD�D�HDHD�HD��D�HDHD�HDHD�HDHD�HDHD�HDHD��D�D��DHDz�DHD�HDHD��D�D��D�D�HDHD�HDHD�HDHDz�D HD �HD!HD!�HD"HD"�HD"��D#z�D$HD$��D%�D%�HD&HD&z�D&��D'�HD(�D(�HD)HD)�HD*HD*�HD+�D+�HD+��D,�HD-HD-�HDy��D�9HD��>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�-A�1'A�5?A�7LA�7LA�7LA�7LA�1'A�+A�(�A�/A�5?A�7LA�7LA�33A�33A�5?A�7LA�;dA�=qA�C�A�A�A�=qA�=qA�?}A�E�A�M�A�z�A�"�A�z�A״9A�7LAӕ�A�~�A�A�ZA�  A�n�A��Aʣ�A�VA��A�G�A���A�v�A�~�A�  A�{A�n�A��A��-A���A�O�A��\A�+A���A�1'A�O�A��A��A�{A��-A�A�A�JA��yA��A��FA�/A�I�A�=qA�JA��HA�
=A���A�A�dZA� �A���A�z�A�ZA�  A��-A��9A���A��/A��PA�G�A��+A�`BA��-A��TA�Q�A���A�\)A�"�A�%A�ffA��A�I�A���A��jA��A��yA���A�A�v�A~jA}�FA|ZAz��AyAu�FAq��ApQ�Al��Aj�Ag\)AfJAe��Ad�AbĜAaS�A_�A\5?AXI�AV{AT(�AQ�^APbNAO?}AMp�AKG�AH�jAF�RAE�AB  A?��A>�A<I�A;7LA9
=A7�A7/A6VA5��A5K�A3�7A21'A1�wA1XA0�yA/��A.Q�A-;dA+�
A*��A*�A)��A)t�A)?}A)oA(~�A(  A&�\A$��A#A#33A"�A"�A!t�A �yA �\AAl�A�\A�A1AVAVAXA�
AȴA�uAQ�A�A-A�/A;dA(�AdZA �AdZA�A��A�!A�uA��A	\)AG�A�AJAC�A�A��A��A�PA�A�A �yA �uA v�A E�@�C�@�I�@��@�n�@��@�z�@�P@�V@��@�%@�9X@�C�@��#@ꗍ@�V@�C�@���@�u@��@�o@���@�V@���@��;@���@�hs@ܛ�@��;@���@��@�1@�l�@Ցh@�1'@���@�=q@��@���@Ͼw@͡�@�t�@�/@ǝ�@�I�@�1'@���@�\)@ēu@�n�@�7L@���@�/@�(�@��@��@��@���@�-@���@��u@�Q�@�n�@�@�/@���@���@��H@���@��@��@���@� �@��P@��+@�E�@��T@��@�z�@���@��u@��@��D@��u@��j@��/@��/@��@�bN@�Z@��j@��#@�Z@� �@��u@�ƨ@�ȴ@�$�@���@��F@�\)@��m@�9X@���@���@�@���@�Ĝ@��@�j@�Q�@�Z@�bN@�bN@�(�@�b@�1@��
@���@��y@�v�@��#@��9@���@��9@�z�@�r�@�(�@��@��@��!@��@�&�@��@�A�@���@�"�@s�@`��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�-A�1'A�5?A�7LA�7LA�7LA�7LA�1'A�+A�(�A�/A�5?A�7LA�7LA�33A�33A�5?A�7LA�;dA�=qA�C�A�A�A�=qA�=qA�?}A�E�A�M�A�z�A�"�A�z�A״9A�7LAӕ�A�~�A�A�ZA�  A�n�A��Aʣ�A�VA��A�G�A���A�v�A�~�A�  A�{A�n�A��A��-A���A�O�A��\A�+A���A�1'A�O�A��A��A�{A��-A�A�A�JA��yA��A��FA�/A�I�A�=qA�JA��HA�
=A���A�A�dZA� �A���A�z�A�ZA�  A��-A��9A���A��/A��PA�G�A��+A�`BA��-A��TA�Q�A���A�\)A�"�A�%A�ffA��A�I�A���A��jA��A��yA���A�A�v�A~jA}�FA|ZAz��AyAu�FAq��ApQ�Al��Aj�Ag\)AfJAe��Ad�AbĜAaS�A_�A\5?AXI�AV{AT(�AQ�^APbNAO?}AMp�AKG�AH�jAF�RAE�AB  A?��A>�A<I�A;7LA9
=A7�A7/A6VA5��A5K�A3�7A21'A1�wA1XA0�yA/��A.Q�A-;dA+�
A*��A*�A)��A)t�A)?}A)oA(~�A(  A&�\A$��A#A#33A"�A"�A!t�A �yA �\AAl�A�\A�A1AVAVAXA�
AȴA�uAQ�A�A-A�/A;dA(�AdZA �AdZA�A��A�!A�uA��A	\)AG�A�AJAC�A�A��A��A�PA�A�A �yA �uA v�A E�@�C�@�I�@��@�n�@��@�z�@�P@�V@��@�%@�9X@�C�@��#@ꗍ@�V@�C�@���@�u@��@�o@���@�V@���@��;@���@�hs@ܛ�@��;@���@��@�1@�l�@Ցh@�1'@���@�=q@��@���@Ͼw@͡�@�t�@�/@ǝ�@�I�@�1'@���@�\)@ēu@�n�@�7L@���@�/@�(�@��@��@��@���@�-@���@��u@�Q�@�n�@�@�/@���@���@��H@���@��@��@���@� �@��P@��+@�E�@��T@��@�z�@���@��u@��@��D@��u@��j@��/@��/@��@�bN@�Z@��j@��#@�Z@� �@��u@�ƨ@�ȴ@�$�@���@��F@�\)@��m@�9X@���@���@�@���@�Ĝ@��@�j@�Q�@�Z@�bN@�bN@�(�@�b@�1@��
@���@��y@�v�@��#@��9@���@��9@�z�@�r�@�(�@��@��@��!@��@�&�@��@�A�@���@�"�@s�@`��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B+B�BO�BW
B\)B`BBiyBm�Bn�By�Bz�B�7B��B��B��B��B�B�jBŢB��B�B�#B�B�B�)B�#B�B��B��BŢB�LBɺBB�'B��B�\B�+B�B�B�B�Bw�Bp�BgmBXBK�B<jB.B�BVB��B�B�ZB�B��BƨB�RB��B�oB}�Bm�BaHB^5BR�BC�B9XB0!B.B+B�B+B
�B
�mB
�5B
��B
ÖB
�'B
��B
�=B
k�B
VB
O�B
F�B
;dB
/B
�B	��B	�B	�B	��B	�LB	�B	�B	��B	��B	�\B	�B	p�B	[#B	K�B	<jB	.B	$�B	�B	�B	PB	B��B�B�HB�B��BǮBÖB�}B�wB�qB�qB�jB�jB�qB�jB�dB�XB�RB�9B�!B�B��B��B��B��B��B��B��B��B��B��B�oB�{B�{B�{B�uB�hB�\B�VB�JB�=B�1B�B�B� B{�By�Bu�Br�Bq�Bp�Bn�BjBffBbNB^5B\)B[#BYBYBYBZBZBYBS�BK�BF�BA�B@�B?}BC�BL�BR�BXBcTBffBdZBcTBbNB_;BVBM�BC�B@�BJ�BQ�BP�BO�BM�BL�BJ�BL�BK�BK�BVBT�BS�BVBT�BR�BS�BZB]/B\)B\)B\)B\)B[#BXBXBW
BXBXBYB[#B_;BaHB^5BZBQ�BK�BN�BZB\)BffBl�BffB_;B\)BZBVB[#B\)B[#B\)BbNBbNBcTBe`Be`Bk�Bm�Bn�Bp�Br�Bu�Bu�Bv�By�Bz�B{�B}�B�B�B�%B�=B�\B�hB�oB�uB�uB��B��B��B��B��B��B�B�-B��B��B�B�)B�/B�#B�#B�B��B��B��B�B�;B�yB��B��B��B��B��B��B��B	  B	+B	DB	JB	JB	VB	bB	�B	!�B	!�B	!�B	"�B	(�B	.B	49B	5?B	?}B	C�B	C�B	B�B	B�B	C�B	D�B	G�B
 iB
�B
Q22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B+B�BO�BW
B\)B`BBiyBm�Bn�By�Bz�B�7B��B��B��B��B�B�jBŢB��B�B�#B�B�B�)B�#B�B��B��BŢB�LBɺBB�'B��B�\B�+B�B�B�B�Bw�Bp�BgmBXBK�B<jB.B�BVB��B�B�ZB�B��BƨB�RB��B�oB}�Bm�BaHB^5BR�BC�B9XB0!B.B+B�B+B
�B
�mB
�5B
��B
ÖB
�'B
��B
�=B
k�B
VB
O�B
F�B
;dB
/B
�B	��B	�B	�B	��B	�LB	�B	�B	��B	��B	�\B	�B	p�B	[#B	K�B	<jB	.B	$�B	�B	�B	PB	B��B�B�HB�B��BǮBÖB�}B�wB�qB�qB�jB�jB�qB�jB�dB�XB�RB�9B�!B�B��B��B��B��B��B��B��B��B��B��B�oB�{B�{B�{B�uB�hB�\B�VB�JB�=B�1B�B�B� B{�By�Bu�Br�Bq�Bp�Bn�BjBffBbNB^5B\)B[#BYBYBYBZBZBYBS�BK�BF�BA�B@�B?}BC�BL�BR�BXBcTBffBdZBcTBbNB_;BVBM�BC�B@�BJ�BQ�BP�BO�BM�BL�BJ�BL�BK�BK�BVBT�BS�BVBT�BR�BS�BZB]/B\)B\)B\)B\)B[#BXBXBW
BXBXBYB[#B_;BaHB^5BZBQ�BK�BN�BZB\)BffBl�BffB_;B\)BZBVB[#B\)B[#B\)BbNBbNBcTBe`Be`Bk�Bm�Bn�Bp�Br�Bu�Bu�Bv�By�Bz�B{�B}�B�B�B�%B�=B�\B�hB�oB�uB�uB��B��B��B��B��B��B�B�-B��B��B�B�)B�/B�#B�#B�B��B��B��B�B�;B�yB��B��B��B��B��B��B��B	  B	+B	DB	JB	JB	VB	bB	�B	!�B	!�B	!�B	"�B	(�B	.B	49B	5?B	?}B	C�B	C�B	B�B	B�B	C�B	D�B	G�B
 iB
�B
Q22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191725                              AO  ARCAADJP                                                                    20181005191725    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191725  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191725  QCF$                G�O�G�O�G�O�8000            