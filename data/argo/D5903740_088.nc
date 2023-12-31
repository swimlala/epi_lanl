CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-08-15T00:03:41Z AOML 3.0 creation; 2016-06-01T00:08:20Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140815000341  20160531170820  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               XA   AO  4055_7112_088                   2C  D   APEX                            5374                            041511                          846 @�B+��1   @�B���@:]�E���d���m1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    XA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dys3D���D�P D��3D�� D�	�D�FfD��3D�� D�� D�)�D�vfDǳ3D�fD�9�Dڌ�D�� D�3D�)�D�l�D�s3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��G@�z�A=qA&=qAF=qAf=qA��A��A��A��A��A��A��A��B�\B	�\B�\B�\B!�\B)�\B1�\B9�\BA�\BI�\BQ�\BY�\Ba�\Bi(�Bq�\By�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC c�Cc�Cc�Cc�Cc�C
c�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�C c�C"c�C$c�C&c�C(c�C*c�C,c�C.c�C0c�C2c�C4c�C6c�C8c�C:c�C<c�C>c�C@c�CBc�CDc�CFc�CHc�CJc�CLc�CNc�CPc�CRc�CTc�CVc�CXc�CZc�C\c�C^c�C`c�Cbc�Cdc�Cfc�Chc�Cjc�Clc�Cnc�Cpc�Crc�Ctc�Cvc�Cxc�Czc�C|c�C~c�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt��Dy�)D��HD�\{D���D��{D�D�R�D���D��{D��{D�6D���Dǿ�D�"�D�FDڙHD��{D��D�6D�yHD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��#A�t�A�E�A��A�1A���A��A��TA��A��
A���A�ĜAإ�A�ZA��
A���A׺^Aף�A�~�AּjA���A��TA�`BA�G�A���A��A�G�A�x�A��#A���A�l�A���A���A���A�A���A��PA� �A��\A��;A�A�;dA�~�A� �A� �A�;dA�1A���A��^A�^5A�?}A���A��wA��A��A��DA�bA��A���A�;dA��!A��A�jA�?}A�-A��-A�hsA�1'A��A�%A���A���A�-A��DA�\)A�7LA��A�bA��A���A��jA���A�5?A��7A�
=A���A��+A�bNA�-A�A��^A��A�VA�ĜA�A�A��TA�ffA�
=A���A�33A��FA���A�{A��!A�+A��A�ĜA�VA�l�A��A�VA�bA�n�A��A���A��A�r�A�I�A��RA���A��jA���A�dZA~�/A~A}p�A|�A{x�AzbNAxĜAvA�At{As/Aq�FApZAn��AnbAl�jAj��Ah-Ag/Af��Af�Af��Ae+Acx�AaA^�HA]��A\�A[G�AY��AWx�AU��ATn�AR��AQ33AO�AMALn�AKC�AI7LAG�-AE�TAEK�AD��AD��ADz�AC�AB�RABffAA�mAAC�A@��A>�A>A<��A;
=A9x�A8�9A8ZA7t�A5dZA3��A2�9A0ĜA/�;A.jA-��A-�A,�/A,�uA,�A+��A+�A+�A+hsA*�DA)O�A(I�A'S�A&��A%�A$ĜA$A�A#�A#�TA#�#A#ƨA#��A#|�A!�A r�A�#A�FA��A��A��A�AG�A%Ar�A��AS�A�A�uAQ�A�Al�AdZAA��A�mA\)A�A��A�AZA��AhsAG�A^5A��A�FA�A
�A�-A33A�PA-AG�A �HA ^5@�dZ@��#@�&�@�Z@�t�@��R@���@��@���@�dZ@�"�@���@�/@�@�%@�@�p�@��@�!@���@�5?@�+@���@��@�ȴ@ާ�@�~�@�ff@�Ĝ@� �@�1@�ƨ@�S�@ف@�?}@�bN@���@�n�@��@���@��;@�A�@�@�7L@�dZ@�ff@�Q�@�\)@�33@�33@��@�~�@�7L@�z�@�|�@�"�@���@�~�@�=q@�{@��T@��-@�G�@��u@��m@�l�@��+@�p�@���@��@���@�hs@��`@���@�r�@�1@�33@�=q@���@�p�@��@�z�@��
@���@�"�@�{@���@�z�@�9X@��@���@��@���@�|�@�;d@��R@��@�bN@���@�5?@���@�j@�(�@��w@�\)@�"�@�v�@�/@�1@���@�o@��y@��H@���@�ff@��T@�hs@���@��;@�S�@��H@�M�@�-@���@��@�x�@�p�@�hs@�hs@�`B@�?}@��@���@���@�r�@�Z@�1'@��F@���@���@��!@��\@�n�@�{@��@��T@���@��-@��7@�G�@��@���@�r�@�A�@���@�|�@�\)@�;d@�"�@��@�n�@�J@��T@�@��@��@��/@�Q�@��;@�ƨ@��@��@���@�l�@���@��@���@�M�@�5?@�$�@���@���@�hs@��j@���@��u@��D@�A�@�ƨ@�o@���@�ff@��@���@�X@�7L@�%@���@���@�z�@�Z@�I�@�9X@�(�@��m@�|�@�;d@��@���@���@�V@�x�@��@��j@��@�A�@|�@~�R@~v�@~v�@~E�@}@{�m@{��@{o@{@z�@z��@zn�@y�#@y&�@x��@x�@x �@w�P@w�@u�-@j�@a�7@[C�@T�@L�/@D(�@<Z@7\)@3"�@-�-@(Q�@#dZ@+@=q@�h@Q�@
M�@��@?}@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��#A�t�A�E�A��A�1A���A��A��TA��A��
A���A�ĜAإ�A�ZA��
A���A׺^Aף�A�~�AּjA���A��TA�`BA�G�A���A��A�G�A�x�A��#A���A�l�A���A���A���A�A���A��PA� �A��\A��;A�A�;dA�~�A� �A� �A�;dA�1A���A��^A�^5A�?}A���A��wA��A��A��DA�bA��A���A�;dA��!A��A�jA�?}A�-A��-A�hsA�1'A��A�%A���A���A�-A��DA�\)A�7LA��A�bA��A���A��jA���A�5?A��7A�
=A���A��+A�bNA�-A�A��^A��A�VA�ĜA�A�A��TA�ffA�
=A���A�33A��FA���A�{A��!A�+A��A�ĜA�VA�l�A��A�VA�bA�n�A��A���A��A�r�A�I�A��RA���A��jA���A�dZA~�/A~A}p�A|�A{x�AzbNAxĜAvA�At{As/Aq�FApZAn��AnbAl�jAj��Ah-Ag/Af��Af�Af��Ae+Acx�AaA^�HA]��A\�A[G�AY��AWx�AU��ATn�AR��AQ33AO�AMALn�AKC�AI7LAG�-AE�TAEK�AD��AD��ADz�AC�AB�RABffAA�mAAC�A@��A>�A>A<��A;
=A9x�A8�9A8ZA7t�A5dZA3��A2�9A0ĜA/�;A.jA-��A-�A,�/A,�uA,�A+��A+�A+�A+hsA*�DA)O�A(I�A'S�A&��A%�A$ĜA$A�A#�A#�TA#�#A#ƨA#��A#|�A!�A r�A�#A�FA��A��A��A�AG�A%Ar�A��AS�A�A�uAQ�A�Al�AdZAA��A�mA\)A�A��A�AZA��AhsAG�A^5A��A�FA�A
�A�-A33A�PA-AG�A �HA ^5@�dZ@��#@�&�@�Z@�t�@��R@���@��@���@�dZ@�"�@���@�/@�@�%@�@�p�@��@�!@���@�5?@�+@���@��@�ȴ@ާ�@�~�@�ff@�Ĝ@� �@�1@�ƨ@�S�@ف@�?}@�bN@���@�n�@��@���@��;@�A�@�@�7L@�dZ@�ff@�Q�@�\)@�33@�33@��@�~�@�7L@�z�@�|�@�"�@���@�~�@�=q@�{@��T@��-@�G�@��u@��m@�l�@��+@�p�@���@��@���@�hs@��`@���@�r�@�1@�33@�=q@���@�p�@��@�z�@��
@���@�"�@�{@���@�z�@�9X@��@���@��@���@�|�@�;d@��R@��@�bN@���@�5?@���@�j@�(�@��w@�\)@�"�@�v�@�/@�1@���@�o@��y@��H@���@�ff@��T@�hs@���@��;@�S�@��H@�M�@�-@���@��@�x�@�p�@�hs@�hs@�`B@�?}@��@���@���@�r�@�Z@�1'@��F@���@���@��!@��\@�n�@�{@��@��T@���@��-@��7@�G�@��@���@�r�@�A�@���@�|�@�\)@�;d@�"�@��@�n�@�J@��T@�@��@��@��/@�Q�@��;@�ƨ@��@��@���@�l�@���@��@���@�M�@�5?@�$�@���@���@�hs@��j@���@��u@��D@�A�@�ƨ@�o@���@�ff@��@���@�X@�7L@�%@���@���@�z�@�Z@�I�@�9X@�(�@��m@�|�@�;d@��@���@���@�V@�x�@��@��j@��@�A�@|�@~�R@~v�@~v�@~E�@}@{�m@{��@{o@{@z�@z��@zn�@y�#@y&�@x��@x�@x �@w�P@w�@u�-@j�@a�7@[C�@T�@L�/@D(�@<Z@7\)@3"�@-�-@(Q�@#dZ@+@=q@�h@Q�@
M�@��@?}@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBJBPBPBVBVB\B\BbBbBhBuB{B�B,Bn�By�Bz�Bx�Br�BffBZBP�BH�B:^B(�B�B%B�HBƨB�qB�RB�FB�FB�?B�3B�3B�3B�-B�B��B��B��B��B�JB�Bo�B_;BS�BK�BG�BE�B@�B<jB.B"�B�BDBDBDBB��B�fB�`B�ZB�`B�fB�fB�NB�B��B��B�B��B�oB�bB�VB�PB�JB�DB�DB�PB�oB��B��B��B�uB�hB�VB�=B�1B�Bu�BjBe`B_;BZBR�BL�BG�B8RBoB��B�sBĜB��B�uB�=B}�BhsBI�B%�B
��B
�B
�B
�NB
�
B
ǮB
�?B
��B
��B
�bB
|�B
t�B
dZB
\)B
XB
P�B
G�B
=qB
.B
uB
B	��B	�B	�TB	�B	��B	ŢB	�FB	��B	��B	��B	��B	��B	�bB	�%B	z�B	n�B	e`B	]/B	XB	M�B	C�B	;dB	49B	,B	$�B	�B	�B	{B	\B	1B	B��B��B��B��B��B��B�B�B�B�B�B�mB�`B�BB�#B�B��B��B��BŢB��B�jB�LB�9B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�bB�\B�\B�\B�VB�PB�=B�%B�B�B�B�B�B�B� B~�B}�B{�By�Bx�Bw�Bu�Br�Bm�BgmBaHB^5B[#BZBYBXBW
BW
BVBT�BS�BR�BO�BM�BJ�BH�BC�B:^B49B/B+B)�B(�B'�B%�B$�B$�B#�B#�B"�B#�B#�B#�B#�B#�B"�B!�B �B �B#�B$�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BhB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B"�B#�B#�B$�B$�B$�B$�B%�B%�B'�B'�B(�B+B,B0!B33B7LB9XB:^B:^B;dB>wBB�BC�BD�BF�BF�BH�BI�BJ�BM�BQ�BR�BR�BR�BYB]/B_;B`BBaHBbNBcTBk�Br�Bu�B|�B� B�B�B�B�%B�7B�bB��B��B��B��B��B��B��B��B��B�B�3B�LB�^B�}B��B��BŢBŢBŢBƨBŢBƨBǮBȴB��B��B��B��B��B��B�B�)B�)B�/B�/B�BB�HB�NB�NB�ZB�`B�mB�B�B�B�B��B��B��B��B��B��B	B	B	%B	+B		7B	PB	\B	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	 �B	 �B	!�B	#�B	(�B	)�B	+B	+B	.B	33B	:^B	=qB	@�B	C�B	E�B	G�B	H�B	J�B	L�B	N�B	O�B	P�B	P�B	Q�B	Q�B	S�B	W
B	XB	YB	ZB	[#B	\)B	`BB	cTB	e`B	ffB	k�B	o�B	r�B	t�B	v�B	w�B	v�B	z�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�B	�%B	�+B	�1B	�=B	�VB	�uB	�FB	��B	�mB	��B

=B
�B
'�B
1'B
7LB
>wB
D�B
K�B
O�B
W
B
]/B
dZB
l�B
p�B
s�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B8B8B=BBB?BFBDBMBIBUB^BhB�B+�Bn�By�Bz�Bx�Br�BfWBZBP�BH�B:KB(�BtBB�0BƋB�UB�<B�'B�)B�"B�B�B�B�B��B��B��B��B�jB�-B��Bo�B_BS�BK�BG�BE�B@bB<FB-�B"�BzBB BB�B��B�CB�?B�7B�@B�DB�EB�(B��B̭B�dB��B�~B�LB�BB�3B�+B�'B�B�B�-B�LB�nB�pB�bB�PB�AB�0B�B�B��Bu�BjZBe;B_BY�BR�BL�BG�B8*BKB��B�LB�xB��B�NB�B}�BhNBI�B%�B
��B
�B
�^B
�)B
��B
ǌB
�B
��B
�nB
�>B
|�B
t�B
d9B
\	B
W�B
P�B
G�B
=QB
-�B
TB
�B	��B	�qB	�5B	��B	��B	ŅB	�(B	��B	��B	��B	��B	��B	�EB	�B	z�B	nzB	eDB	]B	W�B	M�B	CyB	;HB	4B	+�B	$�B	�B	�B	bB	CB	B	�B��B��B��B��B��B��B�B�B��B�B�qB�TB�FB�*B�
B��B��B��B̵BŉB�kB�RB�2B�$B�
B��B��B��B��B��B��B��B��B��B��B��B��B�}B�nB�fB�PB�KB�HB�FB�GB�=B�:B�'B�B��B��B��B��B��B��B�B~�B}�B{�By�Bx�Bw�Bu�Br�Bm{BgYBa0B^B[BZBY BW�BV�BV�BU�BT�BS�BR�BO�BM�BJ�BH�BC�B:FB4&B/B*�B)�B(�B'�B%�B$�B$�B#�B#�B"�B#�B#�B#�B#�B#�B"�B!�B �B �B#�B$�B �B�B�B�BhB}ByB[BVBrBkBiBVBSBOBhBQBuB�B�B�B�BzBwB}BzBhB�B�B�B�B�B�B�B�B�B�B"�B"�B#�B#�B$�B$�B$�B$�B%�B%�B'�B'�B(�B*�B+�B0B3B73B9=B:DB:DB;JB>^BBsBC|BD�BF�BF�BH�BI�BJ�BM�BQ�BR�BR�BR�BX�B]B_B`$Ba,Bb2Bc7BkhBr�Bu�B|�B�B��B��B��B�B�B�BB�uB��B��B��B��B��B��B��B��B��B�B�)B�>B�]B�_B�fB�B�}BŁBƅBŀBƆBǋBȒBʟB̫BͯBηBϻB��B��B�B�B�B�B�B�%B�,B�*B�6B�:B�JB�\B�nB�|B��B��B��B��B��B��B��B	 �B	�B	B	B		B	+B	6B	OB	hB	iB	lB	lB	nB	wB	�B	�B	�B	 �B	 �B	 �B	 �B	!�B	#�B	(�B	)�B	*�B	*�B	-�B	3B	:6B	=MB	@]B	CnB	E{B	G�B	H�B	J�B	L�B	N�B	O�B	P�B	P�B	Q�B	Q�B	S�B	V�B	W�B	X�B	Y�B	Z�B	\B	`B	c*B	e8B	f<B	k]B	ovB	r�B	t�B	v�B	w�B	v�B	z�B	z�B	{�B	|�B	}�B	~�B	��B	��B	��B	��B	�B	�	B	�B	�,B	�KB	�B	��B	�@B	��B

B
}B
'�B
0�B
7B
>HB
DmB
K�B
O�B
V�B
] B
d*B
l\B
pvB
s�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.39 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708202016053117082020160531170820  AO  ARCAADJP                                                                    20140815000341    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140815000341  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140815000341  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170820  IP                  G�O�G�O�G�O�                