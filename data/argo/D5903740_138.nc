CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-01-08T10:17:02Z AOML 3.0 creation; 2016-06-01T00:08:28Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160108101702  20160531170828  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_138                   2C  D   APEX                            5374                            041511                          846 @׌F'Ҁ1   @׌F����@:j��n��c�9XbN1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� DfD� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;fD;�fD<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy�fD���D�FfD�vfD��3D�  D�@ D��fD�ɚD�3D�L�D�  D��3D��D�9�D�y�D���D�  D�I�D�fD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��G@�z�A=qA&=qAF=qAf=qA��A��A��A��A��A��A��A��B�\B	�\B�\B�\B!�\B)�\B1�\B9�\BA�\BI�\BQ�\BY�\Ba�\Bi�\Bq�\By�\B�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC c�Cc�Cc�Cc�Cc�C
c�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�C c�C"c�C$c�C&c�C(c�C*c�C,c�C.c�C0c�C2c�C4c�C6c�C8c�C:c�C<c�C>c�C@c�CBc�CDc�CFc�CHc�CJc�CLc�CNc�CP}qCRc�CTc�CVc�CXc�CZc�C\c�C^c�C`c�Cbc�Cdc�Cfc�Chc�Cjc�Clc�Cnc�Cpc�Crc�Ctc�Cvc�Cxc�Czc�C|c�C~c�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D\D��D�D��D\D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;\D;�\D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy�\D�D�R�D���D���D�,{D�L{D���D��D��D�YHD�,{D�߮D�HD�FDچD��HD�{D�VD��D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�?}A�A�A�?}A�A�A�G�A�?}A�$�A��A�oA�bA�bA�bA�oA�bA�VA�
=A�
=A�
=A�
=A�
=A�1A�
=A�
=A�
=A�
=A�
=A�
=A�1A�1A�1A�1A�%A�%A�A�A��A�|�A�"�A�S�A���A��A��PA���A��RA�A��A�ƨA��A�  A�p�A���A�M�A���A�9XA��A�XA�oA�n�A�+A��TA�x�A��A��/A�;dA���A�?}A��
A�x�A�M�A��^A�9XA�bA���A�A�v�A���A�-A�A��#A�|�A�M�A���A�1A��+A���A��DA�p�A���A�r�A}�-A|r�A{|�A{+Az�jAy��AxȴAxz�Ax5?Aw33Av��Au��At��AtQ�Ar�yAq�^Ap�9AoK�An{AmG�Al��Ak"�Ai��Ah9XAghsAfz�Af9XAe/Ac��Abr�Aa�7A_�#A]��A\��A\JA[�AZ�\AYƨAYhsAX��AW��AV��AUp�AT5?AS�^AS7LAQ|�APn�AO/ANjAL�!AK�AJ�yAI�AHVAGS�AF�DAD�`AD-AAS�A?�wA?/A=�;A=�A<�DA;p�A:A9t�A8z�A7��A6�A5
=A3�#A3�A3?}A2�A1C�A0�A05?A/�wA/S�A.��A.jA-�^A-;dA,jA+l�A+�A*(�A((�A&�uA%ƨA$ȴA#��A"�`A"�A!�hA!VA �uAt�A�/A��AhsA33A��A�AbNA33A��An�A  A��A7LAz�A�^A��AffA�AƨA�AjA|�AȴA��A�DA  A��A�9A�A��A/A
�`A
z�A	A	��A	33AI�Ax�A�yA��AffA�A�-AXA/A^5AK�AM�A+A E�@���@�C�@���@�hs@��/@�bN@�S�@�@�x�@� �@��\@�G�@�@�u@�K�@��@�G�@���@��@��m@旍@噚@���@��@�%@�r�@ߝ�@���@��@ّh@�r�@��y@�{@���@�(�@Ӿw@ҟ�@ѩ�@�bN@�
=@��@���@��@�dZ@���@��#@�G�@ǅ@�5?@��`@�bN@���@�\)@�@��#@�Ĝ@� �@��R@���@�V@�I�@���@���@�;d@�ff@�$�@��-@�7L@��@���@�bN@��@�C�@�$�@��@�A�@�  @�K�@���@��@�l�@�o@���@�E�@��@�@���@���@�Z@�o@��^@�&�@���@��u@��@��P@�S�@���@��#@�O�@�I�@���@�"�@�{@��@�j@�9X@��@���@��@��h@��`@�z�@��;@�+@�^5@��@�/@���@�Ĝ@�z�@�r�@�1'@��@���@��w@���@�"�@���@�E�@��@�7L@��@��@�A�@��@��m@��
@���@���@���@�ƨ@���@��!@�5?@��-@�p�@�/@��@�b@�l�@��y@��\@�{@�J@�p�@��`@���@��@��@�;d@��@��@��@��9@�Ĝ@���@��D@�r�@�I�@�A�@��
@�t�@�K�@�
=@��@���@��H@���@�~�@�E�@�@���@��7@�V@��@�Ĝ@��u@�j@� �@���@���@�1'@��@���@��#@��#@��T@��@��h@��@�r�@�9X@��
@�;d@�o@�o@���@��R@���@�v�@�~�@�~�@�~�@��+@�~�@�n�@�^5@�E�@��@��T@��h@�hs@�X@�?}@��9@��@~�+@~V@~�R@~ȴ@~�y@~��@~5?@}�@}�T@}�T@}@}��@}��@}��@}��@}@}`B@|�/@|�j@|��@|j@|j@z=q@s�F@k�@c��@X��@Q�^@J^5@DZ@>�+@9G�@0A�@.{@(��@#33@�-@G�@��@�`@��@	�#@ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�?}A�A�A�?}A�A�A�G�A�?}A�$�A��A�oA�bA�bA�bA�oA�bA�VA�
=A�
=A�
=A�
=A�
=A�1A�
=A�
=A�
=A�
=A�
=A�
=A�1A�1A�1A�1A�%A�%A�A�A��A�|�A�"�A�S�A���A��A��PA���A��RA�A��A�ƨA��A�  A�p�A���A�M�A���A�9XA��A�XA�oA�n�A�+A��TA�x�A��A��/A�;dA���A�?}A��
A�x�A�M�A��^A�9XA�bA���A�A�v�A���A�-A�A��#A�|�A�M�A���A�1A��+A���A��DA�p�A���A�r�A}�-A|r�A{|�A{+Az�jAy��AxȴAxz�Ax5?Aw33Av��Au��At��AtQ�Ar�yAq�^Ap�9AoK�An{AmG�Al��Ak"�Ai��Ah9XAghsAfz�Af9XAe/Ac��Abr�Aa�7A_�#A]��A\��A\JA[�AZ�\AYƨAYhsAX��AW��AV��AUp�AT5?AS�^AS7LAQ|�APn�AO/ANjAL�!AK�AJ�yAI�AHVAGS�AF�DAD�`AD-AAS�A?�wA?/A=�;A=�A<�DA;p�A:A9t�A8z�A7��A6�A5
=A3�#A3�A3?}A2�A1C�A0�A05?A/�wA/S�A.��A.jA-�^A-;dA,jA+l�A+�A*(�A((�A&�uA%ƨA$ȴA#��A"�`A"�A!�hA!VA �uAt�A�/A��AhsA33A��A�AbNA33A��An�A  A��A7LAz�A�^A��AffA�AƨA�AjA|�AȴA��A�DA  A��A�9A�A��A/A
�`A
z�A	A	��A	33AI�Ax�A�yA��AffA�A�-AXA/A^5AK�AM�A+A E�@���@�C�@���@�hs@��/@�bN@�S�@�@�x�@� �@��\@�G�@�@�u@�K�@��@�G�@���@��@��m@旍@噚@���@��@�%@�r�@ߝ�@���@��@ّh@�r�@��y@�{@���@�(�@Ӿw@ҟ�@ѩ�@�bN@�
=@��@���@��@�dZ@���@��#@�G�@ǅ@�5?@��`@�bN@���@�\)@�@��#@�Ĝ@� �@��R@���@�V@�I�@���@���@�;d@�ff@�$�@��-@�7L@��@���@�bN@��@�C�@�$�@��@�A�@�  @�K�@���@��@�l�@�o@���@�E�@��@�@���@���@�Z@�o@��^@�&�@���@��u@��@��P@�S�@���@��#@�O�@�I�@���@�"�@�{@��@�j@�9X@��@���@��@��h@��`@�z�@��;@�+@�^5@��@�/@���@�Ĝ@�z�@�r�@�1'@��@���@��w@���@�"�@���@�E�@��@�7L@��@��@�A�@��@��m@��
@���@���@���@�ƨ@���@��!@�5?@��-@�p�@�/@��@�b@�l�@��y@��\@�{@�J@�p�@��`@���@��@��@�;d@��@��@��@��9@�Ĝ@���@��D@�r�@�I�@�A�@��
@�t�@�K�@�
=@��@���@��H@���@�~�@�E�@�@���@��7@�V@��@�Ĝ@��u@�j@� �@���@���@�1'@��@���@��#@��#@��T@��@��h@��@�r�@�9X@��
@�;d@�o@�o@���@��R@���@�v�@�~�@�~�@�~�@��+@�~�@�n�@�^5@�E�@��@��T@��h@�hs@�X@�?}@��9@��@~�+@~V@~�R@~ȴ@~�y@~��@~5?@}�@}�T@}�T@}@}��@}��@}��@}��@}@}`B@|�/@|�j@|��@|j@|j@z=q@s�F@k�@c��@X��@Q�^@J^5@DZ@>�+@9G�@0A�@.{@(��@#33@�-@G�@��@�`@��@	�#@ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBI�BI�BI�BI�BI�BI�BJ�BK�BL�BM�BM�BM�BM�BM�BM�BN�BN�BN�BN�BN�BO�BO�BP�BP�BP�BP�BP�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BO�BL�BI�B5?B(�B�BhB��BB  B��B�fB�
BÖB�B��B�{B�oB�JBx�BS�B7LB#�B  B�NB��BȴB�RB�B��B�oB�B�B�B{�Bt�Be`BS�BK�BD�B@�B=qB;dB9XB49B1'B)�B�B
��B
�NB
�LB
�'B
�B
��B
�oB
�VB
�=B
�1B
�%B
� B
x�B
w�B
u�B
r�B
o�B
jB
cTB
]/B
Q�B
F�B
<jB
1'B
%�B
�B
�B

=B	��B	�B	�sB	�TB	�NB	�B	��B	��B	�^B	�'B	��B	��B	��B	�uB	�JB	�1B	�%B	�B	{�B	t�B	l�B	dZB	aHB	]/B	R�B	K�B	D�B	>wB	6FB	0!B	)�B	!�B	�B	�B	bB		7B	B�B�B�B�fB�TB�HB�5B�B�B�B��B��B��B��B��B��BŢBB��B�wB�}B��B��B�}B�qB�dB�RB�?B�3B�B��B��B��B��B��B��B�uB�hB�\B�VB�=B�+B�B�B�B}�By�Bw�Bv�Bu�Bt�Bs�Br�Bp�Bm�BjBhsBgmBffBdZBbNB`BB]/BZBW
BS�BR�BP�BO�BN�BL�BK�BJ�BI�BH�BG�BE�BD�BB�B@�B?}B>wB>wB>wB=qB=qB;dB:^B:^B9XB7LB7LB6FB5?B5?B49B33B2-B1'B1'B.B-B+B(�B'�B&�B%�B$�B#�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B"�B!�B"�B$�B$�B"�B"�B%�B&�B%�B$�B%�B&�B)�B+B+B+B+B,B.B.B0!B33B5?B7LB7LB7LB7LB9XB9XB:^B;dB;dB<jB<jB=qB>wB@�BC�BF�BF�BF�BN�BS�BS�BT�BVBW
BXBXBXB\)B]/B`BBe`BgmBhsBiyBk�Bl�Bl�Bm�Bp�Br�Bw�Bx�B{�B~�B�B�+B�+B�=B�\B�oB�uB��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�3B�^B�qB��BÖBɺB��B��B��B��B��B��B��B��B��B��B��B��B�5B�NB�NB�TB�ZB�mB�B�B�B��B��B��B��B��B��B��B	  B	B		7B	hB	{B	�B	�B	�B	"�B	"�B	#�B	&�B	)�B	-B	/B	1'B	6FB	7LB	;dB	?}B	D�B	G�B	I�B	L�B	M�B	N�B	N�B	O�B	O�B	O�B	P�B	N�B	K�B	K�B	N�B	R�B	ZB	^5B	aHB	dZB	hsB	iyB	iyB	hsB	gmB	jB	k�B	m�B	m�B	n�B	n�B	p�B	r�B	u�B	w�B	y�B	z�B	|�B	~�B	�B	�B	�B	�B	�%B	�%B	�%B	�+B	�1B	�=B	�DB	�\B	�bB	�hB	�hB	�hB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	��B	��B	�yB	�B
B
uB
�B
&�B
0!B
<jB
?}B
F�B
L�B
S�B
[#B
aHB
dZB
iyB
l�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BI�BI�BI�BI�BI�BI�BJ�BK�BL�BM�BM�BM�BM�BM�BM�BN�BN�BN�BN�BN�BO�BO�BP�BP�BP�BP�BP�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BO�BL�BI�B5#B(�B�BFB��B�B��B��B�EB��B�tB��B��B�UB�MB�&Bx�BS�B7(B#�B��B�)BεBȍB�,B��B��B�JB��B��B��B{�Bt�Be:BS�BK�BDvB@_B=NB;?B90B4B1 B)�BtB
��B
�+B
�)B
�B
��B
��B
�MB
�1B
�B
�B
�B
�B
x�B
w�B
u�B
r�B
oyB
j\B
c2B
]	B
Q�B
F�B
<GB
1B
%�B
�B
kB

B	��B	�B	�SB	�4B	�-B	��B	̬B	�dB	�@B	�	B	��B	��B	�sB	�XB	�+B	�B	�	B	��B	{�B	t�B	lmB	d;B	a,B	]B	R�B	K�B	D�B	>YB	6*B	0B	)�B	!�B	�B	pB	EB		B	 �B�B�sB�eB�MB�:B�.B�B�B��B��B��B��BˬB˯B̳BʪBňB�wB�jB�_B�cB�pB�jB�eB�XB�JB�:B�)B�B��B��B��B��B��B�vB�hB�]B�OB�GB�=B�'B�B�B��B��B}�By�Bw�Bv�Bu�Bt�Bs�Br�Bp�Bm{BjgBh^BgTBfMBdCBb7B`-B]BZBV�BS�BR�BP�BO�BN�BL�BK�BJ�BI�BH�BG�BE�BD�BBzB@lB?iB>`B>`B>aB=]B=\B;OB:HB:HB9BB77B76B60B5'B5)B4B3B2B1B1B-�B,�B*�B(�B'�B&�B%�B$�B#�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B"�B!�B"�B$�B$�B"�B"�B%�B&�B%�B$�B%�B&�B)�B*�B*�B*�B*�B+�B-�B-�B0	B3B5'B71B72B72B72B9?B9>B:DB;GB;KB<RB<QB=XB>]B@gBC|BF�BF�BF�BN�BS�BS�BT�BU�BV�BW�BW�BW�B\B]B`&BeEBgPBhXBi\BkhBlnBlnBmuBp�Br�Bw�Bx�B{�B~�B��B�B�B�B�?B�PB�UB�hB�oB�yB��B��B��B��B��B��B��B��B��B��B�B�B�<B�NB�`B�tBəBʟBʟBˣBϼB��B��B��B��B��B��B��B��B�B�/B�*B�.B�8B�HB�[B�kB�B��B��B��B��B��B��B��B��B	�B		B	AB	XB	nB	zB	�B	"�B	"�B	#�B	&�B	)�B	,�B	.�B	1 B	6B	7$B	;>B	?WB	DwB	G�B	I�B	L�B	M�B	N�B	N�B	O�B	O�B	O�B	P�B	N�B	K�B	K�B	N�B	R�B	Y�B	^B	a B	d0B	hMB	iQB	iPB	hJB	gDB	jVB	k\B	mjB	miB	noB	noB	p{B	r�B	u�B	w�B	y�B	z�B	|�B	~�B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�0B	�:B	�?B	�?B	�<B	�=B	�JB	�PB	�SB	�\B	�[B	�\B	�iB	�pB	��B	��B	��B	��B	��B	��B	��B	�XB	ϵB	�NB	�uB
�B
IB
�B
&�B
/�B
<=B
?OB
FxB
L�B
S�B
Z�B
aB
d*B
iJB
l]B
q|11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.39 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708282016053117082820160531170828  AO  ARCAADJP                                                                    20160108101702    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160108101702  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160108101702  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170828  IP                  G�O�G�O�G�O�                