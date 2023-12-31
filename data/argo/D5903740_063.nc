CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:37Z AOML 3.0 creation; 2016-06-01T00:08:15Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230837  20160531170816  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ?A   AO  4055_7112_063                   2C  D   APEX                            5374                            041511                          846 @��39D�1   @��3�o��@:�\(��d����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ?A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D	  D	� D
  D
� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3�fD4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dx� D�  D�@ D�� D���D�	�D�9�D�vfD���D��D�FfD��3D�ɚD�fD�C3DچfD�y�D� D�C3D� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�z�A=qA&=qAF=qAf=qA��A��A��A��A��A�Q�A�Q�A��B�\B	�\B�\B�\B!�\B)�\B1�\B9�\BA�\BI�\BQ�\BY�\Ba�\Bi�\Bq�\By�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮBĔ{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC c�Cc�Cc�Cc�Cc�C
c�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�C c�C"c�C$c�C&c�C(c�C*c�C,c�C.c�C0c�C2c�C4c�C6c�C8c�C:c�C<c�C>c�C@c�CBc�CDc�CFc�CHc�CJc�CLc�CNc�CPc�CRc�CTc�CVc�CXc�CZc�C\c�C^c�C`c�Cbc�Cdc�Cfc�Chc�Cjc�Clc�Cnc�Cpc�Crc�Ctc�Cvc�Cxc�Czc�C|c�C~c�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�%C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�>�C�>�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D�\D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3�\D4\D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dx��D�{D�L{D��{D��HD�D�FD���D��HD�HD�R�D���D��D��D�O�Dڒ�D��D�{D�O�D�{D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�dZA��!A�A�ĜA�A�A�A�A�ĜA�ĜA�ƨA�ȴA���A���A���A���A���A���A���A��
A��A���A���A��
A���A��
A���A��
A��A��#A��#A��#A��#A��/A��/A��/A��;A��A���A��RA�%A�
=A��9A��9A�A���A�1A�"�A���A��A�\)A�ĜA�C�A��wA��#A�;dA��A�;dA��DA��wA�7LA��yA�`BA��-A�33A��A�x�A�$�A���A��jA���A���A�A��hA�O�A�33A��A�=qA�p�A�&�A�^5A��#A��!A�n�A���A��/A�G�A��HA��A�S�A�C�A�-A���A��A��uA��A�^5A�hAA~v�A}�A}G�A{;dAv��Au�hAt1An�AkVAh=qAg;dAf1'Ad��Ac/Aa��A`A�A_hsA_"�A^jA]�#A[��AX  AT�AT  AS�hAR�AQ"�AO;dAM�AM�AL�!AKAIAI|�AI�AH��AH�AG�
AF��AF�+AF-AE�AE�FAEt�AD�AD1AC|�AC&�AB�AB��AA�hA>z�A>A<�!A:�A9�A9��A9&�A8Q�A8bA7;dA6Q�A5�hA5&�A4��A4�jA4�A3��A3?}A2-A1`BA0�+A0�A/�^A/�A/VA.��A-��A+��A*I�A)��A)dZA)�A(ĜA'�#A%�A%A$��A$r�A$A�A#��A"��A!�TA!��A!��A!�A ��A ��A��A�\A��A-A�7Ar�A��A��AbNAdZAdZA�uA^5A�FA&�A��A��A+A��A�FA
v�A	�-A�HAz�A  AA�A7LAVA|�A Z@�+@�E�@���@�V@�ƨ@��@��@��`@��D@��@�$�@�hs@�r�@���@���@�`B@�@�hs@�/@���@�@�j@��H@��@�@㝲@�`B@�b@�K�@��@���@��@�z�@��@�bN@֗�@Ձ@�Q�@��
@�\)@��@θR@�M�@��@�&�@�r�@�%@�|�@�@�V@�I�@��;@�K�@�x�@��@�o@�M�@���@���@�/@�t�@��/@��@�dZ@���@�5?@�%@���@�|�@�E�@��#@�x�@�7L@��/@��D@�1'@���@��P@�"�@���@���@�?}@��@��P@�K�@�+@��!@�J@�x�@�/@���@��/@�Z@�C�@���@�J@�?}@�(�@��@��P@�|�@�dZ@�S�@�33@�"�@��@���@�Ĝ@��9@��@���@��@���@�"�@���@�J@��7@�%@�Q�@��
@�;d@���@���@�M�@�5?@���@��@���@���@���@�$�@�{@���@�@��@�Q�@�ƨ@�l�@�n�@��T@��/@�j@�Z@�Q�@�A�@�(�@���@��w@���@�t�@�K�@�"�@���@���@�V@��^@���@��@�G�@���@���@���@��@���@���@�Ĝ@��j@�I�@�1@��m@���@���@�S�@�@��y@���@���@�M�@�{@��@��-@�p�@�/@��`@��D@�Z@��P@�C�@�o@�ȴ@��+@�n�@�V@���@�/@���@��@��`@���@���@��@�Q�@�9X@�(�@�1@���@�+@�o@��@�ȴ@���@�v�@�M�@�@��7@�hs@�hs@�X@�/@��@��@��9@��@�j@�Z@�Q�@�A�@��@|�@l�@�@~�R@~�+@~ff@~5?@}�T@}��@}�@}?}@}�@|��@|�@{S�@z�@z�!@z^5@z=q@z-@z�@y��@y��@yx�@yG�@y&�@x��@w�@w\)@w+@w
=@u?}@mV@g�@^�R@V5?@L�j@G�@E?}@=V@6��@1��@-�T@)��@&��@ Ĝ@�^@��@r�@�D@	%@�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�dZA��!A�A�ĜA�A�A�A�A�ĜA�ĜA�ƨA�ȴA���A���A���A���A���A���A���A��
A��A���A���A��
A���A��
A���A��
A��A��#A��#A��#A��#A��/A��/A��/A��;A��A���A��RA�%A�
=A��9A��9A�A���A�1A�"�A���A��A�\)A�ĜA�C�A��wA��#A�;dA��A�;dA��DA��wA�7LA��yA�`BA��-A�33A��A�x�A�$�A���A��jA���A���A�A��hA�O�A�33A��A�=qA�p�A�&�A�^5A��#A��!A�n�A���A��/A�G�A��HA��A�S�A�C�A�-A���A��A��uA��A�^5A�hAA~v�A}�A}G�A{;dAv��Au�hAt1An�AkVAh=qAg;dAf1'Ad��Ac/Aa��A`A�A_hsA_"�A^jA]�#A[��AX  AT�AT  AS�hAR�AQ"�AO;dAM�AM�AL�!AKAIAI|�AI�AH��AH�AG�
AF��AF�+AF-AE�AE�FAEt�AD�AD1AC|�AC&�AB�AB��AA�hA>z�A>A<�!A:�A9�A9��A9&�A8Q�A8bA7;dA6Q�A5�hA5&�A4��A4�jA4�A3��A3?}A2-A1`BA0�+A0�A/�^A/�A/VA.��A-��A+��A*I�A)��A)dZA)�A(ĜA'�#A%�A%A$��A$r�A$A�A#��A"��A!�TA!��A!��A!�A ��A ��A��A�\A��A-A�7Ar�A��A��AbNAdZAdZA�uA^5A�FA&�A��A��A+A��A�FA
v�A	�-A�HAz�A  AA�A7LAVA|�A Z@�+@�E�@���@�V@�ƨ@��@��@��`@��D@��@�$�@�hs@�r�@���@���@�`B@�@�hs@�/@���@�@�j@��H@��@�@㝲@�`B@�b@�K�@��@���@��@�z�@��@�bN@֗�@Ձ@�Q�@��
@�\)@��@θR@�M�@��@�&�@�r�@�%@�|�@�@�V@�I�@��;@�K�@�x�@��@�o@�M�@���@���@�/@�t�@��/@��@�dZ@���@�5?@�%@���@�|�@�E�@��#@�x�@�7L@��/@��D@�1'@���@��P@�"�@���@���@�?}@��@��P@�K�@�+@��!@�J@�x�@�/@���@��/@�Z@�C�@���@�J@�?}@�(�@��@��P@�|�@�dZ@�S�@�33@�"�@��@���@�Ĝ@��9@��@���@��@���@�"�@���@�J@��7@�%@�Q�@��
@�;d@���@���@�M�@�5?@���@��@���@���@���@�$�@�{@���@�@��@�Q�@�ƨ@�l�@�n�@��T@��/@�j@�Z@�Q�@�A�@�(�@���@��w@���@�t�@�K�@�"�@���@���@�V@��^@���@��@�G�@���@���@���@��@���@���@�Ĝ@��j@�I�@�1@��m@���@���@�S�@�@��y@���@���@�M�@�{@��@��-@�p�@�/@��`@��D@�Z@��P@�C�@�o@�ȴ@��+@�n�@�V@���@�/@���@��@��`@���@���@��@�Q�@�9X@�(�@�1@���@�+@�o@��@�ȴ@���@�v�@�M�@�@��7@�hs@�hs@�X@�/@��@��@��9@��@�j@�Z@�Q�@�A�@��@|�@l�@�@~�R@~�+@~ff@~5?@}�T@}��@}�@}?}@}�@|��@|�@{S�@z�@z�!@z^5@z=q@z-@z�@y��@y��@yx�@yG�@y&�@x��@w�@w\)@w+@w
=@u?}@mV@g�@^�R@V5?@L�j@G�@E?}@=V@6��@1��@-�T@)��@&��@ Ĝ@�^@��@r�@�D@	%@�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B�B/B49B5?B49B49B49B49B49B49B49B49B5?B5?B49B5?B5?B5?B5?B7LB7LB6FB6FB6FB6FB6FB6FB6FB7LB7LB7LB7LB7LB7LB7LB7LB7LB6FB49B1'B�B�NB��B�%Bs�Bl�BK�B\B%BB  B��B��B�B�mB�/BĜB�9B��B�{B�Bu�BP�B0!B&�B�BJB+B��B�B�/B��B�9B��B�oB� Bo�B]/BQ�BM�B>wB"�BbB
=B
��B
�;B
��B
ȴB
�jB
�-B
��B
�VB
s�B
^5B
VB
J�B
B�B
9XB
5?B
0!B
+B
#�B
bB	��B	�sB	�B	�LB	��B	�DB	�B	{�B	q�B	hsB	^5B	VB	Q�B	N�B	J�B	E�B	8RB	$�B	�B	{B	hB	PB	%B��B��B��B��B�B�B�B�yB�mB�fB�ZB�BB�BB�;B�5B�/B�)B�B�B�B�
B��B��B��BɺBƨBB�wB�jB�dB�XB�XB�XB�RB�?B�?B�9B�9B�3B�'B�!B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B�{B�hB�bB�\B�VB�JB�=B�1B�+B�%B�B�B�B|�Bx�Bu�Br�Bo�Bl�BiyBcTB\)BW
BO�BM�BM�BK�BK�BJ�BG�BB�B=qB9XB7LB5?B1'B,B(�B%�B"�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBoBoBbBbBbBbBbB\BVBJBDBDB
=B
=BDBDBDB
=B	7B	7B1B1B	7B	7B
=B
=B	7B	7BPBPBPBPBDB\BbBbBbB{B{BuB�B�B�B�B�B�B�B�B�B �B!�B!�B"�B$�B'�B'�B,B-B.B.B/B0!B1'B2-B2-B33B49B6FB8RB<jB>wB?}B?}BA�BC�BE�BF�BG�BG�BH�BL�BN�BQ�BT�BZB\)B]/B^5B^5B_;B_;B_;BdZBk�Bm�Bm�Bn�Bo�Bs�Bz�Bx�By�B� B�B�1B�JB�\B�uB��B��B��B��B��B��B��B��B�B�9B�9B�?B�FB�^B��BĜBŢB��B��B�B�/B�5B�5B�5B�;B�BB�NB�TB�ZB�fB�mB�sB�B�B�B�B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	+B		7B	JB	PB	VB	\B	oB	{B	�B	�B	�B	�B	�B	!�B	#�B	,B	.B	0!B	33B	5?B	6FB	6FB	=qB	A�B	C�B	C�B	D�B	D�B	F�B	G�B	H�B	I�B	I�B	J�B	M�B	Q�B	S�B	T�B	W
B	XB	YB	YB	]/B	`BB	aHB	aHB	aHB	bNB	cTB	dZB	ffB	hsB	iyB	iyB	jB	jB	n�B	p�B	p�B	r�B	u�B	v�B	v�B	x�B	y�B	z�B	{�B	|�B	|�B	}�B	~�B	�B	�%B	�+B	�7B	�=B	�=B	�DB	�DB	�JB	�VB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	�B	��B
bB
�B
�B
+B
5?B
<jB
@�B
F�B
I�B
P�B
ZB
_;B
e`B
jB
o�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��BgB/B4#B5&B4%B4#B4#B4#B4%B4%B4#B4!B5'B5'B4#B5'B5+B5'B5'B78B78B6.B6.B6.B60B60B60B60B76B74B76B76B76B74B74B76B76B61B4!B1B�B�2B��B�Bs�BloBK�B:BB�B��B��B��B�tB�MB�B�xB�B��B�WB��Bu�BP�B0 B&�BcB$BB��B�B�	B�cB�B��B�IB�BowB]
BQ�BM�B>SB"�B<B
B
��B
�B
ζB
ȏB
�EB
�B
��B
�2B
s�B
^B
U�B
J�B
BkB
99B
5B
/�B
*�B
#�B
BB	��B	�TB	��B	�-B	��B	�'B	��B	{�B	q�B	hYB	^B	U�B	Q�B	N�B	J�B	E�B	88B	$�B	uB	bB	PB	6B	B��B��B��B��B�B�kB�gB�aB�TB�LB�CB�*B�)B�!B�B�B�B�B��B��B��B��B��BͼBɢBƑB�uB�aB�OB�MB�?B�AB�?B�9B�'B�&B�$B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�|B�eB�RB�KB�GB�=B�2B�&B�B�B�B�B��B��B|�Bx�Bu�Br�Bo�BluBicBc>B\BV�BO�BM�BM�BK�BK�BJ�BG�BByB=\B9CB75B5)B1B+�B(�B%�B"�B �B�B�B|BuBuB�BiBiBbB[BxBtBkBSBLBBB>B?B2B2B2B1BNB*B$BBB,B
B
BB-BB
B	B	B B�B	B	B
B
B	 B	BB9B:BB+BDB1BKBJBcBJB@BhBsB{BhB�B�B�B�B�B �B!�B!�B"�B$�B'�B'�B+�B,�B-�B-�B/ B0B1B2B2B3B4 B6*B87B<QB>]B?cB?dBAlBC{BE�BF�BG�BG�BH�BL�BN�BQ�BT�BY�B\B]B^B^B_B_B_Bd=BkfBmqBmsBn|Bo�Bs�Bz�Bx�By�B�B��B�B�*B�?B�TB�`B�gB�uB�sB��B��B��B��B��B�B�B� B�&B�=B�aB�{BŀBͲB��B��B�B�B�B�B�B� B�-B�1B�6B�AB�IB�PB�_B�nB�B�B��B��B��B��B��B��B��B��B��B��B��B	�B	�B	�B	B		B	&B	+B	0B	8B	HB	UB	\B	lB	|B	�B	�B	!�B	#�B	+�B	-�B	/�B	3B	5B	6B	6B	=HB	AbB	CpB	CpB	DtB	DtB	FB	G�B	H�B	I�B	I�B	J�B	M�B	Q�B	S�B	T�B	V�B	W�B	X�B	X�B	]B	`B	aB	aB	aB	b#B	c*B	d3B	f<B	hJB	iNB	iPB	jVB	jUB	npB	p|B	p|B	r�B	u�B	v�B	v�B	x�B	y�B	z�B	{�B	|�B	|�B	}�B	~�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�,B	�1B	�9B	�EB	�^B	�jB	�oB	�vB	��B	�XB	ϲB	�_B	��B
7B
~B
�B
*�B
5B
<<B
@UB
FyB
I�B
P�B
Y�B
_B
e1B
jOB
omB
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.39 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708162016053117081620160531170816  AO  ARCAADJP                                                                    20140721230837    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230837  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230837  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170816  IP                  G�O�G�O�G�O�                