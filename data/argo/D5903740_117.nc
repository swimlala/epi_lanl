CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-07T02:15:22Z AOML 3.0 creation; 2016-06-01T00:08:25Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150607021522  20160531170825  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               uA   AO  4055_7112_117                   2C  D   APEX                            5374                            041511                          846 @�V|P��1   @�V|� @;���R�d6V�u1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    uA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dyy�D��D�S3D�s3D���D�fD�@ D�� D�ٚD��D�L�D�p D��fD�  D�<�Dډ�D��3D��D�FfD�3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��G@�z�A=qA&=qAF=qAf=qA��A��A��A��A��A��A��A��B�\B	�\B�\B�\B!�\B)�\B1�\B9�\BA�\BI�\BQ�\BY�\Ba�\Bi�\Bq�\By�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC c�Cc�Cc�Cc�Cc�C
c�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�C c�C"c�C$c�C&c�C(c�C*c�C,c�C.c�C0c�C2c�C4c�C6c�C8c�C:c�C<c�C>c�C@c�CBc�CDc�CFc�CHc�CJc�CLc�CNc�CPc�CRc�CTc�CVc�CXc�CZc�C\c�C^c�C`c�Cbc�Cdc�Cfc�Chc�Cjc�Clc�Cnc�Cpc�Crc�Ctc�Cvc�Cxc�Czc�C|c�C~c�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�%C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt��Dy��D�)HD�_�D��D��HD��D�L{D��{D��D�HD�YHD�|{D���D�{D�IHDږD��D�&D�R�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A�?}A� �A��/A�;dA��A�t�A�;dA��\A��\A��PA�
=A�XA�p�A�oA���A�A�E�A���A��A�O�A��RA�ZA�(�A��A�Q�A�VA��A��A���A��HA��mA�%A�bNA���A�G�A��FA�+A�-A���A�A� �A�~�A���A��#A��mA��hA�v�A�(�A���A���A���A� �A�VA��HA�G�A��FA�?}A���A�&�A��A�ffA�
=A���A�O�A��A�|�A�
=A��/A���A�x�A��jA�/A��/A���A���A�ZA�"�A��A��hA���A��A���A���A��A�bNA� �A���A���A�`BA�  A��;A��9A�v�A�jA�dZA�C�A��A��FA��+A�/A��jA��A���A�M�A�%A�v�A�S�A�&�A��
A�v�A��;A�bA���A�l�A�(�A��A�ƨA��hA�;dA��
A��DA��A���A�l�A�
=A�A~��A}�#A|1'AyƨAxM�Aw�;Aw��AwƨAw��Av�AuƨAr��Aql�AodZAl��Ah�jAg`BAe�TAdM�Aa
=A`$�A_��A_A^n�A]��AZn�AYVAW�hAUC�AT$�AS&�AR�AR�+AR{AQ��AQO�AQ+AP�DAP=qAPJAO��AN��AMAMVALQ�AK��AH�yAG��AG��AG7LAFVAEC�AD��AC�PACoAAt�A@~�A@Q�A=�7A;��A;C�A:�9A:M�A9+A7"�A6I�A4�A3�hA1�wA17LA0ĜA0{A.�A,��A+x�A*��A*A�A)�wA(�A( �A'\)A'&�A&bNA%G�A$�A#�FA#�hA#`BA#7LA"ZA"(�A!�7AG�AM�A%At�A�A\)A
=A�RA1'A��A�#A��A�PAK�An�AhsAVA{A?}AĜA��A/AĜA��A5?A��A\)A�A
�DA
1A	��A	O�A	&�A	
=A~�AE�A=qA�A�PA
=AĜA-A?}AAVA��Az�AA�PA ȴ@���@�X@��D@�Z@�b@�7L@�%@�ƨ@�R@���@@�hs@�@�u@�@�r�@�bN@�bN@�\)@�J@�Q�@�;d@�E�@��`@���@��@�1'@�|�@��`@���@��@٩�@�r�@��@��y@Ѻ^@�V@��
@�7L@��#@��
@ēu@�n�@��#@��/@��w@��@��@�b@�t�@�ȴ@�$�@��@�V@��@�ƨ@�l�@�S�@�o@��@���@�=q@��#@�G�@��@��w@�"�@��@���@��-@��@�Z@�l�@��H@�-@��7@�/@��D@��@��@�ƨ@���@�@�@�/@�1@��@�-@�X@���@��@��`@��`@��`@���@�I�@���@��R@�$�@��#@���@�O�@��@�1'@� �@�b@��@�ƨ@�l�@��y@���@��@��m@���@�=q@��7@��@��`@�Ĝ@��D@�1'@��@�33@��y@���@��+@�^5@��#@�hs@�%@���@��u@��@�z�@�A�@�b@��
@�t�@���@�5?@��^@��^@�bN@��P@�o@���@�5?@���@��^@�@���@�X@���@���@��@���@�z�@��m@�|�@�33@�
=@��!@�M�@�5?@��@��T@��#@��#@��#@��#@��#@��#@���@��@�1@�l�@�"�@���@��@�`B@���@��9@�j@�A�@� �@��m@�;d@�~�@�{@��@�@��@���@�j@�A�@�(�@�(�@�(�@� �@� �@�1@�  @�;@��@�w@��@;d@~5?@}O�@{�@zM�@y��@y�7@yG�@yx�@x��@x1'@w�@w�P@s"�@g�@`bN@XbN@S��@K"�@Dz�@=�-@:�@5O�@1G�@+"�@$��@ �9@��@E�@�@��@��@�@�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A�?}A� �A��/A�;dA��A�t�A�;dA��\A��\A��PA�
=A�XA�p�A�oA���A�A�E�A���A��A�O�A��RA�ZA�(�A��A�Q�A�VA��A��A���A��HA��mA�%A�bNA���A�G�A��FA�+A�-A���A�A� �A�~�A���A��#A��mA��hA�v�A�(�A���A���A���A� �A�VA��HA�G�A��FA�?}A���A�&�A��A�ffA�
=A���A�O�A��A�|�A�
=A��/A���A�x�A��jA�/A��/A���A���A�ZA�"�A��A��hA���A��A���A���A��A�bNA� �A���A���A�`BA�  A��;A��9A�v�A�jA�dZA�C�A��A��FA��+A�/A��jA��A���A�M�A�%A�v�A�S�A�&�A��
A�v�A��;A�bA���A�l�A�(�A��A�ƨA��hA�;dA��
A��DA��A���A�l�A�
=A�A~��A}�#A|1'AyƨAxM�Aw�;Aw��AwƨAw��Av�AuƨAr��Aql�AodZAl��Ah�jAg`BAe�TAdM�Aa
=A`$�A_��A_A^n�A]��AZn�AYVAW�hAUC�AT$�AS&�AR�AR�+AR{AQ��AQO�AQ+AP�DAP=qAPJAO��AN��AMAMVALQ�AK��AH�yAG��AG��AG7LAFVAEC�AD��AC�PACoAAt�A@~�A@Q�A=�7A;��A;C�A:�9A:M�A9+A7"�A6I�A4�A3�hA1�wA17LA0ĜA0{A.�A,��A+x�A*��A*A�A)�wA(�A( �A'\)A'&�A&bNA%G�A$�A#�FA#�hA#`BA#7LA"ZA"(�A!�7AG�AM�A%At�A�A\)A
=A�RA1'A��A�#A��A�PAK�An�AhsAVA{A?}AĜA��A/AĜA��A5?A��A\)A�A
�DA
1A	��A	O�A	&�A	
=A~�AE�A=qA�A�PA
=AĜA-A?}AAVA��Az�AA�PA ȴ@���@�X@��D@�Z@�b@�7L@�%@�ƨ@�R@���@@�hs@�@�u@�@�r�@�bN@�bN@�\)@�J@�Q�@�;d@�E�@��`@���@��@�1'@�|�@��`@���@��@٩�@�r�@��@��y@Ѻ^@�V@��
@�7L@��#@��
@ēu@�n�@��#@��/@��w@��@��@�b@�t�@�ȴ@�$�@��@�V@��@�ƨ@�l�@�S�@�o@��@���@�=q@��#@�G�@��@��w@�"�@��@���@��-@��@�Z@�l�@��H@�-@��7@�/@��D@��@��@�ƨ@���@�@�@�/@�1@��@�-@�X@���@��@��`@��`@��`@���@�I�@���@��R@�$�@��#@���@�O�@��@�1'@� �@�b@��@�ƨ@�l�@��y@���@��@��m@���@�=q@��7@��@��`@�Ĝ@��D@�1'@��@�33@��y@���@��+@�^5@��#@�hs@�%@���@��u@��@�z�@�A�@�b@��
@�t�@���@�5?@��^@��^@�bN@��P@�o@���@�5?@���@��^@�@���@�X@���@���@��@���@�z�@��m@�|�@�33@�
=@��!@�M�@�5?@��@��T@��#@��#@��#@��#@��#@��#@���@��@�1@�l�@�"�@���@��@�`B@���@��9@�j@�A�@� �@��m@�;d@�~�@�{@��@�@��@���@�j@�A�@�(�@�(�@�(�@� �@� �@�1@�  @�;@��@�w@��@;d@~5?@}O�@{�@zM�@y��@y�7@yG�@yx�@x��@x1'@w�@w�P@s"�@g�@`bN@XbN@S��@K"�@Dz�@=�-@:�@5O�@1G�@+"�@$��@ �9@��@E�@�@��@��@�@�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B�{B�uB�JB�B~�Bz�Bx�Bu�Bt�Bp�BiyBffBcTBYBVBQ�BP�BO�B]/B^5B_;BZBK�BH�B>wB:^BF�BG�B?}B9XB@�BB�B6FB,B%�B/B<jB0!B)�B(�B#�B!�B�B�BVB��B��B�sB�/B��BǮB�wB�LB�B��B��B��B�\B�+B}�By�Bt�Bo�BaHBT�BM�BH�B<jB6FB2-B.B'�B�B\BDBB��B�B�B�TB��B��BƨBĜBÖB��B��B��B�wB�^B�FB�'B�B��B�bB�+B� Bx�Bo�Bl�BhsBaHBXBK�B=qB6FB/B(�B$�B�B�BbBDBB
�B
�NB
�#B
��B
��B
B
�LB
��B
�JB
�%B
�%B
�JB
�VB
�DB
�%B
v�B
XB
B�B
'�B
JB	�yB	�/B	��B	��B	��B	��B	��B	��B	��B	�VB	� B	y�B	q�B	hsB	bNB	]/B	[#B	YB	W
B	T�B	S�B	R�B	P�B	N�B	M�B	J�B	G�B	B�B	>wB	:^B	49B	'�B	"�B	 �B	�B	�B	{B	bB	DB	B��B��B��B�B�NB�;B�)B�B��BȴBĜB��B�jB�LB�?B�3B�B�B��B��B��B��B��B��B��B��B�uB�bB�PB�JB�DB�=B�=B�7B�+B�%B�B�B|�Bx�Bt�Bq�Bp�Bp�Bo�Bn�Bm�Bm�Bm�Bm�Bl�Bk�BhsBcTB^5B[#BXBT�BR�BQ�BO�BL�BK�BK�BK�BJ�BI�BI�BI�BH�BG�BF�BE�BE�BC�BB�BA�B?}B<jB9XB6FB5?B49B33B2-B1'B/B-B+B+B(�B%�B"�B!�B�B�B�B�B�B�B�B�B�B�B�BuB{B{BuBuBuBuBuBoBbBoBoBoBoBoBoB�B�B�B�B�B�B�B�B#�B#�B$�B&�B&�B)�B-B.B/B/B0!B1'B2-B49B5?B49B49B49B5?B5?B6FB7LB8RB:^B<jB<jB<jB>wB?}B@�BC�BD�BF�BH�BI�BJ�BL�BL�BL�BL�BO�BYBYB\)B]/B]/B`BBdZBffBk�Bn�Bo�Bp�Br�Bt�Bz�B|�B}�B}�B� B�B�B�B�B�B�%B�+B�1B�PB�\B�uB��B��B��B��B��B��B��B��B��B�B�B�-B�9B�9B�LB�^B�jB�wB�}B�}B�}B��B��B��BÖBĜBǮB��B��B��B�
B�B�)B�5B�HB�HB�HB�HB�NB�fB�yB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B��B	B	
=B	JB	DB	DB	
=B	\B	oB	uB	{B	�B	�B	�B	�B	�B	�B	!�B	#�B	(�B	-B	.B	/B	0!B	0!B	0!B	0!B	0!B	1'B	1'B	2-B	2-B	2-B	33B	49B	8RB	9XB	>wB	C�B	E�B	F�B	G�B	K�B	P�B	R�B	T�B	[#B	iyB	�JB	�B	��B	�/B	��B

=B
�B
�B
'�B
1'B
9XB
C�B
J�B
R�B
W
B
]/B
cTB
iyB
l�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B�rB�_B�WB�,B�B~�Bz�Bx�Bu�Bt�Bp�Bi[BfDBc4BX�BU�BQ�BP�BO�B]B^B_BY�BK�BH�B>YB:9BF�BG�B?^B9:B@aBBkB6#B+�B%�B.�B<JB0 B)�B(�B#�B!�B�BuB4B��B��B�RB�B��BǇB�WB�'B��B��B��B�`B�:B�B}�By�Bt�BozBa"BT�BM�BH�B<GB6!B2B-�B'�B{B4BB�B��B�rB�_B�,B��B̧BƃB�tB�qB�_B�[B�]B�PB�8B� B��B��B��B�;B�B�Bx�BoxBleBhPBaBW�BK�B=KB6B.�B(�B$�B�BmB=BB�B
�~B
�,B
��B
��B
��B
�jB
�(B
��B
�%B
�B
�B
�'B
�1B
�!B
�B
v�B
W�B
BnB
'�B
&B	�XB	�B	ϾB	�dB	��B	��B	��B	��B	�jB	�7B	�B	y�B	q�B	hWB	b1B	]B	[B	X�B	V�B	T�B	S�B	R�B	P�B	N�B	M�B	J�B	G�B	BtB	>[B	:BB	4 B	'�B	"�B	 �B	�B	�B	`B	HB	+B	B��B��B��B�gB�6B�!B�B��B��BțBăB�kB�QB�5B�)B�B�B��B��B��B��B��B��B��B�uB�jB�]B�KB�9B�4B�-B�'B�%B�#B�B�B��B��B|�Bx�Bt�Bq�Bp�Bp�Bo�Bn�BmyBm{Bm{Bm|BlsBkmBh`Bc>B^B[BW�BT�BR�BQ�BO�BL�BK�BK�BK�BJ�BI�BI�BI�BH�BG�BF�BE�BE�BCBBzBArB?hB<TB9BB6/B5(B4#B3B1�B1B/B,�B*�B*�B(�B%�B"�B!�B�B�BzB�B�B�B�B�B�B�BwB_BeBKB]B]B`B_BAB?BMBZB>B=BYBWBYBUB[BUBrBSBnBpB�B#�B#�B$�B&�B&�B)�B,�B-�B/B/B0	B1B2B4B5'B4B4 B4 B5&B5%B6.B74B87B:EB<QB<QB<QB>[B?cB@gBC}BD�BF�BH�BI�BJ�BL�BL�BL�BL�BO�BX�BX�B\B]B]B`&Bd:BfJBkfBn{Bo�Bp�Br�Bt�Bz�B|�B}�B}�B�B��B��B�B�B�B�B�B�B�1B�;B�TB�fB��B��B��B��B��B��B��B��B��B��B�
B�B�B�+B�>B�HB�UB�ZB�]B�[B�cB�iB�iB�sB�}BǉB˥BʟB��B��B��B�B�B�$B�$B�$B�$B�-B�DB�VB�\B�`B�bB�tB�{B��B��B�B�B��B��B��B��B��B��B��B��B��B��B	�B	
B	"B	B	B	
B	7B	GB	NB	VB	\B	YB	bB	aB	�B	�B	!�B	#�B	(�B	,�B	-�B	.�B	/�B	/�B	/�B	/�B	/�B	1 B	1B	2B	2B	2B	3B	4B	8*B	91B	>OB	CmB	EzB	F�B	G�B	K�B	P�B	R�B	T�B	Z�B	iPB	�B	��B	ʖB	�B	��B

B
WB
�B
'�B
0�B
9*B
CgB
J�B
R�B
V�B
\�B
c%B
iLB
l\B
om111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.39 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708252016053117082520160531170825  AO  ARCAADJP                                                                    20150607021522    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150607021522  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150607021522  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170825  IP                  G�O�G�O�G�O�                