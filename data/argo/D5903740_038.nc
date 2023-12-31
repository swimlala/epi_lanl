CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:24Z AOML 3.0 creation; 2016-06-01T00:08:11Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230824  20160531170811  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               &A   AO  4055_7112_038                   2C  D   APEX                            5374                            041511                          846 @֌ww�1   @֌���@:,������c;C��%1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    &A   A   A   @�33@�  A   A   A@  A`  A~ffA�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dyy�D���D�I�D�|�D�ɚD�  D�S3D��fD��3D���D�@ D�y�Dǹ�D��D�@ D�y�D���D�	�D�I�D�p D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��\@�\)A�A#�AC�Ac�A�
=A�
=A��
A��
A��
A��
A��
A��
B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�BiQ�Bp�Bx�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�BШ�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C :�C:�C:�C:�C:�C
:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C :�C":�C$:�C&:�C(:�C*:�C,:�C.:�C0:�C2:�C4:�C6:�C8:�C::�C<:�C>:�C@:�CB:�CD:�CF:�CH:�CJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\:�C^:�C`:�Cb:�Cd:�Cf:�Ch:�Cj:�Cl:�Cn:�Cp:�Cr:�Ct:�Cv:�Cx:�Cz:�C|:�C~:�C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'RD'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy�RD�)D�P�D��)D���D�\D�Z�D���D��D� �D�G\D���D���D� �D�G\Dڀ�D��)D��D�P�D�w\D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�I�A�C�A�-A�JA�A�A�A�A�A���A���A���A���A���A��A��
A��jA��!A���A���A���A���A���A���A��uA��hA�n�A�hsA�hsA�dZA�\)A�VA�E�A�C�A�;dA�;dA�;dA�9XA�5?A�5?A�1'A�1'A�33A�/A�/A�/A�1'A�7LA�5?A�33A�-A�-A�-A�/A�/A�1'A�1'A�1'A�-A�+A�(�A�bA��`A��\A�VA�1A��9A�M�A��-A��A�C�A���A�VA���A�S�A�ĜA�&�A���A���A���A���A�~�A�|�A�dZA�bNA��A���A�v�A�33A��A���A��wA��mA�bA�oA�&�A��jA�M�A�|�A�bNA{S�AwVAv�Av��Av1'Aq�mAn{Ak?}Aj1Ag�Af(�Ae`BAd$�Ab�9A`9XA^�DA]x�AZ�jAXE�AW33AV��AV9XAU�-AUK�AT��AT �ATJAS��AS�TASdZARr�AO�TAL�AK�AJVAI�AI33AHr�AF�yAFA�AF$�AE�PAD�+AC�
AC7LAB$�AA|�A@JA>E�A;�TA:�A:~�A:z�A:-A7��A6��A61A4ffA3��A2��A1K�A0��A0�A/dZA.��A.=qA-G�A+�mA*��A*A)�
A)�A(��A'�A'%A&Q�A%�hA%�A$�\A$-A#x�A!�;A �A �uA z�Ax�A�mA��AbNA�-A�DAJA�-A&�AffA�
A�AȴA�9A�AhsA�A�#A�A��A�FA�^A�A`BA��A�AbNA�#A�jAp�A��A+A	dZA�A�#Ax�A33AA�Az�A�A��An�A��A �@��@�dZ@�?}@�S�@��7@�@�hs@�@�p�@�bN@띲@�ȴ@�?}@�@�?}@���@�hs@�j@�C�@�ff@݁@�  @�@���@�r�@���@���@�$�@ղ-@�7L@�j@���@�K�@җ�@��T@�7L@��/@��;@�-@́@�G�@�1'@�M�@ɩ�@ț�@�ƨ@�K�@���@�{@ř�@���@ģ�@ě�@�9X@Ý�@�
=@+@��#@�G�@��@�S�@���@��#@�/@��u@��@�&�@�j@�(�@��;@��@�^5@��@��j@��u@�  @�v�@��^@�x�@�&�@� �@��P@�n�@�`B@�bN@��m@��w@��@�V@���@��w@�"�@���@�^5@��7@��u@� �@��m@��@�ȴ@��!@��@��@��P@�C�@�"�@���@���@�v�@��^@�V@�Ĝ@�(�@�|�@���@�-@��@���@�p�@��@�z�@�  @�\)@��y@��R@���@��@���@��@��D@��@��@��@���@�K�@�"�@��y@���@�^5@��@���@�7L@��j@��@���@�o@�ȴ@��\@�@�p�@�?}@���@�bN@���@��w@�\)@��@��@���@�=q@�@�@��@�?}@���@�Z@��m@���@���@�t�@�+@��@���@��\@�^5@�E�@�$�@�@��^@���@�x�@�O�@�7L@��@�%@��@��`@�Ĝ@��9@��u@��@�bN@��@�ƨ@���@�l�@�C�@�o@��@��H@���@��!@�-@�J@��^@��h@�X@�G�@�/@�V@��`@���@�Ĝ@��9@��@���@��D@�j@�9X@�@��@��@\)@
=@~��@~v�@~E�@~@}�@|�@|�@|�@|Z@{�m@{�F@{��@{C�@z�@z�!@z=q@y�#@yx�@y7L@x�9@x1'@w�@w;d@w�@w;d@v��@u�@u�@uO�@u�-@uO�@t�j@tj@t1@s�F@p �@f�y@`A�@W;d@Q�^@L��@FE�@?|�@8��@3t�@/�w@+"�@%�@ 1'@S�@�w@�@ff@
^5@ff@dZ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�I�A�C�A�-A�JA�A�A�A�A�A���A���A���A���A���A��A��
A��jA��!A���A���A���A���A���A���A��uA��hA�n�A�hsA�hsA�dZA�\)A�VA�E�A�C�A�;dA�;dA�;dA�9XA�5?A�5?A�1'A�1'A�33A�/A�/A�/A�1'A�7LA�5?A�33A�-A�-A�-A�/A�/A�1'A�1'A�1'A�-A�+A�(�A�bA��`A��\A�VA�1A��9A�M�A��-A��A�C�A���A�VA���A�S�A�ĜA�&�A���A���A���A���A�~�A�|�A�dZA�bNA��A���A�v�A�33A��A���A��wA��mA�bA�oA�&�A��jA�M�A�|�A�bNA{S�AwVAv�Av��Av1'Aq�mAn{Ak?}Aj1Ag�Af(�Ae`BAd$�Ab�9A`9XA^�DA]x�AZ�jAXE�AW33AV��AV9XAU�-AUK�AT��AT �ATJAS��AS�TASdZARr�AO�TAL�AK�AJVAI�AI33AHr�AF�yAFA�AF$�AE�PAD�+AC�
AC7LAB$�AA|�A@JA>E�A;�TA:�A:~�A:z�A:-A7��A6��A61A4ffA3��A2��A1K�A0��A0�A/dZA.��A.=qA-G�A+�mA*��A*A)�
A)�A(��A'�A'%A&Q�A%�hA%�A$�\A$-A#x�A!�;A �A �uA z�Ax�A�mA��AbNA�-A�DAJA�-A&�AffA�
A�AȴA�9A�AhsA�A�#A�A��A�FA�^A�A`BA��A�AbNA�#A�jAp�A��A+A	dZA�A�#Ax�A33AA�Az�A�A��An�A��A �@��@�dZ@�?}@�S�@��7@�@�hs@�@�p�@�bN@띲@�ȴ@�?}@�@�?}@���@�hs@�j@�C�@�ff@݁@�  @�@���@�r�@���@���@�$�@ղ-@�7L@�j@���@�K�@җ�@��T@�7L@��/@��;@�-@́@�G�@�1'@�M�@ɩ�@ț�@�ƨ@�K�@���@�{@ř�@���@ģ�@ě�@�9X@Ý�@�
=@+@��#@�G�@��@�S�@���@��#@�/@��u@��@�&�@�j@�(�@��;@��@�^5@��@��j@��u@�  @�v�@��^@�x�@�&�@� �@��P@�n�@�`B@�bN@��m@��w@��@�V@���@��w@�"�@���@�^5@��7@��u@� �@��m@��@�ȴ@��!@��@��@��P@�C�@�"�@���@���@�v�@��^@�V@�Ĝ@�(�@�|�@���@�-@��@���@�p�@��@�z�@�  @�\)@��y@��R@���@��@���@��@��D@��@��@��@���@�K�@�"�@��y@���@�^5@��@���@�7L@��j@��@���@�o@�ȴ@��\@�@�p�@�?}@���@�bN@���@��w@�\)@��@��@���@�=q@�@�@��@�?}@���@�Z@��m@���@���@�t�@�+@��@���@��\@�^5@�E�@�$�@�@��^@���@�x�@�O�@�7L@��@�%@��@��`@�Ĝ@��9@��u@��@�bN@��@�ƨ@���@�l�@�C�@�o@��@��H@���@��!@�-@�J@��^@��h@�X@�G�@�/@�V@��`@���@�Ĝ@��9@��@���@��D@�j@�9X@�@��@��@\)@
=@~��@~v�@~E�@~@}�@|�@|�@|�@|Z@{�m@{�F@{��@{C�@z�@z�!@z=q@y�#@yx�@y7L@x�9@x1'@w�@w;d@w�@w;d@v��@u�@u�@uO�@u�-@uO�@t�j@tj@t1@s�F@p �@f�y@`A�@W;d@Q�^@L��@FE�@?|�@8��@3t�@/�w@+"�@%�@ 1'@S�@�w@�@ff@
^5@ff@dZ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��BɺBɺBɺB��BɺBɺBɺBɺBɺBȴBȴBǮBƨBŢBŢBĜBĜBĜBÖBĜBĜBÖB��BBBBBB��B��B��B�}B�}B�}B�wB�wB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�qB�jB�jB�dB�RB�9B�B��B��B��B�+BVB49B%�B�B1B��B�B�)BǮB��B�7Bt�Bp�Bl�Bl�BjBp�BcTBZBN�B33B�5B�By�B`BB2-B+B
�;B
��B
ffB
G�B
1'B
%B	�ZB	�BB	�/B	��B	�3B	�{B	�B	v�B	gmB	ZB	S�B	I�B	?}B	/B	"�B	�B	1B��B��B�B�B�B�B�B�B�B�B�B�sB�fB�/B��B��BŢBB�}B�dB�LB�wBBǮBŢB��B�wB�dB��B�LB��B��B��B�B�3B�LB�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�VB�JB�DB�7B�+B�B� Bx�Bt�Bt�Br�Bm�BgmBcTBcTBdZBcTBaHB_;B]/BYBW
BS�BR�BQ�BM�BJ�BE�BB�BA�BA�BA�BA�BA�B@�B?}B>wB<jB:^B7LB49B1'B/B,B)�B'�B%�B#�B�B�B�B�B�B�B�BuBbBVBPBJBJB
=B	7BJB
=B	7B+B%B%BBBBBBBBB%B+B+B%B+B1B1B1B	7B	7B	7B	7B
=B
=B
=B
=BPBPBJBPB\B\BhBuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B#�B(�B+B,B,B,B0!B5?B6FB6FB8RB<jB>wB?}B?}BB�BD�BG�BJ�BM�BO�BP�BQ�BT�BXBZBZBZB[#B^5BaHBcTBdZBgmBjBjBl�Bt�By�B{�B|�B}�B~�B� B�B�7B�=B�PB�bB�{B��B��B��B��B��B��B��B��B�B�B�9B�XB�XB�dB�jB�jB��BBBŢBƨBȴB��B��B��B��B�B�B�B�BB�TB�fB�sB�B�B�B�B��B��B��B	  B	B	B	%B		7B	DB	PB	\B	hB	�B	�B	�B	�B	�B	 �B	#�B	%�B	'�B	)�B	,B	-B	/B	0!B	2-B	33B	6FB	8RB	9XB	;dB	<jB	=qB	=qB	>wB	?}B	@�B	A�B	B�B	E�B	G�B	I�B	J�B	L�B	N�B	P�B	P�B	Q�B	R�B	VB	W
B	ZB	[#B	^5B	^5B	_;B	aHB	cTB	dZB	dZB	e`B	e`B	ffB	gmB	iyB	jB	m�B	n�B	p�B	r�B	t�B	w�B	x�B	y�B	{�B	~�B	�B	�B	�1B	�=B	�DB	�JB	�PB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�LB	�B	�B
B
VB
�B
"�B
-B
6FB
>wB
C�B
I�B
O�B
W
B
\)B
aHB
ffB
k�B
p�B
u�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B˷B˺B˼BʱBɫBɫBɫBʯBɫBɭBɫBɫBɭBȪBȣBǠBƚBőBœBČBďBďBÆBďBČBÊB�yB�~BBB�|BB�wB�{B�uB�jB�lB�lB�fB�iB�bB�bB�bB�bB�`B�bB�bB�bB�bB�bB�`B�bB�bB�`B�`B�`B�bB�bB�\B�\B�VB�BB�*B�B��B��B�xB�BU�B4)B%�B~BB��B�xB�BǝB��B�Bt�Bp�BlwBlwBjiBp�Bc?BZBN�B3B�B��By�B`,B2BB
�%B
��B
fUB
G�B
1B
B	�LB	�2B	�"B	��B	�&B	�rB	� B	v�B	gaB	ZB	S�B	I�B	?sB	/B	"�B	�B	'B��B��B�B�B�B�B�~B�}B�~B�}B�xB�mB�`B�+B��BʻBŜBB�xB�^B�HB�qBBǨBŞB��B�pB�`B�|B�EB��B��B��B�B�.B�GB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�}B�vB�hB�PB�EB�@B�2B�)B�B�Bx�Bt�Bt�Br�Bm�BglBcPBcRBdUBcQBaEB_5B]*BYBWBS�BR�BQ�BM�BJ�BE�BB�BA�BA�BA�BA�BA�B@~B?|B>uB<hB:\B7JB46B1%B/B,B)�B'�B%�B#�B�B�B�B�B�B�BkBrBFB:B2B,B,B
!B	B,B
!B	BBB	BB�BBBBB�B�BB*BBBBBBB	B	5B	5B	5B
!B
 B
 B
 BNBNB-B2BXBYBfBrB[B|B~BjB�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B#�B(�B*�B, B+�B,B0B58B6>B6?B8NB<cB>pB?vB?xBB�BD�BG�BJ�BM�BO�BP�BQ�BT�BXBZBZBZB[B^-BaCBcMBdSBgdBjyBjwBl�Bt�By�B{�B|�B}�B~�B�B�B�,B�1B�DB�WB�nB��B��B��B��B��B��B��B��B� B�	B�,B�LB�IB�XB�\B�_B�uBBBŕBƚBȦB��B��B��B��B��B�B�B�4B�GB�VB�bB�xB�B�B�B��B��B��B��B	�B	B	B		'B	3B	AB	LB	YB	nB	�B	�B	�B	�B	 �B	#�B	%�B	'�B	)�B	+�B	,�B	/B	0B	2B	3!B	64B	8?B	9FB	;RB	<XB	=_B	=]B	>cB	?mB	@rB	AwB	B}B	E�B	G�B	I�B	J�B	L�B	N�B	P�B	P�B	Q�B	R�B	U�B	V�B	ZB	[B	^!B	^!B	_)B	a6B	cAB	dJB	dHB	eJB	eJB	fRB	gYB	ifB	jjB	mB	n�B	p�B	r�B	t�B	w�B	x�B	y�B	{�B	~�B	��B	��B	�B	�'B	�/B	�3B	�>B	�AB	�LB	�XB	�hB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�5B	��B	�{B
�B
=B
oB
"�B
,�B
6,B
>^B
C~B
I�B
O�B
V�B
\B
a/B
fJB
kkB
p�B
u�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.23 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708112016053117081120160531170811  AO  ARCAADJP                                                                    20140721230824    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230824  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230824  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170811  IP                  G�O�G�O�G�O�                