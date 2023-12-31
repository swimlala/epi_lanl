CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-17T02:15:53Z AOML 3.0 creation; 2016-06-01T00:08:24Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150417021553  20160531170824  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               pA   AO  4055_7112_112                   2C  D   APEX                            5374                            041511                          846 @�I�|5 1   @�I�S�@:�7KƧ��d:z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    pA   A   A   @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
y�D
��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy� D�3D�I�D���D��fD�fD�C3D�Y�D�� D�3D�C3D���Dǹ�D���D�<�Dڃ3D�� D��D�9�D�ffD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��G@�z�A��A&=qAF=qAf=qA��A��A��A��A��A��A��A��B�\B	�\B�\B�\B!�\B)�\B1�\B9�\BA�\BI�\BQ�\BY�\Ba�\Bi�\Bq�\By�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC c�Cc�Cc�Cc�Cc�C
c�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�C c�C"c�C$c�C&c�C(c�C*c�C,c�C.c�C0c�C2c�C4c�C6c�C8c�C:c�C<c�C>c�C@c�CBc�CDc�CFc�CHc�CJc�CLc�CNc�CPc�CRc�CTc�CVc�CXc�CZc�C\c�C^c�C`c�Cbc�Cdc�Cfc�Chc�Cjc�Clc�Cnc�Cpc�Crc�Ctc�Cvc�Cxc�Czc�C|c�C~c�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy��D��D�VD��HD���D�"�D�O�D�fD��{D��D�O�D��D��D�	HD�IHDڏ�D��{D�HD�FD�r�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�
=A�JA�  A��A��mA��yA��A��A���A�A��^A��!A���A��PA��A�dZA�7LA��A�K�A�r�A��A�;dA��TA��9A�G�A�l�A�%A�  A���A� �A�ȴA��A���A���A�dZA�v�A��9A��A�"�A��hA�\)A�
=A���A�z�A��A�n�A��mA�33A��uA�=qA���A��A��A��mA�C�A��A��mA�7LA���A�jA�A�A�bNA�`BA�XA�bA���A��FA�A�z�A�"�A�~�A���A�TA}��A{�Ay/Aw�
AwoAv�Av�jAv��Avn�Au��At=qAq��Ao�Amt�AlĜAjn�Ag��Ag|�Afz�Ac��AadZAaoA`�A`��A`VA_��A^��A\��AZ�jAZM�AZ(�AZ�AZ�AZJAZ1AY�#AXA�AV-AT�HASK�ARĜARbAQ+APz�AO�AOVAN��ANA�ANbAM�mAM��ALjAJ�AJ(�AI��AI�AI�#AIƨAI�FAIdZAG��AF5?AEt�AD�\AC�ABffAA�;AA;dA@��A?ƨA?33A>�!A<�A:n�A9�TA8�DA7hsA6�!A6A�A6{A5A5�A5S�A533A4�/A4v�A4�A3��A2A�A1l�A/x�A.(�A-A-��A-33A,��A,��A+�mA+|�A*��A*^5A)S�A'��A'%A&M�A%�mA$�A$��A$bA#\)A"bNA!�;A!��A!O�A ��A =qA��A��A;dA�HA��A^5A�wA��A5?AƨAC�A�A��A�mAVA�PAM�A��AȴA(�A��A�\AjA"�A�A+Av�A
��A
1A	�wA	�A	C�A	
=A��AM�A�A��@���@���@�Ĝ@��m@�|�@�S�@���@���@�&�@� �@�\)@��y@��#@��@�@���@�9@�{@�X@�?}@�9@�\)@���@��
@�ȴ@�b@�M�@���@ۮ@�+@�ȴ@ڗ�@ٺ^@�C�@���@� �@ӝ�@�
=@���@���@���@Ұ!@ҏ\@�v�@�E�@��T@с@���@�z�@�C�@��@�(�@�$�@�/@Ȭ@�Q�@�ƨ@��@�M�@��@ź^@�&�@�z�@� �@��m@���@ÍP@�l�@�C�@�J@�1@��H@��@��@�1'@�"�@�M�@���@���@���@�C�@�{@��@��9@�1'@��@�o@�{@��j@�1@��@�|�@�l�@�dZ@�;d@��\@�9X@�dZ@�+@��H@�ȴ@���@�x�@�`B@�G�@��j@��@�~�@��7@��@��9@���@�p�@�l�@��@���@���@��!@�n�@�E�@�{@���@���@��7@�p�@�O�@�%@���@�Q�@��@�S�@��y@���@��#@�z�@���@�33@��@��9@�z�@�Z@�b@��m@�+@�ȴ@�v�@�M�@��@��@�`B@���@�1'@��m@��@��P@�S�@�;d@�+@�
=@��y@�ȴ@�~�@�V@��@�X@���@��9@�j@�Q�@�I�@� �@��@���@��@��@�t�@�K�@���@��+@�v�@�ff@�M�@�$�@�@���@���@��h@��7@��@�p�@�7L@�V@��9@�A�@��w@���@���@�t�@��y@���@��\@�ff@�=q@�@��@���@��^@���@���@�p�@�`B@�O�@�/@��@��j@���@��D@��@�z�@�r�@�bN@�1'@��@�1@|�@;d@�@
=@~�y@~�@~�+@~@}p�@|��@|(�@z��@y�@y��@y�7@y�7@y�7@y�7@y�7@yhs@y7L@x�`@xbN@xb@w��@w\)@v�@vff@v@u��@u�@u`B@u/@t�@t�@t�@tj@s�
@q&�@j�!@d�D@_�@X  @P�9@HA�@?�;@;�
@7�@2�@.�R@(�@"�@��@  @�y@	��@z�@%?�p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�
=A�JA�  A��A��mA��yA��A��A���A�A��^A��!A���A��PA��A�dZA�7LA��A�K�A�r�A��A�;dA��TA��9A�G�A�l�A�%A�  A���A� �A�ȴA��A���A���A�dZA�v�A��9A��A�"�A��hA�\)A�
=A���A�z�A��A�n�A��mA�33A��uA�=qA���A��A��A��mA�C�A��A��mA�7LA���A�jA�A�A�bNA�`BA�XA�bA���A��FA�A�z�A�"�A�~�A���A�TA}��A{�Ay/Aw�
AwoAv�Av�jAv��Avn�Au��At=qAq��Ao�Amt�AlĜAjn�Ag��Ag|�Afz�Ac��AadZAaoA`�A`��A`VA_��A^��A\��AZ�jAZM�AZ(�AZ�AZ�AZJAZ1AY�#AXA�AV-AT�HASK�ARĜARbAQ+APz�AO�AOVAN��ANA�ANbAM�mAM��ALjAJ�AJ(�AI��AI�AI�#AIƨAI�FAIdZAG��AF5?AEt�AD�\AC�ABffAA�;AA;dA@��A?ƨA?33A>�!A<�A:n�A9�TA8�DA7hsA6�!A6A�A6{A5A5�A5S�A533A4�/A4v�A4�A3��A2A�A1l�A/x�A.(�A-A-��A-33A,��A,��A+�mA+|�A*��A*^5A)S�A'��A'%A&M�A%�mA$�A$��A$bA#\)A"bNA!�;A!��A!O�A ��A =qA��A��A;dA�HA��A^5A�wA��A5?AƨAC�A�A��A�mAVA�PAM�A��AȴA(�A��A�\AjA"�A�A+Av�A
��A
1A	�wA	�A	C�A	
=A��AM�A�A��@���@���@�Ĝ@��m@�|�@�S�@���@���@�&�@� �@�\)@��y@��#@��@�@���@�9@�{@�X@�?}@�9@�\)@���@��
@�ȴ@�b@�M�@���@ۮ@�+@�ȴ@ڗ�@ٺ^@�C�@���@� �@ӝ�@�
=@���@���@���@Ұ!@ҏ\@�v�@�E�@��T@с@���@�z�@�C�@��@�(�@�$�@�/@Ȭ@�Q�@�ƨ@��@�M�@��@ź^@�&�@�z�@� �@��m@���@ÍP@�l�@�C�@�J@�1@��H@��@��@�1'@�"�@�M�@���@���@���@�C�@�{@��@��9@�1'@��@�o@�{@��j@�1@��@�|�@�l�@�dZ@�;d@��\@�9X@�dZ@�+@��H@�ȴ@���@�x�@�`B@�G�@��j@��@�~�@��7@��@��9@���@�p�@�l�@��@���@���@��!@�n�@�E�@�{@���@���@��7@�p�@�O�@�%@���@�Q�@��@�S�@��y@���@��#@�z�@���@�33@��@��9@�z�@�Z@�b@��m@�+@�ȴ@�v�@�M�@��@��@�`B@���@�1'@��m@��@��P@�S�@�;d@�+@�
=@��y@�ȴ@�~�@�V@��@�X@���@��9@�j@�Q�@�I�@� �@��@���@��@��@�t�@�K�@���@��+@�v�@�ff@�M�@�$�@�@���@���@��h@��7@��@�p�@�7L@�V@��9@�A�@��w@���@���@�t�@��y@���@��\@�ff@�=q@�@��@���@��^@���@���@�p�@�`B@�O�@�/@��@��j@���@��D@��@�z�@�r�@�bN@�1'@��@�1@|�@;d@�@
=@~�y@~�@~�+@~@}p�@|��@|(�@z��@y�@y��@y�7@y�7@y�7@y�7@y�7@yhs@y7L@x�`@xbN@xb@w��@w\)@v�@vff@v@u��@u�@u`B@u/@t�@t�@t�@tj@s�
@q&�@j�!@d�D@_�@X  @P�9@HA�@?�;@;�
@7�@2�@.�R@(�@"�@��@  @�y@	��@z�@%?�p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBx�Bw�Bw�Bx�Bx�Bx�Bx�Bw�Bx�Bx�Bx�Bx�Bx�Bx�Bw�Bw�By�B}�Bw�B`BBXB]/B\)BJ�B+B�B��B�TB�B��B��B�qB�FB�dB�wB�!B��B�{B�7B� B|�Bw�Bq�BiyB[#BO�BF�B8RB�BuB%B��B�B�`B�/B�fBĜB�-B�B��B��B�uBq�B49B\B
�B
�B
��B
ƨB
ŢB
�?B
��B
�uB
~�B
iyB
VB
I�B
C�B
A�B
A�B
@�B
>wB
:^B
1'B
"�B
oB
B	��B	�BB	�
B	�B	��B	�wB	�3B	�'B	�B	�B	��B	��B	��B	�bB	�1B	�%B	�B	�B	�B	�B	�B	�B	u�B	jB	bNB	YB	T�B	O�B	M�B	M�B	G�B	B�B	>wB	<jB	;dB	9XB	7LB	1'B	-B	+B	)�B	)�B	(�B	'�B	&�B	#�B	�B	�B	�B	uB	\B	JB	
=B	+B	B	B��B��B�B�B�B�fB�TB�HB�BB�;B�5B�/B�/B�)B�B�B�
B��B��B��BŢB��B��B�}B�qB�jB�^B�RB�FB�3B�'B�B��B��B��B��B��B��B��B��B�{B�uB�oB�bB�VB�JB�DB�=B�7B�+B�%B�B�B~�B|�Bz�Bx�Bw�Bu�Bs�Br�Bm�BjBgmBdZBaHB]/BXBS�BP�BL�BJ�BH�BE�BD�BC�BB�BB�BA�B?}B9XB1'B,B(�B)�B(�B(�B(�B'�B&�B&�B%�B$�B$�B#�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B!�B"�B"�B"�B"�B#�B#�B"�B"�B&�B'�B(�B)�B,B-B.B/B.B0!B9XB;dB>wB>wB?}B?}BA�BC�BF�BH�BI�BJ�BJ�BI�BI�BJ�BR�BW
BW
BXBXB\)B]/B]/B]/B^5B_;Be`BhsBiyBiyBr�Bx�B�B�+B�1B�1B�1B�=B�DB�JB�PB�VB�bB�bB�hB�uB�uB��B��B��B��B��B��B�B�'B�3B�dBĜBŢBƨBȴBɺB��B��B��B�B�
B�B�)B�NB�fB�yB�B�B�B�B�B�B�B�B��B��B��B	  B	B	%B		7B	
=B	
=B	DB	PB	bB	hB	hB	hB	oB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	 �B	!�B	!�B	#�B	$�B	&�B	+B	1'B	33B	49B	6FB	<jB	?}B	@�B	A�B	D�B	G�B	H�B	I�B	K�B	L�B	L�B	N�B	N�B	O�B	P�B	R�B	T�B	VB	W
B	W
B	XB	XB	XB	ZB	[#B	\)B	_;B	aHB	aHB	bNB	bNB	bNB	dZB	ffB	iyB	l�B	p�B	x�B	}�B	� B	� B	� B	� B	� B	� B	�B	�B	�B	�+B	�1B	�=B	�JB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	ƨB	�B	�fB	��B
%B
�B
#�B
)�B
2-B
9XB
=qB
F�B
M�B
T�B
\)B
ffB
m�B
t�B
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bx�Bw�Bw�Bx�Bx�Bx�Bx�Bw�Bx�Bx�Bx�Bx�Bx�Bx�Bw�Bw�By�B}�Bw�B`#BW�B]B\BJ�B*�BgB��B�2B��B��BˣB�OB�#B�>B�QB��B��B�RB�B�B|�Bw�Bq�BiSB[ BO�BFB8*B�BNB�B��B��B�8B�	B�AB�wB�B��B��B��B�MBq�B4B8B
�sB
��B
ͯB
ƅB
�B
�B
��B
�RB
~�B
iXB
U�B
I�B
CuB
AiB
AhB
@^B
>UB
:>B
1B
"�B
MB
�B	��B	�%B	��B	��B	��B	�XB	�B	�B	��B	��B	��B	��B	��B	�BB	�B	�B	��B	��B	��B	��B	��B	��B	u�B	j`B	b1B	X�B	T�B	O�B	M�B	M�B	G�B	BqB	>ZB	<OB	;IB	9>B	7/B	1B	,�B	*�B	)�B	)�B	(�B	'�B	&�B	#�B	�B	�B	rB	XB	@B	0B	
!B	B	B	 �B��B��B�B�yB�dB�MB�:B�-B�)B�"B�B�B�B�B�B� B��B��B��B˭BŉB�pB�iB�dB�XB�QB�GB�:B�,B�B�B��B��B��B��B��B��B��B��B�uB�cB�\B�UB�HB�<B�3B�,B�'B�B�B�B�B��B~�B|�Bz�Bx�Bw�Bu�Bs�Br�BmzBjgBgXBdABa0B]BW�BS�BP�BL�BJ�BH�BE�BD�BCBByBBxBArB?fB9'B1B+�B(�B)�B(�B(�B(�B'�B&�B&�B%�B$�B$�B#�B"�B!�B�B�B�B�B�B�B�BkB�B�BxBvBwB{B|B|B|BvBVBxBuB|BiBiB�B�B�B�B�B�B�B�BhBhBfB�B�BvB�B�B�B�B�B�B �B �B �B!�B"�B"�B"�B"�B#�B#�B"�B"�B&�B'�B(�B)�B+�B,�B-�B/B-�B0B9=B;JB>YB>[B?`B?bBAmBCzBF�BH�BI�BJ�BJ�BI�BI�BJ�BR�BV�BV�BW�BW�B\B]B]B]B^B_BeBBhUBi]Bi^Br�Bx�B��B�B�B�B�B�B�%B�(B�1B�7B�CB�CB�KB�VB�TB�fB�nB��B��B��B��B��B�B�B�FB�zB�~BƄBȑBɘBθB��B��B��B��B��B�B�+B�DB�TB�\B�dB�sB�rB�xB��B�B�B��B��B��B��B	�B	�B		B	
B	
B	B	)B	=B	BB	@B	CB	IB	bB	sB	sB	yB	�B	�B	�B	�B	 �B	 �B	 �B	!�B	!�B	#�B	$�B	&�B	*�B	1 B	3B	4B	6B	<CB	?TB	@\B	AaB	DsB	G�B	H�B	I�B	K�B	L�B	L�B	N�B	N�B	O�B	P�B	R�B	T�B	U�B	V�B	V�B	W�B	W�B	W�B	Y�B	Z�B	\B	_B	a#B	aB	b$B	b$B	b$B	d0B	f<B	iPB	lcB	pzB	x�B	}�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�0B	�=B	�MB	�]B	�^B	�`B	�jB	�oB	�oB	�uB	�zB	��B	��B	�}B	��B	�:B	��B
�B
`B
#�B
)�B
2 B
9(B
=BB
FzB
M�B
T�B
[�B
f6B
mbB
t�B
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.39 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708242016053117082420160531170824  AO  ARCAADJP                                                                    20150417021553    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150417021553  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150417021553  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170824  IP                  G�O�G�O�G�O�                