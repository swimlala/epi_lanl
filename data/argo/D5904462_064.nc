CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-07-17T02:15:46Z AOML 3.0 creation; 2016-08-07T21:51:19Z UW 3.1 conversion     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20150717021546  20160807145119  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               @A   AO  5287_9017_064                   2C  D   APEX                            6529                            072314                          846 @�`{��?�1   @�`|$�	@0���E��d���S��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    @A   B   B   @9��@�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D��D�P D���D�ٚD�	�D�9�D�s3D��fD�3D�I�D�� D��3D��D�I�DچfD��3D��D�I�D�vfD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @R�]@�z�@�z�A��A&=qAF=qAf=qA��A��A��A��A��A��A��A��B�\B	�\B�\B�\B!�\B)�\B1�\B9�\BA�\BI�\BQ�\BY�\Ba�\Bi�\Bq�\By�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC c�Cc�Cc�Cc�Cc�C
c�Cc�Cc�Cc�C}qCJ=Cc�Cc�Cc�Cc�Cc�C c�C"c�C$c�C&c�C(c�C*c�C,c�C.c�C0c�C2c�C4c�C6c�C8c�C:c�C<c�C>c�C@c�CBc�CDc�CFc�CHc�CJc�CLc�CNc�CPc�CRc�CTc�CVc�CXc�CZc�C\c�C^c�C`c�Cbc�Cdc�Cfc�Chc�Cjc�Clc�Cnc�Cpc�Crc�Ctc�Cvc�Cxc�Czc�C|c�C~c�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�%C�%C�1�C�1�C�1�C�1�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�DyҐD�&D�\{D��HD��D�D�FD��D���D��D�VD��{D�߮D�HD�VDڒ�D�߮D�&D�VD��D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�oA�^A�ffA�^5A�M�A�G�A�E�A�?}A�;dA�5?A�/A�-A�(�A�(�A�$�A�$�A�$�A�"�A��A���AߋDA�;dA���A�
=A���A���AݮA�jA�33A�A��mA��#AܼjAܣ�Aܟ�A�bNA���A�t�A�5?A��/A�E�A�S�A֓uAԟ�A���AҴ9A�C�A� �A�K�AѶFA�
=A�ZA�ffA˲-AʸRA�C�A���Aɩ�A���A�bNA��AǸRA�x�AƧ�A��yA�$�A�ĜA��A�ĜA���A�I�A��#A��A�A���A�A���A�A�ZA�/A�Q�A���A��7A��A�O�A��HA�C�A�ȴA�`BA�A���A���A��A�n�A��A���A��A�ƨA���A�|�A�ƨA���A�VA�bA���A��jA���A�A���A�hsA��A�v�A~1Az�RAxbAvVAu33ArffAn��Al�DAj1Af�+Ab��A_��A]�PA[
=AYC�AVjATbAR��AQ7LAOt�AM��AL�AK
=AH1'AG/AG%AF�AF��AE�wAD-AC&�ABbNA@��A>��A<�A:^5A7t�A6=qA5��A4�uA1�wA/&�A.v�A-\)A,n�A,1A+��A*^5A(ffA'�A&I�A%VA${A#`BA"��A"bA!�mA!+A �A��AbAt�Az�AK�A�mA�AjAƨA��Av�AJA��A1'A�\A��A�A
bNA	�hA	��A	|�A	C�A	VA�jA�A��A�mA�TA�;A�^A�A��A��@���@�C�@�|�@�"�@�x�@��H@�dZ@�|�@��+A =q@���@��mA @�O�@�hs@��#@��@�;d@��9@��j@��;@���@��/@���@�1'@�ƨ@��@��@�5?@�Ĝ@��@�
=@��@�$�@��H@�dZ@�t�@�C�@�n�@��@�hs@��@���@�r�@�  @���@�@�C�@�
=@�R@���@�hs@�@�Ĝ@� �@㕁@�K�@���@�n�@�J@�J@�7L@��@�l�@�5?@�7L@�%@�%@�(�@�|�@�+@���@��@��@ڰ!@��@�?}@�9X@�K�@�ff@թ�@�7L@��@���@� �@��T@ѡ�@��@Гu@�9X@ϥ�@�C�@�M�@Ͳ-@�1@�+@�ȴ@�M�@�J@�E�@���@ʇ+@Ɂ@�X@��@ȋD@���@���@��@���@�{@�@��#@Ų-@�`B@�&�@Ĵ9@�1'@�1@��
@��
@î@�;d@��H@�n�@���@��^@���@�\)@���@���@���@���@�(�@��;@���@�;d@�33@�"�@�o@��@���@�
=@�K�@���@�I�@�z�@�z�@�Z@��@���@��@�|�@�l�@�l�@�;d@�@���@��@���@�?}@��`@�r�@��;@�dZ@���@��@��-@���@�hs@�/@��j@�bN@�1@��F@�dZ@��@��+@��@���@��-@���@��7@�O�@�V@��@���@�Ĝ@���@� �@��@�S�@��@���@���@�~�@�v�@�$�@��@���@��h@�O�@�%@��@�z�@�9X@��
@��P@�\)@�
=@���@��!@���@�n�@�@��@�&�@��u@��m@���@�K�@�
=@���@��@���@���@�~�@�V@��@�@�`B@��@�bN@�1@�  @���@��w@��y@�v�@�ff@���@�&�@�z�@�Q�@�1'@��@�|�@�ȴ@���@��\@�v�@�V@�=q@��@��@���@�1'@���@���@�
=@�~�@�ff@�V@�E�@�-@�@�O�@��@�Ĝ@��@�b@��w@�S�@�o@��@��!@���@�E�@�$�@���@��@�&�@���@���@�9X@� �@���@���@��@���@}O�@s��@i��@^��@T�j@K��@C��@;��@4z�@.��@'�@"�@ȴ@��@ff@�@�j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�oA�^A�ffA�^5A�M�A�G�A�E�A�?}A�;dA�5?A�/A�-A�(�A�(�A�$�A�$�A�$�A�"�A��A���AߋDA�;dA���A�
=A���A���AݮA�jA�33A�A��mA��#AܼjAܣ�Aܟ�A�bNA���A�t�A�5?A��/A�E�A�S�A֓uAԟ�A���AҴ9A�C�A� �A�K�AѶFA�
=A�ZA�ffA˲-AʸRA�C�A���Aɩ�A���A�bNA��AǸRA�x�AƧ�A��yA�$�A�ĜA��A�ĜA���A�I�A��#A��A�A���A�A���A�A�ZA�/A�Q�A���A��7A��A�O�A��HA�C�A�ȴA�`BA�A���A���A��A�n�A��A���A��A�ƨA���A�|�A�ƨA���A�VA�bA���A��jA���A�A���A�hsA��A�v�A~1Az�RAxbAvVAu33ArffAn��Al�DAj1Af�+Ab��A_��A]�PA[
=AYC�AVjATbAR��AQ7LAOt�AM��AL�AK
=AH1'AG/AG%AF�AF��AE�wAD-AC&�ABbNA@��A>��A<�A:^5A7t�A6=qA5��A4�uA1�wA/&�A.v�A-\)A,n�A,1A+��A*^5A(ffA'�A&I�A%VA${A#`BA"��A"bA!�mA!+A �A��AbAt�Az�AK�A�mA�AjAƨA��Av�AJA��A1'A�\A��A�A
bNA	�hA	��A	|�A	C�A	VA�jA�A��A�mA�TA�;A�^A�A��A��@���@�C�@�|�@�"�@�x�@��H@�dZ@�|�@��+A =q@���@��mA @�O�@�hs@��#@��@�;d@��9@��j@��;@���@��/@���@�1'@�ƨ@��@��@�5?@�Ĝ@��@�
=@��@�$�@��H@�dZ@�t�@�C�@�n�@��@�hs@��@���@�r�@�  @���@�@�C�@�
=@�R@���@�hs@�@�Ĝ@� �@㕁@�K�@���@�n�@�J@�J@�7L@��@�l�@�5?@�7L@�%@�%@�(�@�|�@�+@���@��@��@ڰ!@��@�?}@�9X@�K�@�ff@թ�@�7L@��@���@� �@��T@ѡ�@��@Гu@�9X@ϥ�@�C�@�M�@Ͳ-@�1@�+@�ȴ@�M�@�J@�E�@���@ʇ+@Ɂ@�X@��@ȋD@���@���@��@���@�{@�@��#@Ų-@�`B@�&�@Ĵ9@�1'@�1@��
@��
@î@�;d@��H@�n�@���@��^@���@�\)@���@���@���@���@�(�@��;@���@�;d@�33@�"�@�o@��@���@�
=@�K�@���@�I�@�z�@�z�@�Z@��@���@��@�|�@�l�@�l�@�;d@�@���@��@���@�?}@��`@�r�@��;@�dZ@���@��@��-@���@�hs@�/@��j@�bN@�1@��F@�dZ@��@��+@��@���@��-@���@��7@�O�@�V@��@���@�Ĝ@���@� �@��@�S�@��@���@���@�~�@�v�@�$�@��@���@��h@�O�@�%@��@�z�@�9X@��
@��P@�\)@�
=@���@��!@���@�n�@�@��@�&�@��u@��m@���@�K�@�
=@���@��@���@���@�~�@�V@��@�@�`B@��@�bN@�1@�  @���@��w@��y@�v�@�ff@���@�&�@�z�@�Q�@�1'@��@�|�@�ȴ@���@��\@�v�@�V@�=q@��@��@���@�1'@���@���@�
=@�~�@�ff@�V@�E�@�-@�@�O�@��@�Ĝ@��@�b@��w@�S�@�o@��@��!@���@�E�@�$�@���@��@�&�@���@���@�9XG�O�@���@���@��@���@}O�@s��@i��@^��@T�j@K��@C��@;��@4z�@.��@'�@"�@ȴ@��@ff@�@�j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�DB	�DB	�DB	�JB	�PB	�PB	�PB	�VB	�VB	�VB	�VB	�VB	�VB	�VB	�\B	�\B	�\B	�bB	�oB	��B	��B
$�B
^5B
dZB
]/B
^5B
`BB
aHB
bNB
`BB
`BB
`BB
`BB
_;B
_;B
cTB
hsB
aHB
_;B
]/B
R�B
2-B
�B
�B
I�B
dZB
}�B
�BB-Bm�B��B�B�LB�?B�^BɺB�B�#B��B	7B�BP�BZBl�Bn�Bw�B�B�\B�bB��B�BÖB�ZBȴB� BcTBgmBQ�BA�BA�BE�BB�BD�BA�B;dB1'B1'B5?B,B"�B
=B�B�`B��B��B�LB��B��B��B� BjB[#BJ�B1'B!�B�B
��B
�B
ȴB
�B
z�B
H�B
�B
%B
PB
B	��B	�B	�NB	�B	ÖB	�B	��B	�B	w�B	jB	`BB	S�B	H�B	A�B	6FB	-B	$�B	�B	�B	PB	
=B		7B	1B	+B	B��B��B��B��B�B�B�ZB�5B�)B�B�B�
B�B�B�#B�)B�)B�B�B�)B�HB�fB�B�B�B��B��B	B	PB	VB	hB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	bB	  B�B�B��B�B�B�B�B�
B��BÖBŢBȴB��B��B�
B�B�B�BB�B�B�B�B�fB	
=B	uB	 �B	8RB	7LB	>wB	?}B	7LB	9XB	=qB	6FB	5?B	D�B	O�B	A�B	8RB	>wB	@�B	?}B	>wB	:^B	6FB	8RB	7LB	5?B	7LB	9XB	<jB	C�B	I�B	K�B	O�B	ZB	]/B	^5B	aHB	cTB	dZB	e`B	e`B	e`B	e`B	ffB	ffB	gmB	iyB	gmB	l�B	q�B	q�B	p�B	o�B	o�B	o�B	q�B	r�B	r�B	p�B	m�B	k�B	m�B	s�B	v�B	u�B	v�B	x�B	z�B	{�B	}�B	z�B	}�B	� B	}�B	|�B	z�B	{�B	|�B	�B	�B	~�B	}�B	|�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�PB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�RB	�}B	ŢB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�#B	�)B	�/B	�5B	�;B	�BB	�BB	�BB	�BB	�BB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
1B
oB
�B
#�B
)�B
0!B
5?B
<jB
C�B
I�B
O�B
VB
\)B
`BB
bNB
ffB
k�B
o�B
s�B
w�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B	�7B	�6B	�6B	�:B	�@B	�@B	�@B	�GB	�CB	�EB	�FB	�FB	�DB	�DB	�KB	�MB	�KB	�PB	�_B	��B	��B
$�B
^B
dBB
]B
^B
`+B
a/B
b6B
`*B
`*B
`+B
`*B
_"B
_#B
c;B
hZB
a.B
_"B
]B
R�B
2B
�B
�B
I�B
dBB
}�B
�&B,�BmsB��B��B�+B� B�ABɚB��B�B��B	B{BP�BY�BllBnxBw�B��B�:B�EB��B��B�tB�;BȓB�Bc3BgIBQ�BAjBAgBE~BBpBDwBAeB;AB1B1B5B+�B"�B
B�~B�=BδB�aB�(B��B��B�wB�BjZBZ�BJ�B1B!�BnB
��B
��B
ȐB
��B
z�B
H�B
_B
B
.B
 �B	��B	�_B	�-B	��B	�wB	��B	�uB	�B	w�B	jbB	`$B	S�B	H�B	AnB	6)B	,�B	$�B	�B	kB	6B	
"B		B	B	B	 B��B��B��B��B�B�qB�@B�B�B�B��B��B��B��B�B�B�B�B��B�B�-B�KB�}B�B�B��B��B	�B	4B	9B	JB	RB	VB	`B	hB	|B	�B	�B	}B	sB	tB	sB	sB	bB	DB��B�tB��B��B��B��B��B��B��B͵B�{BŃBȖB͵B��B��B�B�sB�"B�tB�B�B�sB�EB	
B	SB	 �B	80B	7)B	>UB	?\B	7*B	92B	=NB	6$B	5B	D{B	O�B	AeB	8/B	>TB	@^B	?ZB	>UB	:<B	6$B	80B	7+B	5B	7'B	96B	<IB	CpB	I�B	K�B	O�B	Y�B	]B	^B	a$B	c.B	d5B	e>B	e<B	e:B	e:B	f@B	f?B	gHB	iUB	gIB	leB	q�B	q�B	pB	owB	oxB	ozB	q�B	r�B	r�B	p~B	mlB	k`B	mkB	s�B	v�B	u�B	v�B	x�B	z�B	{�B	}�B	z�B	}�B	�B	}�B	|�B	z�B	{�B	|�B	��B	��B	~�B	}�B	|�B	~�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�(B	�GB	�OB	�MB	�dB	�}B	�yB	�sB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�(B	�TB	�xB	�vB	ǆB	ȉB	ʙB	˛B	̤B	ͪB	αB	ϵB	ϴB	��B	��B	��B	��B	��B	��B	лB	лB	лB	мB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�*B	�)B	�0B	�7B	�:B	�EB	�CB	�IB	�PB	�OB	�SB	�ZB	�aB	�]B	�^B	�aB	�eB	�fB	�sB	�|B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
 B
B
B
 B
G�O�B
B
@B
gB
#�B
)�B
/�B
5B
<<B
ChB
I�B
O�B
U�B
[�B
`B
bB
f8B
kVB
onB
s�B
w�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.39 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451192016080714511920160807145119  AO  ARCAADJP                                                                    20150717021546    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150717021546  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150717021546  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145119  IP                  G�O�G�O�G�O�                