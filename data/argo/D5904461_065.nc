CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-11T02:16:02Z AOML 3.0 creation; 2016-08-07T21:36:37Z UW 3.1 conversion     
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
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20150811021602  20160807143638  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               AA   AO  5286_8897_065                   2C  D   APEX                            6531                            072314                          846 @�f���
1   @�f��m��@3��\)�c7�;dZ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    AA   B   B   @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A���A�  B   B  B  B  B   B(  B0  B7��B@  BHffBO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy&fD�fD�9�D�y�D��3D�� D�\�D��fD�� D�fD�L�D�|�D���D�3D�0 D�p D�� D�fD�6fD�vfD�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@���@���A ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�33A�33A�ffB 33B33B33B33B 33B(33B033B7��B@33BH��BO��BX33B`33Bh33Bp33Bx33B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�L�B��B��B��B��B��C �C�C�C�C�C
�C�C�C&gC�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�3C�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�3C���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB|�DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3DtəDy)�D� D�;4D�{4D���D��D�^gD�� D�њD� D�NgD�~gD��gD��D�1�D�q�D�њD� D�8 D�x D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�G�A�I�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�I�A�I�A�E�A�E�A�E�A�E�A�E�A�?}A�;dA�;dA�7LA�33A��Aإ�A�ffAμjA�{A�;dA��`A˗�A��A�5?A��A���A���A�(�A�I�A�$�A���A���AƋDAŅA��TA�dZA���A�ƨA���A���A�l�A�bA���A�=qA�jA��jA��A�?}A��9A��A�t�A��A��A���A��9A��A�bNA�oA�A�A��A��+A�l�A�A���A��A���A�A��uA�n�A��+A��yA�A���A���A�C�A�`BA��uA���A�z�A���A�?}A���A�A�VA��A�ƨA���A��A�XA�33A�O�A��!A��jA���A�&�A�^5A���A�A�O�A���A�/A��A���A��wA���A��-A���A��
A�ĜA�PA}�Ax�Av�!Av�Au7LAr�HAn��Al�\Ai7LAdȴAaG�A]7LAZ1AW�AR�RAOXAM��AK�#AI&�AF�9AA�A?%A=%A;��A9C�A6^5A3G�A0��A.��A,ȴA*VA(M�A'?}A&�A&�RA&��A%��A"jA ȴAƨA�7A�HA�AI�A{A�A9XA&�A��A9XA��AA9XAz�AE�A�yAI�A�A
�\A�Az�AbAAbNA$�A�#A��A��A�hA�A�^A
��A�A
�jA
��A
=qA	oA�A�A=qA��A��A�yA��AV@��@���@���@���@���@�E�@��@�  @���@��-@��
@�{@�1'@��HA �@�@�|�@�t�@�=q@���@�r�@�{@�K�@��T@�E�@��@�1@�33@��
@��^@�?}@�F@���@���@��@�Ĝ@�9@�K�@���@�+@�1@�ȴ@�@���@�
=@��@��@�|�@�%@۶F@�v�@�^5@ج@�\)@�`B@�1@��@�1@·+@�n�@�-@ͩ�@���@�1'@ˮ@�|�@�S�@�o@��H@�ff@ɉ7@�/@ȼj@� �@�j@���@��@��@�  @�
=@�
=@��@Ƨ�@Ƈ+@ź^@�@�X@ļj@��m@�1'@���@���@�ƨ@Õ�@��@�@�n�@�@�%@�z�@�b@���@�dZ@�o@��@��H@�~�@�@���@��@�V@���@�Ĝ@���@��@�Z@�A�@��;@�+@�ȴ@���@��T@��@��@�ƨ@��P@�\)@��@��\@��@��#@���@�7L@��/@�A�@���@�"�@��y@��R@��+@�E�@�{@��^@��@�bN@���@���@�\)@���@��R@��+@�E�@�J@��#@�/@��/@���@��;@��m@��D@�x�@�?}@�O�@���@�Z@���@��
@��m@�1@���@��F@�|�@�E�@�`B@���@��^@��@���@���@��F@���@���@���@��w@�|�@�\)@�33@��@��!@��R@��H@�
=@���@�33@��@��\@�E�@���@�&�@��@��P@�+@�l�@��P@��@�ƨ@�ƨ@�ƨ@�bN@�I�@��@�;d@�33@�^5@�Z@�t�@�S�@��u@��@���@��m@���@�t�@��y@��@��R@��7@��D@��@�+@��@���@�  @��m@��m@��F@��
@�A�@�r�@�9X@���@��F@�S�@�@�"�@��@�~�@��@�v�@�n�@�-@��@��#@���@�7L@�V@��@���@�A�@��m@���@�@��!@�V@�J@��#@���@�hs@�X@��@��h@�p�@�&�@��@�Z@�9X@� �@��m@�ƨ@��F@���@���@�dZ@�@�ȴ@��!@�~�@��@��@���@���@��@w�w@pb@i��@_��@T�D@M��@E�h@=�T@9hs@4�@-�T@(�u@#�@E�@=q@?}@Ĝ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�G�A�I�A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�I�A�I�A�E�A�E�A�E�A�E�A�E�A�?}A�;dA�;dA�7LA�33A��Aإ�A�ffAμjA�{A�;dA��`A˗�A��A�5?A��A���A���A�(�A�I�A�$�A���A���AƋDAŅA��TA�dZA���A�ƨA���A���A�l�A�bA���A�=qA�jA��jA��A�?}A��9A��A�t�A��A��A���A��9A��A�bNA�oA�A�A��A��+A�l�A�A���A��A���A�A��uA�n�A��+A��yA�A���A���A�C�A�`BA��uA���A�z�A���A�?}A���A�A�VA��A�ƨA���A��A�XA�33A�O�A��!A��jA���A�&�A�^5A���A�A�O�A���A�/A��A���A��wA���A��-A���A��
A�ĜA�PA}�Ax�Av�!Av�Au7LAr�HAn��Al�\Ai7LAdȴAaG�A]7LAZ1AW�AR�RAOXAM��AK�#AI&�AF�9AA�A?%A=%A;��A9C�A6^5A3G�A0��A.��A,ȴA*VA(M�A'?}A&�A&�RA&��A%��A"jA ȴAƨA�7A�HA�AI�A{A�A9XA&�A��A9XA��AA9XAz�AE�A�yAI�A�A
�\A�Az�AbAAbNA$�A�#A��A��A�hA�A�^A
��A�A
�jA
��A
=qA	oA�A�A=qA��A��A�yA��AV@��@���@���@���@���@�E�@��@�  @���@��-@��
@�{@�1'@��HA �@�@�|�@�t�@�=q@���@�r�@�{@�K�@��T@�E�@��@�1@�33@��
@��^@�?}@�F@���@���@��@�Ĝ@�9@�K�@���@�+@�1@�ȴ@�@���@�
=@��@��@�|�@�%@۶F@�v�@�^5@ج@�\)@�`B@�1@��@�1@·+@�n�@�-@ͩ�@���@�1'@ˮ@�|�@�S�@�o@��H@�ff@ɉ7@�/@ȼj@� �@�j@���@��@��@�  @�
=@�
=@��@Ƨ�@Ƈ+@ź^@�@�X@ļj@��m@�1'@���@���@�ƨ@Õ�@��@�@�n�@�@�%@�z�@�b@���@�dZ@�o@��@��H@�~�@�@���@��@�V@���@�Ĝ@���@��@�Z@�A�@��;@�+@�ȴ@���@��T@��@��@�ƨ@��P@�\)@��@��\@��@��#@���@�7L@��/@�A�@���@�"�@��y@��R@��+@�E�@�{@��^@��@�bN@���@���@�\)@���@��R@��+@�E�@�J@��#@�/@��/@���@��;@��m@��D@�x�@�?}@�O�@���@�Z@���@��
@��m@�1@���@��F@�|�@�E�@�`B@���@��^@��@���@���@��F@���@���@���@��w@�|�@�\)@�33@��@��!@��R@��H@�
=@���@�33@��@��\@�E�@���@�&�@��@��P@�+@�l�@��P@��@�ƨ@�ƨ@�ƨ@�bN@�I�@��@�;d@�33@�^5@�Z@�t�@�S�@��u@��@���@��m@���@�t�@��y@��@��R@��7@��D@��@�+@��@���@�  @��m@��m@��F@��
@�A�@�r�@�9X@���@��F@�S�@�@�"�@��@�~�@��@�v�@�n�@�-@��@��#@���@�7L@�V@��@���@�A�@��m@���@�@��!@�V@�J@��#@���@�hs@�X@��@��h@�p�@�&�@��@�Z@�9X@� �@��m@�ƨ@��F@���@���@�dZ@�@�ȴ@��!@�~�G�O�@��@���@���@��@w�w@pb@i��@_��@T�D@M��@E�h@=�T@9hs@4�@-�T@(�u@#�@E�@=q@?}@Ĝ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
�'B
�7B
�{B
��B
�}B
��B
�HB	7B2-BK�Bz�B��BÖB�ZB��B=qBZBk�Bu�B�1B��B��B�XBBǮBȴB��B�)B�/B�BB��B  B	7B\B�B�B�B$�B,B&�B$�B�B�B�B�B��B��B�B�B�B�B�B��B�}BÖB��B��Bv�Bo�BcTBL�B9XB(�BuBB��B�B�`B�#B��B��B��BȴBÖB��B�VB�Bo�B[#BC�B5?B'�B�B
��B
�B
��B
�B
��B
�JB
y�B
k�B
ZB
K�B
<jB
,B
�B	��B	�ZB	�/B	��B	��B	��B	�VB	p�B	P�B	6FB	�B	JB��B�`B�B��B��BĜB�jB�RB�FB�3B�?B�3B��B��B�\B�=B� By�Bt�Bt�Bt�Bs�Br�Bp�Br�Bn�BjBhsBcTB]/BVBN�BK�BJ�BK�BL�BQ�BQ�BP�BZBXBP�BM�BJ�BF�BA�B?}B>wB=qB<jB<jBN�Bs�Bv�B}�B�wBɺB��B��B�#B�;B�ZB�BB�B��B��B��B��B�B��B��BȴB�B��B�VB�{B��B��B��B��B��B�!B��B��B�NB�B	+B��B��B��B��B�B�fB�BB�B�B�5B�/B�#B�)B�B��B��B��B	�B	{B	{B	�B	�B	�B	{B	{B	�B	�B	�B	�B	�B	oB	�B	�B	�B	�B	�B	�B	�B	uB	\B	DB	%B	B	B	B	B	B	B	B	+B		7B	PB	bB	uB	�B	�B	�B	�B	�B	+B	/B	0!B	2-B	1'B	/B	0!B	1'B	1'B	6FB	<jB	?}B	@�B	A�B	B�B	F�B	G�B	H�B	K�B	O�B	N�B	L�B	K�B	J�B	G�B	F�B	F�B	F�B	I�B	I�B	I�B	I�B	I�B	J�B	L�B	L�B	M�B	O�B	Q�B	S�B	T�B	VB	VB	XB	\)B	[#B	[#B	]/B	`BB	bNB	cTB	dZB	e`B	e`B	ffB	gmB	gmB	gmB	hsB	hsB	iyB	l�B	n�B	o�B	o�B	o�B	r�B	r�B	t�B	v�B	y�B	z�B	z�B	{�B	{�B	|�B	}�B	}�B	~�B	�B	�B	�B	�B	�B	�%B	�DB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�XB	�dB	�jB	�qB	B	ĜB	ŢB	ŢB	ƨB	ǮB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�)B	�5B	�;B	�`B	�ZB	�NB	�HB	�HB	�5B	�
B	��B	�B	�/B	�HB	�NB	�NB	�HB	�BB	�5B	�B	��B	ǮB	��B	�}B	B	ƨB	ȴB	��B	��B	��B	��B	��B	�B	�#B	�#B	�#B	�#B	�#B	�)B	�5B	�5B	�BB	�NB	�fB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
JB
B
DB
uB
{B
{B
�B
%�B
.B
33B
8RB
:^B
A�B
I�B
R�B
XB
]/B
_;B
bNB
hsB
n�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B
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
�&B
�6B
�~B
��B
�~B
��B
�JB	6B2+BK�Bz�B��BÏB�WB��B=lBZBk�Bu�B�.B��B��B�WBBǪBȵB��B�%B�/B�BB��B��B	4B\B�B�B�B$�B,	B&�B$�B�B�B�B�B��B��B�B�B�B�B�B��B�xBÐB��B��Bv�Bo�BcNBL�B9TB(�BoBB��B�B�ZB�B��B��B��BȰBÒB��B�RB�Bo�B[ BC�B5;B'�B�B
��B
�B
��B
�B
��B
�IB
y�B
k�B
ZB
K�B
<jB
,B
�B	��B	�^B	�4B	��B	��B	��B	�\B	p�B	P�B	6NB	�B	UB��B�kB�*B�B��BīB�yB�]B�TB�AB�OB�BB�B��B�mB�NB�By�Bt�Bt�Bt�Bs�Br�Bp�Br�Bn�Bj�Bh�BceB]>BVBN�BK�BJ�BK�BL�BQ�BQ�BP�BZ0BX BP�BM�BJ�BF�BA�B?�B>�B=�B<}B<|BN�Bs�Bv�B~B��B��B��B��B�.B�DB�eB�MB�!B�B�	B�	B�B�B�B��B��B�(B��B�fB��B��B��B��B��B��B�-B��B��B�ZB��B	3B��B��B��B��B�B�pB�MB�#B�'B�@B�9B�-B�2B�B��B��B��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	yB	�B	�B	�B	�B	�B	�B	�B	}B	cB	NB	,B	$B	B	B	B	B	B	B	5B		@B	XB	kB	|B	�B	�B	�B	�B	�B	+	B	/#B	0%B	24B	1-B	/#B	0(B	1,B	1-B	6JB	<qB	?�B	@�B	A�B	B�B	F�B	G�B	H�B	K�B	O�B	N�B	L�B	K�B	J�B	G�B	F�B	F�B	F�B	I�B	I�B	I�B	I�B	I�B	J�B	L�B	L�B	M�B	O�B	Q�B	S�B	UB	V	B	V
B	XB	\0B	[)B	[(B	]4B	`DB	bSB	cYB	d`B	efB	edB	fiB	gtB	gtB	gpB	hxB	hyB	i}B	l�B	n�B	o�B	o�B	o�B	r�B	r�B	t�B	v�B	y�B	z�B	z�B	{�B	{�B	|�B	}�B	}�B	~�B	�
B	�B	�B	�B	�B	�*B	�GB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�%B	�YB	�gB	�lB	�rB	B	ĞB	šB	ţB	ƫB	ǱB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�(B	�9B	�:B	�_B	�XB	�KB	�JB	�JB	�5B	�B	��B	�B	�1B	�JB	�NB	�MB	�IB	�BB	�9B	�B	��B	ǱB	��B	�B	B	ƪB	ȵB	��B	��B	��B	��B	��B	�B	�$B	�%B	�#B	�"B	�#B	�)B	�8B	�7B	�EB	�NB	�hB	�uB	�{B	�zB	�wB	�wB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
G�O�B
B
DB
tB
xB
{B
�B
%�B
.B
33B
8SB
:\B
A�B
I�B
R�B
XB
]-B
_:B
bMB
hqB
n�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.05 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436382016080714363820160807143638  AO  ARCAADJP                                                                    20150811021602    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150811021602  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150811021602  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143638  IP                  G�O�G�O�G�O�                