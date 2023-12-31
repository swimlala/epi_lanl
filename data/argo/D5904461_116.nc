CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-05-06T02:20:01Z AOML 3.0 creation; 2016-08-07T21:36:46Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160506022001  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               tA   AO  5286_8897_116                   2C  D   APEX                            6531                            072314                          846 @ש����1   @ש����@3���R�cQ�E��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    tA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dyy�D�fD�I�D�|�D��fD���D�@ D�vfD���D��D�)�D��fD�� D�3D�I�Dڌ�D��D�fD�C3D�s3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���A ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B��B ��B(33B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B�L�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��gB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C�C�C�C�C&gC�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT&gCV�CX�CZ�C\�C^�C`�Cb�Cd�Cf&gCh�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D|�D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,��D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dtp Dy|�D� D�K4D�~gD�� D��4D�A�D�x D��4D�4D�+4D�� D�њD��D�K4DڎgD�gD� D�D�D�t�D��g11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��Aʴ9A�jA�VA�K�A�A�A�;dA�1'A�-A�&�A�$�A�"�A��A�oA�%A��TA�t�A�$�A��/Aȝ�AȃA�VA���AǓuA�I�A��AƧ�A�Aç�A��;A�z�A�?}A��`A�jA��
A�l�A�A��A�A���A�;dA���A�v�A���A��A�E�A��A��A�JA�(�A�ffA���A�bA�dZA�-A�"�A��A�%A���A��TA�bNA�7LA�1A���A�Q�A��A���A�t�A�5?A��FA��`A���A��A�VA�S�A���A�|�A��yA�C�A��A�oA�O�A�K�A���A�S�A���A�Q�A��#A�A���A�bA�ZA��HA�JA��A�hsA��A�jA���A��+A���A�9XA���A�XA~�uA|��Az�9Aw��Aq\)Am"�AkAg��AeG�Ab�/A_��A\��A[��AY+AUoAQ��AP�uAO�7ANv�AL=qAI�AFn�ADZAAx�A@�A?hsA>��A=�#A<r�A;�PA;"�A;�A:�uA9A7��A5&�A4r�A4(�A3��A2{A/��A/x�A/VA,�/A+33A);dA&n�A#�hA"�9A!�-A �/A7LA��A�uA��AC�A�uA�TAbNA�AȴA1'AhsAG�A��AA�Ap�A��AffAZAbA  A+A�+A�
A\)A��A�wA�^A��A�A
�DA	"�A=qAƨA�-A�7AC�A��A�A��A\)A+A�AoA
=A�9Az�AA�A1'A1'A5?A1'A�hAO�A��A��A1A ȴA {@�l�@�n�@��T@�&�@� �@��\@���@���@��P@�+@�
=@�M�@��@��@�M�@�G�@�j@웦@���@�\)@�"�@���@��/@�P@���@���@�v�@�7L@���@���@���@�l�@އ+@ݙ�@۝�@ڗ�@�X@��@�\)@�;d@�@֏\@���@��@� �@�ƨ@�\)@�5?@�-@Ѻ^@�r�@�l�@�^5@���@ʧ�@�&�@�1'@�1@�@�E�@�@��@�  @Ý�@Õ�@Ý�@�\)@��@�@���@�%@�Q�@�Q�@���@�v�@���@��@��j@�1@��@�5?@��^@�O�@�X@�`B@�hs@�`B@�/@��7@��`@���@�I�@�b@��F@�|�@�@���@�ff@�^5@��@���@�1'@��@���@�5?@�M�@�5?@�V@�ff@�M�@�M�@��R@��R@�E�@��7@�7L@��D@��R@��@���@��@�ȴ@���@��@��@��@�33@�E�@�@�@�V@�^5@�E�@�`B@��@�/@��9@�1@�C�@�M�@���@� �@��w@�t�@��@�dZ@�C�@�o@�
=@��@�ff@�E�@��T@�x�@�7L@��j@�z�@�b@�l�@��@���@�ff@�E�@�^5@�n�@�n�@��@��@��^@�p�@�G�@��`@��@�bN@��@��F@��
@�S�@��P@�r�@�hs@�?}@�%@�bN@�"�@�v�@�x�@���@���@�Ĝ@���@� �@�1'@�(�@�ƨ@�dZ@��@�@��#@��#@��#@�@���@�O�@���@��j@�I�@�1@���@���@�M�@�{@���@��^@��h@�p�@�`B@�G�@�/@��j@�9X@���@�;d@�ȴ@�E�@���@��#@��^@��@�G�@�7L@��@���@��/@��@�Q�@��;@���@��P@�dZ@��@���@���@���@��!@���@�n�@�=q@�-@�^5@�-@�$�@�{@�J@��@�@��7@���@��@��F@�33@��H@�ȴ@�v�@�M�@�-@���@���@�O�@�%@���@��@�j@�(�@��@~ff@st�@m��@d��@Z~�@R~�@L��@Ax�@<Z@4�/@1��@.5?@*�H@'\)@!%@�@ȴ@�`@��@Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��Aʴ9A�jA�VA�K�A�A�A�;dA�1'A�-A�&�A�$�A�"�A��A�oA�%A��TA�t�A�$�A��/Aȝ�AȃA�VA���AǓuA�I�A��AƧ�A�Aç�A��;A�z�A�?}A��`A�jA��
A�l�A�A��A�A���A�;dA���A�v�A���A��A�E�A��A��A�JA�(�A�ffA���A�bA�dZA�-A�"�A��A�%A���A��TA�bNA�7LA�1A���A�Q�A��A���A�t�A�5?A��FA��`A���A��A�VA�S�A���A�|�A��yA�C�A��A�oA�O�A�K�A���A�S�A���A�Q�A��#A�A���A�bA�ZA��HA�JA��A�hsA��A�jA���A��+A���A�9XA���A�XA~�uA|��Az�9Aw��Aq\)Am"�AkAg��AeG�Ab�/A_��A\��A[��AY+AUoAQ��AP�uAO�7ANv�AL=qAI�AFn�ADZAAx�A@�A?hsA>��A=�#A<r�A;�PA;"�A;�A:�uA9A7��A5&�A4r�A4(�A3��A2{A/��A/x�A/VA,�/A+33A);dA&n�A#�hA"�9A!�-A �/A7LA��A�uA��AC�A�uA�TAbNA�AȴA1'AhsAG�A��AA�Ap�A��AffAZAbA  A+A�+A�
A\)A��A�wA�^A��A�A
�DA	"�A=qAƨA�-A�7AC�A��A�A��A\)A+A�AoA
=A�9Az�AA�A1'A1'A5?A1'A�hAO�A��A��A1A ȴA {@�l�@�n�@��T@�&�@� �@��\@���@���@��P@�+@�
=@�M�@��@��@�M�@�G�@�j@웦@���@�\)@�"�@���@��/@�P@���@���@�v�@�7L@���@���@���@�l�@އ+@ݙ�@۝�@ڗ�@�X@��@�\)@�;d@�@֏\@���@��@� �@�ƨ@�\)@�5?@�-@Ѻ^@�r�@�l�@�^5@���@ʧ�@�&�@�1'@�1@�@�E�@�@��@�  @Ý�@Õ�@Ý�@�\)@��@�@���@�%@�Q�@�Q�@���@�v�@���@��@��j@�1@��@�5?@��^@�O�@�X@�`B@�hs@�`B@�/@��7@��`@���@�I�@�b@��F@�|�@�@���@�ff@�^5@��@���@�1'@��@���@�5?@�M�@�5?@�V@�ff@�M�@�M�@��R@��R@�E�@��7@�7L@��D@��R@��@���@��@�ȴ@���@��@��@��@�33@�E�@�@�@�V@�^5@�E�@�`B@��@�/@��9@�1@�C�@�M�@���@� �@��w@�t�@��@�dZ@�C�@�o@�
=@��@�ff@�E�@��T@�x�@�7L@��j@�z�@�b@�l�@��@���@�ff@�E�@�^5@�n�@�n�@��@��@��^@�p�@�G�@��`@��@�bN@��@��F@��
@�S�@��P@�r�@�hs@�?}@�%@�bN@�"�@�v�@�x�@���@���@�Ĝ@���@� �@�1'@�(�@�ƨ@�dZ@��@�@��#@��#@��#@�@���@�O�@���@��j@�I�@�1@���@���@�M�@�{@���@��^@��h@�p�@�`B@�G�@�/@��j@�9X@���@�;d@�ȴ@�E�@���@��#@��^@��@�G�@�7L@��@���@��/@��@�Q�@��;@���@��P@�dZ@��@���@���@���@��!@���@�n�@�=q@�-@�^5@�-@�$�@�{@�J@��@�@��7@���@��@��F@�33@��H@�ȴ@�v�@�M�@�-@���@���@�O�@�%@���@��@�jG�O�@��@~ff@st�@m��@d��@Z~�@R~�@L��@Ax�@<Z@4�/@1��@.5?@*�H@'\)@!%@�@ȴ@�`@��@Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�B
�B
�B
�B
�B
�B
�yB
�B
��B
��B
��BB	7B �B,B6FB:^BB�Br�B��BŢB�)B�B��B1B�B"�B$�B2-B+B)�BC�BXB`BBv�Bz�Bw�B�DB�=B��B��B��B��B��B�hB�bB�hB�hB�hB�hB�oB�oB�hB�oB�{B��B��B��B�B�!B��B��B�=B}�Bx�Bm�BQ�B>wB(�B�BuBB�B�;B��B�}B��B�uB�JB�7B�%B{�Bn�BJ�B,B$�B�BJB
�yB
�qB
��B
e`B
YB
N�B
5?B
hB
B	�B	�#B	�B	�7B	u�B	cTB	O�B	8RB	�B	DB	B�B�/B��BȴBÖB�qB�3B��B��B�oB�JB�7B�7B�1B�+B�=B�DB�VB�\B�\B�PB�=B�7B�7B�1B�%B�B�7B�1B�%B�%B�B�B�B�B�B�%B�%B�=B�=B�=B�=B�=B�JB�PB�\B�\B�hB�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�9B�RB�XB�^B�qB�^B�wB�}B��B��B��BƨBǮBƨBƨBƨBƨB��B�jB�B��B��B��B��B��B��B��B��B�-B�9B�FB�3B�'B�'B�9B�jB��B�B��B��B��B��B��B��B��B��B�
B�B�B�B�#B�#B�)B�/B�/B�/B�NB�`B�sB�sB�B�B�B�B�B�B�B�B��B��B��B��B��B��B	B	B	%B	1B		7B	
=B	+B	%B	B	+B	1B		7B	DB	DB	JB	JB	JB	JB	PB	VB	uB	�B	�B	�B	�B	�B	�B	 �B	"�B	"�B	#�B	%�B	)�B	/B	49B	8RB	;dB	?}B	B�B	D�B	E�B	F�B	H�B	P�B	T�B	YB	XB	\)B	]/B	[#B	[#B	bNB	iyB	m�B	q�B	w�B	{�B	z�B	x�B	v�B	v�B	x�B	z�B	{�B	z�B	w�B	u�B	y�B	z�B	x�B	t�B	r�B	o�B	p�B	s�B	u�B	v�B	x�B	|�B	}�B	}�B	� B	�B	�B	�%B	�7B	�=B	�DB	�JB	�PB	�\B	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�^B	B	ŢB	ŢB	ŢB	ƨB	ǮB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	ɺB	ɺB	ȴB	ǮB	ŢB	ŢB	ƨB	ƨB	ƨB	ƨB	ƨB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�5B	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B

=B
�B
�B
'�B
,B
5?B
?}B
G�B
N�B
N�B
R�B
T�B
XB
\)B
aHB
gmB
k�B
p�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�B
�B
�B
�B
�B
�~B
�|B
�~B
�vB
�B
�~B
�B
�B
�B
�B
�uB
�B
��B
��B
��BB	6B �B,B6CB:YBB�Br�B��BşB�$B�B��B,B�B"�B$�B2+B*�B)�BC�BXB`ABv�Bz�Bw�B�BB�7B��B��B��B��B��B�cB�]B�dB�dB�fB�dB�lB�nB�bB�mB�vB��B��B��B�B� B��B��B�<B}�Bx�Bm�BQ�B>sB(�B�BpBB�B�7B��B�yB��B�qB�DB�2B�B{�Bn�BJ�B,B$�B�BHB
�yB
�qB
��B
ebB
YB
N�B
5AB
hB

B	�B	�'B	�B	�=B	u�B	c^B	O�B	8\B	�B	OB	B�B�>B��B��BåB��B�DB�B��B��B�ZB�JB�GB�BB�=B�KB�VB�dB�mB�nB�bB�OB�IB�HB�BB�4B�0B�IB�@B�4B�6B�*B�B�#B�0B�/B�5B�6B�MB�NB�LB�LB�LB�YB�_B�oB�mB�yB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�
B�B�B�B�B�B�4B�FB�`B�eB�lB�}B�lB��B��B��B��B��BƳBǷBƴBƳBƵBƴB��B�wB�(B��B��B��B��B�B�B��B�	B�;B�FB�QB�>B�5B�3B�EB�wB�B�B�B��B��B��B��B��B��B�B�B�B�!B�*B�.B�-B�3B�:B�9B�:B�XB�kB�~B�}B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	B	%B	.B	8B		BB	
GB	5B	-B	&B	3B	;B		?B	KB	MB	QB	RB	RB	SB	YB	]B	}B	�B	�B	�B	�B	�B	�B	 �B	"�B	"�B	#�B	%�B	*B	/#B	4@B	8WB	;kB	?�B	B�B	D�B	E�B	F�B	H�B	P�B	UB	YB	XB	\-B	]5B	[%B	[(B	bQB	i~B	m�B	q�B	w�B	{�B	z�B	x�B	v�B	v�B	x�B	z�B	{�B	z�B	w�B	u�B	y�B	z�B	x�B	t�B	r�B	o�B	p�B	s�B	u�B	v�B	x�B	|�B	}�B	}�B	�B	�B	�$B	�)B	�<B	�@B	�GB	�NB	�TB	�^B	�gB	�kB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�4B	�`B	B	ŤB	ŢB	šB	ƫB	ǳB	ƩB	ƩB	ƪB	ǲB	ǰB	ȵB	ɽB	ɽB	ȷB	ǯB	ŢB	ţB	ƨB	ƩB	ƨB	ƩB	ƪB	ǮB	ȳB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�)B	�7B	�;B	�<B	�>B	�CB	�IB	�IB	�FB	�MB	�NB	�NB	�UB	�XB	�`B	�aB	�aB	�cB	�nB	�lB	�nB	�oB	�qB	�sB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B

?B
�B
�B
'�B
,B
5=B
?}B
G�B
N�B
N�B
R�B
T�B
XB
\$B
aCB
gkB
k�B
p�B
u�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.05 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436462016080714364620160807143646  AO  ARCAADJP                                                                    20160506022001    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160506022001  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160506022001  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143646  IP                  G�O�G�O�G�O�                