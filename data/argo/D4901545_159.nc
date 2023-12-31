CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-09-12T15:16:26Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  AL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  CD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  px   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  zL   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �,   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �,   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �,   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �,   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��        ��Argo profile    3.1 1.2 19500101000000  20170912151626  20181025093510  4901545 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  4741                            2C  D   NAVIS_A                         0185                            052512                          863 @�%�j ͭ1   @�%����@9W
=p���cJ��O�;1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   B   @@  @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9y�D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @C33@���@���A ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp��Bx33B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C�C�C�C�C&gC�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv&gCx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D��D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*	�D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2��D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9|�D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY	�DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3Dx3Dx�3Dy3Dy�3Dz3Dz�3D{	�D{�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aܛ�A܏\Aܕ�A�XA��A�Q�A�%A��A��HA���A�ȴAڲ-Aڧ�Aڧ�AڑhA�x�A�C�A�M�A�O�A�I�A�M�A�I�A�E�A�A�A�-A��AٮAٛ�A�x�A�5?A�t�AѓuA�A�AƗ�A���A��A� �A��A��wA��FA��\A���A�7LA�I�A�O�A�ĜA�l�A���A�XA�ƨA�S�A�bA�$�A�=qA��hA��-A��
A��A�A��A�l�A�dZA��A��A��A��yA��^A���A�n�A���A�G�A�XA�~�A��yA�`BA��^A��;A�bA��uA�VA��A�  A��DA��A���A��A�VA�O�A�JA�9XA���A�K�A�~�A��A���A��A�p�A��A��mA���A�bA�~�A��A~��A}�
A|��A|M�A{oAyp�Aw�mAu�#As��Aq|�Aox�Am�FAlVAj��AjffAh�9Af�Ae�PAe"�Ac`BAaA`ĜA_?}A]p�AZ�AY;dAX��AW��AV�jAU��AT��ASO�AR��AR�9ARA�AQ��AQVAPr�AO`BANALA�AK�mAK|�AJ �AI�7AI
=AHJAG��AF�uAE\)AD��AD-ACC�AB^5AA�TAA
=A@��A@VA@ �A?&�A>9XA=A=��A=`BA<VA;��A;S�A9�A8bNA7�A77LA6�A6ffA5��A4��A3�;A3G�A2�A25?A1�mA0��A0-A/t�A.�uA-�FA,�`A+�A*9XA)�PA);dA(r�A&�HA&I�A%�^A$�/A#t�A!�-A!��A!|�A!dZA!
=AVA�
A��A-A�A`BAM�A�AXA5?A��A�
A�A �Ap�A�RA�TA�AƨA"�A
ȴA
��A
5?A	�A	%A��A�!AjA��A��A|�A�mA��Al�A �A ĜA @���@��!@��P@�5?@�1'@�ff@���@�@���@�F@��j@�p�@�x�@�`B@���@�33@���@�\@�-@��@�O�@�n�@�M�@��#@��
@ݡ�@�I�@��@ڰ!@�ff@���@ٙ�@�G�@ؼj@ׅ@�@�A�@�5?@д9@���@�@��`@��
@�S�@��@��@�-@��@���@���@���@Ĭ@ģ�@��/@ļj@��@��
@���@�j@�@���@���@��R@�X@���@��@��9@��@��m@�ƨ@�33@��y@�v�@�n�@���@�G�@��@���@�bN@���@�O�@��@�V@��#@�@�x�@�/@���@�Z@�b@�ȴ@��@��@��;@�ƨ@��@��+@�@��@��j@��F@��@���@�S�@�o@��y@�~�@��^@�X@�hs@��#@�hs@��/@���@�z�@��@���@��@�^5@�V@��;@��!@���@�(�@��w@��w@���@�K�@�@���@��@�O�@��9@�9X@��F@�dZ@�33@�33@�;d@�;d@�33@�"�@�@���@�J@��#@��-@��h@�V@���@��@�(�@�  @��;@��w@���@��\@�v�@�@�p�@�G�@��@��`@��j@���@��u@�z�@�1'@���@�l�@��@�ȴ@�v�@�V@�{@��-@�X@�%@���@���@�Ĝ@��9@��@��@��@�j@�I�@�A�@�(�@� �@��@��F@�|�@�K�@�33@��@��!@�ff@�$�@���@���@�I�@�1'@�(�@�b@�  @�;@��@��@+@~�R@~$�@}?}@|�@|��@|j@|j@|Z@|1@{�
@{��@{S�@{@zn�@y��@yx�@y%@x�@w�@w��@w
=@v�y@vȴ@vv�@v5?@v5?@v$�@u�@up�@uV@t�@t�j@sƨ@sC�@r^5@p��@p1'@o��@o\)@o\)@oK�@oK�@o�P@o�@o�@pA�@p��@q��@r�\@r�!@r�@q�#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aܛ�A܏\Aܕ�A�XA��A�Q�A�%A��A��HA���A�ȴAڲ-Aڧ�Aڧ�AڑhA�x�A�C�A�M�A�O�A�I�A�M�A�I�A�E�A�A�A�-A��AٮAٛ�A�x�A�5?A�t�AѓuA�A�AƗ�A���A��A� �A��A��wA��FA��\A���A�7LA�I�A�O�A�ĜA�l�A���A�XA�ƨA�S�A�bA�$�A�=qA��hA��-A��
A��A�A��A�l�A�dZA��A��A��A��yA��^A���A�n�A���A�G�A�XA�~�A��yA�`BA��^A��;A�bA��uA�VA��A�  A��DA��A���A��A�VA�O�A�JA�9XA���A�K�A�~�A��A���A��A�p�A��A��mA���A�bA�~�A��A~��A}�
A|��A|M�A{oAyp�Aw�mAu�#As��Aq|�Aox�Am�FAlVAj��AjffAh�9Af�Ae�PAe"�Ac`BAaA`ĜA_?}A]p�AZ�AY;dAX��AW��AV�jAU��AT��ASO�AR��AR�9ARA�AQ��AQVAPr�AO`BANALA�AK�mAK|�AJ �AI�7AI
=AHJAG��AF�uAE\)AD��AD-ACC�AB^5AA�TAA
=A@��A@VA@ �A?&�A>9XA=A=��A=`BA<VA;��A;S�A9�A8bNA7�A77LA6�A6ffA5��A4��A3�;A3G�A2�A25?A1�mA0��A0-A/t�A.�uA-�FA,�`A+�A*9XA)�PA);dA(r�A&�HA&I�A%�^A$�/A#t�A!�-A!��A!|�A!dZA!
=AVA�
A��A-A�A`BAM�A�AXA5?A��A�
A�A �Ap�A�RA�TA�AƨA"�A
ȴA
��A
5?A	�A	%A��A�!AjA��A��A|�A�mA��Al�A �A ĜA @���@��!@��P@�5?@�1'@�ff@���@�@���@�F@��j@�p�@�x�@�`B@���@�33@���@�\@�-@��@�O�@�n�@�M�@��#@��
@ݡ�@�I�@��@ڰ!@�ff@���@ٙ�@�G�@ؼj@ׅ@�@�A�@�5?@д9@���@�@��`@��
@�S�@��@��@�-@��@���@���@���@Ĭ@ģ�@��/@ļj@��@��
@���@�j@�@���@���@��R@�X@���@��@��9@��@��m@�ƨ@�33@��y@�v�@�n�@���@�G�@��@���@�bN@���@�O�@��@�V@��#@�@�x�@�/@���@�Z@�b@�ȴ@��@��@��;@�ƨ@��@��+@�@��@��j@��F@��@���@�S�@�o@��y@�~�@��^@�X@�hs@��#@�hs@��/@���@�z�@��@���@��@�^5@�V@��;@��!@���@�(�@��w@��w@���@�K�@�@���@��@�O�@��9@�9X@��F@�dZ@�33@�33@�;d@�;d@�33@�"�@�@���@�J@��#@��-@��h@�V@���@��@�(�@�  @��;@��w@���@��\@�v�@�@�p�@�G�@��@��`@��j@���@��u@�z�@�1'@���@�l�@��@�ȴ@�v�@�V@�{@��-@�X@�%@���@���@�Ĝ@��9@��@��@��@�j@�I�@�A�@�(�@� �@��@��F@�|�@�K�@�33@��@��!@�ff@�$�@���@���@�I�@�1'@�(�@�b@�  @�;@��@��@+@~�R@~$�@}?}@|�@|��@|j@|j@|Z@|1@{�
@{��@{S�@{@zn�@y��@yx�@y%@x�@w�@w��@w
=@v�y@vȴ@vv�@v5?@v5?@v$�@u�@up�@uV@t�@t�j@sƨ@sC�@r^5@p��@p1'@o��@o\)@o\)@oK�@oK�@o�P@o�@o�@pA�@p��@q��@r�\@r�!@r�@q�#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB!�B �B�B�B{B	7B%BBB  BB��B��B��B��B��B�B��B��B��BB+B1B1B%B%B1B1B1B+B  B�B�
BƨBɺB�#B�`B�)BȴB�?B�B��B��B�{B�oB�oB�\B�7B� Bu�Bq�By�B_;BN�B`BBgmB`BBVBF�B8RB,B�B{BoB1B�yB�B��B��B�dB�^B�'B�B��B��B��B�\B�=B�B�B}�Bu�Bm�BcTBZBP�BD�B9XB5?B(�BPB
�B
�B
ȴB
�dB
B
�}B
�}B
ȴB
ĜB
�jB
�'B
��B
��B
�bB
�%B
~�B
p�B
_;B
L�B
5?B
�B
VB	��B	�B	�sB	�BB	�B	��B	�}B	�XB	�wB	�3B	��B	��B	�{B	�+B	w�B	p�B	n�B	o�B	k�B	e`B	aHB	ZB	ZB	^5B	^5B	_;B	\)B	ZB	R�B	J�B	A�B	>wB	<jB	7LB	33B	0!B	,B	(�B	$�B	�B	�B	�B	oB	JB	
=B	%B	B	B	B��B��B��B��B��B�B�B�sB�BB�#B�B�
B��B��B��BɺBŢBB�}B�jB�XB�?B�'B�!B�B�B��B��B��B��B��B�oB�VB�=B�%B�%B�B~�B}�B}�B|�Bx�Bl�BffBdZBe`BbNB\)BZBVBP�BK�BD�B@�B=qB;dB:^B;dB;dB:^B7LB49B2-B2-B2-B33B49B5?B5?B5?B5?B2-B,B%�B!�B�B�B�B�B�B�B(�B)�B$�B�BuBhB�B!�B.B8RB9XB;dBA�BF�BE�BE�BB�B:^B1'B�B\BJB
=B	7B
=BJBPBPBPBVBVB\BbB\BVBJB
=B+B+B1B
=BDB
=BDBVBbBbBbBbBuB�B�B�B�B�B �B �B �B�B�B�B�B�B �B"�B$�B$�B$�B%�B&�B,B2-B8RB:^B:^B;dB;dB<jB=qB;dB8RB;dB<jB@�BD�BG�BH�BI�BO�BT�BW
BXBXB[#B\)B\)B^5B^5BcTBiyBm�Bn�Bo�Bq�Bq�Bt�Bu�Bv�B�B�+B�DB�DB�JB�VB�VB�VB�bB�hB�\B�JB�PB�JB�bB�oB�uB��B��B��B��B��B��B��B��B��B�B��Br�B�B�B�B�B�B�FB�dB�qB�wBŢBȴBɺB��B��B��B��B��B�B�B�B�B�B�B�B�#B�#B�)B�/B�5B�TB�`B�yB�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	  B	B	B	B	B	B		7B	DB	PB	VB	hB	{B	�B	�B	�B	#�B	'�B	(�B	(�B	)�B	+B	+B	,B	,B	.B	1'B	5?B	;dB	>wB	@�B	B�B	C�B	C�B	F�B	I�B	K�B	L�B	O�B	S�B	W
B	XB	YB	YB	YB	ZB	ZB	[#B	[#B	]/B	_;B	_;B	`BB	dZB	gmB	jB	k�B	l�B	o�B	p�B	r�B	w�B	}�B	~�B	�B	�B	�B	�B	�7B	�=B	�DB	�VB	��B	��B	��B	��B	��B	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B"WB �B �B�BFB
?BjB^BjB BZB�B��B�KB�HB��B�B��B��B��BB1B6BbB�B�B]B�B�B	B!B��B�DB�
B�6B�yB�zB�xBϦB��B�!B��B�aB��B��B�/B��B�B�>B{�Bv�B�YBd�BS�Bb�Bi�BcCB[�BK�B<B/�B"0B�BB�B��B�*B��B�B�CB�^B��B�LB��B�eB��B��B��B�jB�-B��BxBpiBeB]UBUBG�B:�B8�B/SBB
��B
��B
̜B
��B
ŘB
��B
�wB
�,B
�'B
�B
�#B
��B
��B
��B
��B
�B
t�B
cYB
R~B
;	B
%6B
�B
 �B	�jB	� B	��B	�hB	�@B	��B	�qB	B	�B	��B	��B	��B	�HB	{B	q�B	p�B	qB	nB	f�B	d�B	[TB	Z=B	_7B	_�B	`�B	]�B	\�B	V
B	N�B	BXB	?rB	?TB	8�B	4WB	2oB	,�B	+oB	'|B	 B	B	�B	_B	ZB	B	0B	�B	�B	.B	#B��B�4B�pB�AB��B��B��B�&B��B�BB�B�-B�BАB˚B�BB��B�6B�UB��B��B�CB��B��B�RB��B��B��B��B��B��B��B��B��B��B��BBB~5B~8B}�B}sBoBg�Be�Bg�Bd�B^ B]GBXBR�BN�BFBA�B?�B<�B;�B=HB=vB=B8�B5B2�B3?B4'B4�B4�B5�B6B7�B7~B6B0�B(�B%�B #BHB�B_B�B�B*�B,�B'�B%(B\BxBQB JB-B8DB9�B<�BC�BG*BFBGBE�B<{B8TBB�B}B�B
�B�B�B�B�B�B�BB�B�BOB�BQB�B�BQB	�B
�BB�BLB�B{BhB_B�BvB0B�B�BB!@B#�B"�B"�B"B $ByB 	B!�B"SB#�B$�B%B%�B&>B'nB,B3 B8�B:�B:�B;�B=nB>sB?B=�B8�B;�B<�B@�BE\BH	BI'BK�BR�BU�BWSBX:BY;B[�B\�B]uB^�B_�Bc�Bi]Bm�Bn�Bo�BrEBr�BuABu�Bv)B��B��B�[B��B��B�B�B�NB��B��B��B�AB�:B��B�^B��B��B��B�B�tB�mB�B�aB�sB�@B�1B��B�wG�O�B�B�B�)B��B��B��B��B��B�"B��B�B�.B��B��B�	B�-B�0B�"B؝B��B�JB�XB�YB�OB�IB�+B�IBݔB�B�B�B�B��B��B��B�6B�EB�>B�B��B��B�B�4B��B��B	  B	 .B	B	,B	B	%B	�B		�B	�B	vB	�B	�B	�B	�B	�B	�B	$�B	(B	(�B	)B	*B	+B	+	B	,1B	,eB	.sB	1�B	5�B	;�B	>�B	@�B	B�B	C�B	C�B	F�B	I�B	K�B	L�B	P6B	T_B	W:B	XTB	YeB	YkB	Y+B	Z�B	Z-B	[/B	[OB	]UB	_2B	_AB	`�B	d\B	g�B	j�B	k�B	m B	o�B	q*B	s�B	x*B	~KB	B	��B	�B	�B	��B	�B	�B	�B	��B	��B	�B	��B	� B	��B	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<?>�<#�
<99�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<.D}<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =-0.05 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810231352052018102313520520181023135205  AO  ARCAADJP                                                                    20170912151626    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170912151626  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170912151626  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181023135205  QC  PRES            @@  D{� G�O�                PM  ARSQCTM V1.1                                                                20181023135205  QC  PSAL            @@  D{� G�O�                PM  ARSQSIQCV2.0WOD2001 & Argo                                                  20181024103353  CF  PSAL            D>3D>3?�                  PM  ARSQOWGUV1.0CTD_2017v1 + Argo_2017v02                                       20181025093510  IP                  G�O�G�O�G�O�                