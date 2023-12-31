CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-05-16T19:15:57Z AOML 3.0 creation; 2016-08-07T21:36:46Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160516191557  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               vA   AO  5286_8897_118                   2C  D   APEX                            6531                            072314                          846 @׬�8��1   @׬��~y@4@ě��T�c?����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    vA   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C33C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dyy�D��D�6fD���D�ٚD�fD�I�D��fD��fD��D�@ D�S3D�ٚD�	�D�I�D�|�D� D��D�@ D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���A ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(33B033B833B@33BH��BP33BX33B`33Bh33Bp33Bx33B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C &gC@ C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK��DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Dt�3Dy|�D�gD�8 D��4D��4D� D�K4D�� D�� D�4D�A�D�T�D��4D�4D�K4D�~gDౚD�4D�A�D�gD��g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A�r�A�r�A�t�A͍PA��A� �A�JA���A���A��;A��A���Aʹ9A�M�A�-A�(�A�
=A���A��;A̝�Ȁ\A�r�A�ZA��;A˰!Aˣ�A�l�A�ffA�bA��Aɰ!A�x�A�Q�A�{AȑhAǅAƥ�A�M�A�G�A��A��Aŝ�A�VA���Aĉ7AÛ�A�1A®A��A���A�9XA�x�A���A��A� �A�%A��wA��A�v�A�-A���A���A��#A��A�7LA���A���A�E�A��RA���A���A�1A���A�oA�C�A�1A�?}A�C�A�l�A�oA��/A��^A�5?A��!A���A�=qA�n�A�1'A�E�A��A��A��7A���A�n�A���A��\A�Q�A��!A�p�A�ffA���A���A���A�r�A�ĜA���A�A�;A}�A{�Ax��Ax  AwAs�-Ap�An�Aj�/AjffAh�jAf�DAd��Ab��Aa�wA`�`A_�FA\�/AZ �AXAV�jAUXAS33AR9XAP��AO7LAOoAOVAOAN��AN�\AM%AK�wAJbAH�AF��AE��ACO�A@r�A?�A>�A=C�A:M�A7G�A4ffA3�TA2ĜA1G�A.�`A-�wA,�A,VA+/A*{A(��A&��A&�\A$-A"Q�A!O�A r�A �A�mA\)AVA�FA�jA=qA  A�A�/AA�A%A�Al�Av�A/A�A��A �A��A�;A�A�
A��A�uA?}Ap�A��AffAƨA	�mAI�A&�A��A�A�A  A�FA��A?}A�/AQ�A�A �j@�$�@��-@��@��m@�@�^5@��@�p�@��`@��@��m@��@���@�@���@���@��@� �@�Ĝ@�R@���@�J@���@�V@�1@�^5@�G�@��@߾w@���@��#@� �@��@ؼj@�Ĝ@ؓu@ؓu@�  @�  @�~�@��@�r�@�b@϶F@�l�@�\)@��@��@��`@��m@�
=@ʏ\@�5?@�$�@ɺ^@���@�Z@���@��y@Ɨ�@�{@��@��T@��T@��T@�x�@�Z@��@���@�@�p�@��@��j@��@���@��@��@�@�x�@��j@�1@�|�@�+@�v�@��-@�G�@��`@��`@��`@���@�bN@��@�|�@��@�^5@��h@�`B@��@��T@�x�@���@���@��@���@���@���@�v�@�@�G�@�7L@�I�@���@��F@�33@�~�@���@�V@� �@��F@���@�|�@��@�@�o@���@��@��!@��\@�J@�X@�  @�;d@��@�V@�@��@��T@��-@�`B@�%@���@�(�@�  @��@��F@�l�@�C�@�K�@�"�@�\)@�t�@�+@���@�ff@�-@���@���@���@��h@��-@���@���@��@�^5@�^5@�O�@��@��@�J@�E�@��@��j@���@��D@�A�@�S�@�l�@�+@��\@�
=@���@��@��#@��7@���@�hs@��@���@�A�@���@�K�@��@�v�@�-@�-@�$�@�-@�J@���@���@��h@��h@�hs@�&�@���@��/@��j@�bN@�1'@�  @��F@�l�@�K�@�@���@��#@�X@�%@��@��@�bN@�(�@���@�dZ@�;d@���@���@���@���@�$�@��#@�`B@�7L@�V@���@���@�r�@�Q�@�  @��@��@�ƨ@���@�C�@��H@��+@�M�@�E�@�=q@�5?@�$�@�{@��@���@��h@�hs@�/@�Ĝ@��@�j@�bN@�I�@��w@�;d@�"�@�+@�+@�+@�+@�33@�"�@��y@���@���@��@��w@�n�@z~�@t9X@l�j@cdZ@\�@U/@N�y@F��@>@7
=@1��@-p�@)X@$�j@�R@�`@��@b@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�p�A�r�A�r�A�t�A͍PA��A� �A�JA���A���A��;A��A���Aʹ9A�M�A�-A�(�A�
=A���A��;A̝�Ȁ\A�r�A�ZA��;A˰!Aˣ�A�l�A�ffA�bA��Aɰ!A�x�A�Q�A�{AȑhAǅAƥ�A�M�A�G�A��A��Aŝ�A�VA���Aĉ7AÛ�A�1A®A��A���A�9XA�x�A���A��A� �A�%A��wA��A�v�A�-A���A���A��#A��A�7LA���A���A�E�A��RA���A���A�1A���A�oA�C�A�1A�?}A�C�A�l�A�oA��/A��^A�5?A��!A���A�=qA�n�A�1'A�E�A��A��A��7A���A�n�A���A��\A�Q�A��!A�p�A�ffA���A���A���A�r�A�ĜA���A�A�;A}�A{�Ax��Ax  AwAs�-Ap�An�Aj�/AjffAh�jAf�DAd��Ab��Aa�wA`�`A_�FA\�/AZ �AXAV�jAUXAS33AR9XAP��AO7LAOoAOVAOAN��AN�\AM%AK�wAJbAH�AF��AE��ACO�A@r�A?�A>�A=C�A:M�A7G�A4ffA3�TA2ĜA1G�A.�`A-�wA,�A,VA+/A*{A(��A&��A&�\A$-A"Q�A!O�A r�A �A�mA\)AVA�FA�jA=qA  A�A�/AA�A%A�Al�Av�A/A�A��A �A��A�;A�A�
A��A�uA?}Ap�A��AffAƨA	�mAI�A&�A��A�A�A  A�FA��A?}A�/AQ�A�A �j@�$�@��-@��@��m@�@�^5@��@�p�@��`@��@��m@��@���@�@���@���@��@� �@�Ĝ@�R@���@�J@���@�V@�1@�^5@�G�@��@߾w@���@��#@� �@��@ؼj@�Ĝ@ؓu@ؓu@�  @�  @�~�@��@�r�@�b@϶F@�l�@�\)@��@��@��`@��m@�
=@ʏ\@�5?@�$�@ɺ^@���@�Z@���@��y@Ɨ�@�{@��@��T@��T@��T@�x�@�Z@��@���@�@�p�@��@��j@��@���@��@��@�@�x�@��j@�1@�|�@�+@�v�@��-@�G�@��`@��`@��`@���@�bN@��@�|�@��@�^5@��h@�`B@��@��T@�x�@���@���@��@���@���@���@�v�@�@�G�@�7L@�I�@���@��F@�33@�~�@���@�V@� �@��F@���@�|�@��@�@�o@���@��@��!@��\@�J@�X@�  @�;d@��@�V@�@��@��T@��-@�`B@�%@���@�(�@�  @��@��F@�l�@�C�@�K�@�"�@�\)@�t�@�+@���@�ff@�-@���@���@���@��h@��-@���@���@��@�^5@�^5@�O�@��@��@�J@�E�@��@��j@���@��D@�A�@�S�@�l�@�+@��\@�
=@���@��@��#@��7@���@�hs@��@���@�A�@���@�K�@��@�v�@�-@�-@�$�@�-@�J@���@���@��h@��h@�hs@�&�@���@��/@��j@�bN@�1'@�  @��F@�l�@�K�@�@���@��#@�X@�%@��@��@�bN@�(�@���@�dZ@�;d@���@���@���@���@�$�@��#@�`B@�7L@�V@���@���@�r�@�Q�@�  @��@��@�ƨ@���@�C�@��H@��+@�M�@�E�@�=q@�5?@�$�@�{@��@���@��h@�hs@�/@�Ĝ@��@�j@�bN@�I�@��w@�;d@�"�@�+@�+@�+@�+@�33@�"�@��y@���@���G�O�@��w@�n�@z~�@t9X@l�j@cdZ@\�@U/@N�y@F��@>@7
=@1��@-p�@)X@$�j@�R@�`@��@b@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B
1B
YB
�B
�?B
�XB
��B
�/B
�/B
�/B
�5B
�ZB
�`B
�`B
�ZB
�ZB
�ZB
�yB
�B
�B
�B
��B
��B
��B
��B{B�BoBuB�B�B#�B$�B�B@�Be`Bk�Br�Bx�B�JB�{B��B��B�B�RBB�B�yB��B%BB��B��BPB&�B&�B!�B#�B)�B.BA�BH�B5?BL�Bl�Bo�Bm�B_;BVB^5BH�B;dB<jBaHBZB'�B�B�BB{B+B9XB9XB1'B%�B�B��B��B�B��B��B��B��B�7Bq�B\)BC�B+BVB
�?B
R�B
33B
�B
\B
B	��B	�B	�NB	��B	��B	��B	ƨB	�3B	��B	��B	�B	~�B	u�B	n�B	e`B	\)B	W
B	R�B	K�B	>wB	1'B	'�B	!�B	�B	oB	VB		7B	%B	B	B	B	B	  B��B��B�B�B�`B�BB�B��BǮBÖB�}B�RB�3B�B�B�B��B��B��B��B��B��B�-B�B�B�LB�?B�B�-B�3B�3B�-B�9B�?B�9B�FB�FB�FB�LB�LB�RB�XB�LB�?B�?B�?B�RB�}B�qB�?B�?B�3B�B�B�B�dBB��B�qB�LB��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B��BĜBŢBĜB��B��B��B��BƨBÖB��B�wBB��B�B�
B��B��B��B��B��B��B��B��BǮBȴBɺB��B��B��B��B��B��B��B��B��B�B�B�B�B�#B�5B�;B�NB�sB�yB�B�B�B�B�B��B��B��B	  B	  B	B	B	B	B	%B	%B	%B	
=B	DB	\B	uB	{B	�B	�B	�B	�B	�B	�B	!�B	#�B	(�B	.B	0!B	0!B	0!B	2-B	33B	49B	7LB	;dB	;dB	?}B	D�B	E�B	F�B	G�B	F�B	G�B	H�B	J�B	K�B	O�B	Q�B	S�B	VB	ZB	[#B	^5B	bNB	ffB	k�B	l�B	o�B	q�B	q�B	p�B	q�B	r�B	r�B	r�B	r�B	t�B	y�B	~�B	�B	�B	�%B	�1B	�=B	�JB	�\B	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�9B	�?B	�?B	�RB	�jB	�}B	��B	�wB	�^B	�XB	�XB	�RB	�RB	�qB	�wB	�}B	B	ÖB	ĜB	ƨB	ƨB	ȴB	ɺB	ɺB	ȴB	ȴB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�)B	�)B	�)B	�/B	�5B	�;B	�HB	�NB	�TB	�TB	�TB	�`B	�`B	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
JB	��B
1B
hB
�B
�B
%�B
-B
2-B
8RB
?}B
G�B
M�B
P�B
VB
ZB
^5B
cTB
iyB
n�B
r�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	��B	��B	��B	��B
7B
YB
�B
�>B
�VB
��B
�.B
�+B
�-B
�3B
�YB
�]B
�^B
�XB
�VB
�YB
�|B
�B
�B
�B
��B
��B
��B
��ByB~BjBuB�B�B#�B$�B�B@Be]Bk�Br�Bx�B�IB�wB��B��B�B�MBB�B�uB��B"BB��B��BMB&�B&�B!�B#�B)�B.BA�BH�B5;BL�Bl�Bo�Bm�B_5BV B^0BH�B;aB<eBa?BZB'�B�B�?ByB*�B9TB9SB1%B%�B�B��B��B�B��B��B��B��B�4Bq�B\$BC�B*�BUB
�9B
R�B
34B
�B
]B
B	��B	�B	�QB	��B	��B	��B	ƭB	�6B	��B	��B	�B	B	u�B	n�B	egB	\0B	WB	R�B	K�B	>�B	10B	'�B	!�B	�B	xB	aB		BB	1B	*B	$B	%B	B	 B��B��B�B�B�mB�NB�+B��BǻBäB��B�`B�?B�'B�$B�B��B��B��B��B��B��B�;B�B�"B�[B�NB�$B�<B�@B�>B�;B�HB�NB�HB�SB�VB�UB�[B�ZB�^B�dB�XB�MB�KB�LB�`B��B�}B�LB�KB�@B�+B�B�*B�pBB��B�~B�ZB�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B� B�FB��BīBŭBĨB��B��B��B��BƲBâB��B��BB��B�B�B�B��B��B��B��B�B� B��BǺB��B��B��B��B��B��B��B��B��B��B�
B�B� B�(B�)B�-B�@B�EB�YB�}B�B�B�B�B�B�B��B��B��B	 	B	 B	B	B	B	'B	.B	-B	-B	
EB	KB	dB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	(�B	.B	0(B	0)B	0(B	22B	3:B	4@B	7SB	;iB	;iB	?�B	D�B	E�B	F�B	G�B	F�B	G�B	H�B	J�B	K�B	O�B	Q�B	TB	V
B	Z"B	[(B	^;B	bQB	fjB	k�B	l�B	o�B	q�B	q�B	p�B	q�B	r�B	r�B	r�B	r�B	t�B	y�B	~�B	�B	�B	�(B	�4B	�BB	�NB	�_B	�sB	�xB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�+B	�;B	�AB	�BB	�SB	�kB	�B	��B	�yB	�bB	�ZB	�[B	�UB	�VB	�qB	�xB	�}B	B	ØB	ğB	ƫB	ƩB	ȶB	ɽB	ɼB	ȵB	ȵB	ǱB	ǱB	ǰB	ɻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	� B	�(B	�*B	�(B	�0B	�5B	�<B	�HB	�MB	�UB	�UB	�VB	�aB	�_B	�nB	�sB	�zB	�xB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B
/B
gB
�B
�B
%�B
-B
2+B
8QB
?zB
G�B
M�B
P�B
VB
ZB
^2B
cRB
itB
n�B
r�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.05 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436462016080714364620160807143646  AO  ARCAADJP                                                                    20160516191557    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160516191557  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160516191557  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143646  IP                  G�O�G�O�G�O�                