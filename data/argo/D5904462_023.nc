CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:24Z AOML 3.0 creation; 2016-08-07T21:51:12Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221424  20160807145113  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_023                   2C  D   APEX                            6529                            072314                          846 @�*z��@1   @�*{K�@@2	x����d;"��`B1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33Bș�B˙�B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DH��DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dyl�D�3D�C3D�y�D�ɚD� D�C3D�c3D��fD���D�6fD���D�ٚD�  D�P Dڃ3D��fD�3D�VfD�p D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @L��@���@ə�A��A$��AD��Ad��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B	33B33B33B!33B)33B133B933BA33BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�34B�34B�fgBԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B���B���B���C L�CfgCL�CL�CL�C
L�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVfgCXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D��D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-��D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI�DI�3DJ3DJ�3DK3DK�3DL3DL�3DM�DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt��Dy� D��D�L�D��4D��4D��D�L�D�l�D�� D��gD�@ D��gD��4D�	�D�Y�Dڌ�D�� D��D�` D�y�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�"�A�JA���AҮAҟ�Aҏ\AҍPAҋDAҋDA҉7A҅A҃A҃A҃A�|�A�|�A�|�A�|�A�|�A�z�AҁA҇+Aҧ�A���A��A���A�ƨAҾwAҩ�AҶFAҏ\Aҗ�Aҩ�A���A�I�A�XAӓuAӶFAӮAӮA�p�A��A�  A�bA��A�%AҺ^Aѧ�A���A��`Aȗ�A���A�(�A�9XA�jA��\A��\A���A��A�v�A���A�jA�/A��FA��A��9A�-A�;dA��A��/A��7A���A�1A��HA�1'A�(�A��A�
=A��
A�ƨA�
=A��wA�=qA���A�G�A��7A�7LA���A�C�A���A��wA���A�%A��+A���A��A��-A���A���A�(�A�`BA��yA��A���A���A��#A��-A�7LA�`BA�~�A~��Ay�Av��Ar�RAn�Aj��Ag7LAe�Ac��Ac/AcVAb�DAa�PA_�FA]O�A\5?A[��AYoAR�ANQ�ALĜAJ��AI�AG/ABz�AA
=A?S�A>�9A=�TA<�yA;\)A;/A:n�A9�A7%A2�A1x�A0z�A.ĜA,��A*��A'�A#�-AO�A��A �AhsA�DAz�A��A��A%A�7AVA�9A��A�RA�mA�A��A��A�jA�+A�A��A�A�A�A$�A�#A?}A�jAM�A��At�AA��AbNA�A7LA
=A
ȴA
A	��A$�A�FA��A�A�A�!AE�A��A�Ap�A��Ar�A`BA&�A%Ar�A�#AG�A �HA �uA ~�A -@��
@�;d@��#@�+@���@�E�@�$�@�@���@�?}@���@�I�@�(�@�1@��F@���@���@���@��/@�A�@��;@�|�@��@�^5@�$�@�@��`@@���@�@�V@�@�D@땁@�v�@��@�9@�"�@��@�V@���@�V@�@��@�33@���@�$�@���@��@��@�I�@�\)@�o@��H@�^5@݉7@��/@���@�bN@�t�@�o@�o@ڧ�@�$�@�p�@�/@ؓu@�  @�+@�ȴ@�ff@�@�p�@�7L@�(�@ӶF@�|�@���@��H@�$�@��@��`@Гu@�Z@�I�@�I�@�(�@��m@��
@Ϯ@ϝ�@ϕ�@υ@�
=@Χ�@�^5@�M�@�-@��@͙�@�&�@���@�z�@��@���@ˍP@��y@��#@�G�@�?}@��@��@���@�bN@��
@��m@�J@�Q�@�b@�  @�1@�C�@�~�@�E�@�$�@��#@���@��j@�r�@��@�ƨ@��w@�"�@��@��@���@���@�E�@���@�bN@�  @��m@��
@��
@�ƨ@��P@�S�@��H@�V@��-@��@��j@�r�@��m@���@�S�@��@���@��\@��@���@�x�@�X@��j@�  @��
@��F@�|�@���@��+@��@���@�G�@�&�@��`@��j@��D@�I�@�ƨ@��@�t�@�33@��H@���@��\@���@���@�I�@��@���@��F@�ff@�$�@���@��T@�X@���@��@�  @�t�@�"�@�ȴ@���@��h@�p�@�/@���@���@�9X@��P@�dZ@�+@�@�ȴ@�=q@���@�p�@�7L@��@��9@��@��@��P@�33@�
=@��@���@�n�@�5?@�G�@��j@��D@�1'@���@�S�@�C�@�o@���@�5?@��#@�hs@��`@���@�A�@��;@��
@�ƨ@��F@�dZ@�+@���@��\@��@��T@���@�p�@��j@�9X@���@�|�@�"�@��@�V@�=q@�5?@�5?@�5?@�$�@��@�{@��@��@��@�33@�E�@�$�@�x�@{��@n5?@b-@W�;@O
=@G��@@��@;�m@5��@0��@,�D@%�h@ A�@@�w@j@Ĝ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�"�A�JA���AҮAҟ�Aҏ\AҍPAҋDAҋDA҉7A҅A҃A҃A҃A�|�A�|�A�|�A�|�A�|�A�z�AҁA҇+Aҧ�A���A��A���A�ƨAҾwAҩ�AҶFAҏ\Aҗ�Aҩ�A���A�I�A�XAӓuAӶFAӮAӮA�p�A��A�  A�bA��A�%AҺ^Aѧ�A���A��`Aȗ�A���A�(�A�9XA�jA��\A��\A���A��A�v�A���A�jA�/A��FA��A��9A�-A�;dA��A��/A��7A���A�1A��HA�1'A�(�A��A�
=A��
A�ƨA�
=A��wA�=qA���A�G�A��7A�7LA���A�C�A���A��wA���A�%A��+A���A��A��-A���A���A�(�A�`BA��yA��A���A���A��#A��-A�7LA�`BA�~�A~��Ay�Av��Ar�RAn�Aj��Ag7LAe�Ac��Ac/AcVAb�DAa�PA_�FA]O�A\5?A[��AYoAR�ANQ�ALĜAJ��AI�AG/ABz�AA
=A?S�A>�9A=�TA<�yA;\)A;/A:n�A9�A7%A2�A1x�A0z�A.ĜA,��A*��A'�A#�-AO�A��A �AhsA�DAz�A��A��A%A�7AVA�9A��A�RA�mA�A��A��A�jA�+A�A��A�A�A�A$�A�#A?}A�jAM�A��At�AA��AbNA�A7LA
=A
ȴA
A	��A$�A�FA��A�A�A�!AE�A��A�Ap�A��Ar�A`BA&�A%Ar�A�#AG�A �HA �uA ~�A -@��
@�;d@��#@�+@���@�E�@�$�@�@���@�?}@���@�I�@�(�@�1@��F@���@���@���@��/@�A�@��;@�|�@��@�^5@�$�@�@��`@@���@�@�V@�@�D@땁@�v�@��@�9@�"�@��@�V@���@�V@�@��@�33@���@�$�@���@��@��@�I�@�\)@�o@��H@�^5@݉7@��/@���@�bN@�t�@�o@�o@ڧ�@�$�@�p�@�/@ؓu@�  @�+@�ȴ@�ff@�@�p�@�7L@�(�@ӶF@�|�@���@��H@�$�@��@��`@Гu@�Z@�I�@�I�@�(�@��m@��
@Ϯ@ϝ�@ϕ�@υ@�
=@Χ�@�^5@�M�@�-@��@͙�@�&�@���@�z�@��@���@ˍP@��y@��#@�G�@�?}@��@��@���@�bN@��
@��m@�J@�Q�@�b@�  @�1@�C�@�~�@�E�@�$�@��#@���@��j@�r�@��@�ƨ@��w@�"�@��@��@���@���@�E�@���@�bN@�  @��m@��
@��
@�ƨ@��P@�S�@��H@�V@��-@��@��j@�r�@��m@���@�S�@��@���@��\@��@���@�x�@�X@��j@�  @��
@��F@�|�@���@��+@��@���@�G�@�&�@��`@��j@��D@�I�@�ƨ@��@�t�@�33@��H@���@��\@���@���@�I�@��@���@��F@�ff@�$�@���@��T@�X@���@��@�  @�t�@�"�@�ȴ@���@��h@�p�@�/@���@���@�9X@��P@�dZ@�+@�@�ȴ@�=q@���@�p�@�7L@��@��9@��@��@��P@�33@�
=@��@���@�n�@�5?@�G�@��j@��D@�1'@���@�S�@�C�@�o@���@�5?@��#@�hs@��`@���@�A�@��;@��
@�ƨ@��F@�dZ@�+@���@��\@��@��T@���@�p�@��j@�9X@���@�|�@�"�@��@�V@�=q@�5?@�5?@�5?@�$�@��@�{@��@��G�O�@�33@�E�@�$�@�x�@{��@n5?@b-@W�;@O
=@G��@@��@;�m@5��@0��@,�D@%�h@ A�@@�w@j@Ĝ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB\BJB
=B	7B1B1B+B+B+B+B+B+B+B1B+B+B+B+B+B+B
=BPB�BE�BM�BM�BM�BL�BK�BVBXB[#BaHBs�B�1B�JB��B��B��B��B��B�\B�=B�uB��B�B�^B��B��B�5B+BW
BN�BH�BG�BQ�B\)BjBn�Bp�B~�B�1B�hB��B��B��B��B��B�B�B��B��B��B�\B~�Bz�Br�BaHBQ�BI�B9XB1'B-B#�BB�B�B�/B��B��B�}B��BffBJ�B@�B8RB{B
�ZB
�/B
ĜB
��B
�hB
�DB
{�B
]/B
N�B
K�B
D�B
7LB
(�B
+B	�fB	��B	�'B	�VB	w�B	cTB	W
B	P�B	N�B	M�B	I�B	D�B	:^B	.B	&�B	 �B	uB��B�B�B�sB�TB�5B�B��B��B��B��B��B��B��B��B��B��B��B�B�#B�B�B��BÖB�9B�!B�dB�?B��B��B�B��BŢBǮB��B�ZB�B��B��B	B	B	
=B	VB	VB	VB	bB	oB	{B	uB	JB	%B	B	+B	1B	
=B	oB	�B	�B	"�B	2-B	2-B	33B	33B	49B	6FB	7LB	<jB	>wB	>wB	=qB	=qB	=qB	>wB	@�B	@�B	?}B	A�B	B�B	G�B	I�B	J�B	K�B	L�B	M�B	P�B	Q�B	Q�B	R�B	Q�B	P�B	O�B	Q�B	R�B	S�B	S�B	T�B	VB	W
B	ZB	ZB	[#B	[#B	\)B	[#B	aHB	cTB	bNB	cTB	cTB	cTB	e`B	gmB	gmB	l�B	l�B	l�B	n�B	p�B	r�B	u�B	y�B	|�B	~�B	~�B	|�B	}�B	�B	�B	�B	�DB	�PB	�\B	�bB	�hB	�oB	�oB	�uB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�'B	�3B	�9B	�?B	�LB	�XB	�dB	�jB	�wB	�wB	ÖB	ƨB	ǮB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�/B	�;B	�BB	�BB	�;B	�5B	�BB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�ZB	�fB	�fB	�fB	�`B	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�mB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
+B
+B

=B
	7B
hB
�B
�B
�B
'�B
2-B
;dB
=qB
E�B
L�B
P�B
VB
ZB
]/B
dZB
hsB
m�B
p�B
s�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BHB5B
$B	!BBBBBBBBBBBBBBBBB
(B;B�BE�BM�BM�BM�BL�BK�BU�BW�B[Ba/Bs�B�B�5B�|B��B��B��B��B�EB�&B�^B��B��B�GB�qBʪB�BBV�BN�BH�BG�BQ�B\BjeBnBp�B~�B�B�NB��B��B��B��B��B��B��B��B��B�~B�DB~�Bz�Br�Ba)BQ�BI�B9=B1
B,�B#�B�B�aB��B�BͶB˪B�dB��BfIBJ�B@iB87B_B
�AB
�B
ĀB
��B
�NB
�(B
{�B
]B
N�B
K�B
D�B
74B
(�B
B	�PB	��B	�B	�CB	w�B	cBB	V�B	P�B	N�B	M�B	I�B	D�B	:MB	.B	&�B	 �B	eB��B�B�B�dB�FB�'B��B��B��B��B��B��B��B��B��B��B̾B��B�B�B�B� B��BÈB�+B�B�SB�0B��B��B�B�sBőBǝB��B�JB�B��B��B	�B	B	
*B	CB	CB	DB	OB	YB	gB	^B	4B	B	B	B	B	
(B	XB	xB	�B	"�B	2B	2B	3B	3B	4#B	6.B	73B	<RB	>aB	>`B	=ZB	=[B	=YB	>^B	@oB	@kB	?iB	ArB	BwB	G�B	I�B	J�B	K�B	L�B	M�B	P�B	Q�B	Q�B	R�B	Q�B	P�B	O�B	Q�B	R�B	S�B	S�B	T�B	U�B	V�B	ZB	ZB	[B	[
B	\B	[B	a/B	c=B	b5B	c<B	c<B	c<B	eEB	gUB	gUB	lpB	lrB	lrB	n}B	p�B	r�B	u�B	y�B	|�B	~�B	~�B	|�B	}�B	��B	��B	�B	�*B	�6B	�AB	�IB	�JB	�RB	�SB	�[B	�RB	�SB	�eB	�jB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	� B	�/B	�:B	�FB	�LB	�ZB	�[B	�zB	ƊB	ǑB	ǒB	ȖB	ȘB	ʤB	̮B	ͷB	͵B	λB	νB	λB	λB	μB	λB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	˩B	ʤB	λB	��B	��B	ʹB	͵B	λB	ͶB	κB	��B	��B	��B	��B	��B	��B	�B	�B	�%B	�$B	�B	�B	�%B	�/B	�/B	�-B	�7B	�5B	�6B	�4B	�4B	�5B	�2B	�:B	�BB	�CB	�AB	�?B	�@B	�?B	�CB	�AB	�>B	�GB	�HB	�GB	�@B	�:B	�;B	�;B	�@B	�GB	�HB	�EB	�IB	�MB	�OB	�FB	�GB	�FB	�GB	�GB	�CB	�EB	�GB	�NB	�FB	�FB	�HB	�NB	�VB	�UB	�SB	�QB	�^B	�aB	�eB	�gB	�jB	�lB	�qB	�sB	�xB	�yB	�B	�}B	�B	�}B	�~B	�}B	�~B	�zB	�wB	�|B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
�B
 B
B
B
B
B
B
B
B
B
G�O�B
	B
GB
rB
rB
�B
'�B
2B
;BB
=OB
E�B
L�B
P�B
U�B
Y�B
]B
d6B
hOB
mkB
p�B
s�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.3 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451132016080714511320160807145113  AO  ARCAADJP                                                                    20150226221424    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221424  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221424  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145113  IP                  G�O�G�O�G�O�                