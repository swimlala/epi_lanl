CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-27T19:23:17Z AOML 3.0 creation; 2016-06-01T00:08:23Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150327192317  20160531170824  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               nA   AO  4055_7112_110                   2C  D   APEX                            5374                            041511                          846 @�D��h�	1   @�D�� _�@:L������d3|�hs1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    nA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy�fD��D�6fD��3D��3D���D�I�D�p D�ٚD�	�D�@ D��3D�ɚD�3D�I�Dڣ3D�ٚD�	�D�)�D�s3D�C311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @ə�A��A$��AD��Ad��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B	33B33B33B!33B)33B133B933BA33BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B���B���B���C L�CL�CL�CL�CL�C
L�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*��D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dty�Dy��D�#4D�@ D���D���D��gD�S4D�y�D��4D�4D�I�D���D��4D��D�S4Dڬ�D��4D�4D�34D�|�D�L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��^A���A�A�A�ĜA�ĜA���A��jA��-A���A�|�A�C�A�+A� �A��A�oA�oA�oA�oA�JA�JA�1A�A�A�  A���A��A���A��A��mA�$�A���A�`BA�JA��^A�M�A���A��`A���A�I�A���A��wA���A�ffA�=qA��A�A���A��hA�|�A�ffA�Q�A�5?A�=qA�O�A�G�A�E�A�\)A�"�A��+A��\A�  A�-A�bA���A���A�x�A�-A�x�A���A�t�A�I�A�7LA�$�A��yA�5?A��HA�oA�n�A�JA�A7LA~�!A~ZA}K�Az  AxAw�FAwt�Av1'AuO�Atn�Ar^5Aq��Ao��An�+AnM�Am�TAl��Al��Al�DAk��AhĜAhv�AgK�Ad�Ac33AbVAat�AaoA`�9A`bNA_��A^�yA^~�A^bNA^=qA^1'A^bA]�-A]�hA]dZA]oA\��A\E�A[�hA[&�AZ�9AZ(�AYS�AYoAX�\AV�AT�+ASS�AR��AQAO��AN��ANv�ANQ�AN$�AMl�AJ��AI��AIC�AH$�AG33AE�;AE��AEO�ACƨACoA@��A>�A>A:z�A9�A8VA6�uA5��A5%A3��A3A2�A1��A0�A01'A/7LA-&�A+�A*-A)��A)t�A)hsA)dZA)`BA)\)A)C�A(�yA(1'A'p�A&��A&r�A&{A%&�A#�A"�jA!;dA (�A"�Az�A�A33A��AĜAn�A  AoA\)A��A��Ax�A?}A��A�A��A�yA�!AQ�A�TA��At�AG�A&�AA�!AI�A
��A	S�AA�A�A�wA�-A�A�hAVA�A�hA��A��A �D@���@��F@�-@�G�@�A�@�+@��+@��-@���@�b@�@�v�@�&�@�F@��H@��@��@�{@��@��@�E�@��m@㝲@�@�n�@�o@އ+@�?}@۶F@�O�@�1@׾w@�v�@��
@ёh@�{@̴9@�33@�=q@�O�@ȴ9@�I�@� �@��m@ǝ�@�C�@��@��`@�K�@�/@�K�@�^5@��@���@���@�r�@�(�@��w@��P@�K�@�=q@���@���@��@�@��+@�/@��;@���@��H@�M�@���@��P@�?}@���@���@��`@�Ĝ@��9@��9@���@�r�@��@�=q@��j@�ƨ@�K�@��@��!@�@���@���@�5?@�p�@�dZ@���@�=q@�J@���@�`B@��@��j@�9X@��@���@�hs@�O�@��@���@��j@���@��u@�r�@�(�@��
@��@�l�@�\)@�S�@�"�@��H@��+@�J@��-@�O�@��j@��@�^5@��@��u@��m@�\)@��H@��@��@�(�@���@��@�|�@�l�@�l�@�l�@�dZ@�dZ@�\)@�\)@�S�@�S�@�S�@�S�@�S�@�K�@�C�@�33@�+@��@�
=@���@���@��@��y@��H@�ȴ@��!@�E�@�bN@���@��P@�\)@��H@�ff@���@��-@��7@�p�@�X@�G�@�G�@�?}@�/@�&�@�&�@��@��@�%@��/@��u@�b@���@���@��@�\)@���@��-@�&�@�V@��`@�Ĝ@���@��@�r�@�j@�Z@�(�@�  @~�@}��@}O�@}/@|��@|�@{�m@{�
@{ƨ@{��@{o@z^5@y��@y�#@y��@y�@xĜ@x�u@x�@xbN@x �@x �@xb@x �@x �@x �@x �@x �@w�@w��@w�@w�P@w\)@wl�@wK�@wK�@w;d@w�@w+@v��@v�@v�@v�@vȴ@vȴ@vȴ@v�@v�@v�@v�@v�@vff@m�T@g�@^��@Vv�@O\)@Fȴ@@ �@:J@7�@.ff@*M�@$(�@�+@�^@
=@o@?}@Ĝ@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��^A���A�A�A�ĜA�ĜA���A��jA��-A���A�|�A�C�A�+A� �A��A�oA�oA�oA�oA�JA�JA�1A�A�A�  A���A��A���A��A��mA�$�A���A�`BA�JA��^A�M�A���A��`A���A�I�A���A��wA���A�ffA�=qA��A�A���A��hA�|�A�ffA�Q�A�5?A�=qA�O�A�G�A�E�A�\)A�"�A��+A��\A�  A�-A�bA���A���A�x�A�-A�x�A���A�t�A�I�A�7LA�$�A��yA�5?A��HA�oA�n�A�JA�A7LA~�!A~ZA}K�Az  AxAw�FAwt�Av1'AuO�Atn�Ar^5Aq��Ao��An�+AnM�Am�TAl��Al��Al�DAk��AhĜAhv�AgK�Ad�Ac33AbVAat�AaoA`�9A`bNA_��A^�yA^~�A^bNA^=qA^1'A^bA]�-A]�hA]dZA]oA\��A\E�A[�hA[&�AZ�9AZ(�AYS�AYoAX�\AV�AT�+ASS�AR��AQAO��AN��ANv�ANQ�AN$�AMl�AJ��AI��AIC�AH$�AG33AE�;AE��AEO�ACƨACoA@��A>�A>A:z�A9�A8VA6�uA5��A5%A3��A3A2�A1��A0�A01'A/7LA-&�A+�A*-A)��A)t�A)hsA)dZA)`BA)\)A)C�A(�yA(1'A'p�A&��A&r�A&{A%&�A#�A"�jA!;dA (�A"�Az�A�A33A��AĜAn�A  AoA\)A��A��Ax�A?}A��A�A��A�yA�!AQ�A�TA��At�AG�A&�AA�!AI�A
��A	S�AA�A�A�wA�-A�A�hAVA�A�hA��A��A �D@���@��F@�-@�G�@�A�@�+@��+@��-@���@�b@�@�v�@�&�@�F@��H@��@��@�{@��@��@�E�@��m@㝲@�@�n�@�o@އ+@�?}@۶F@�O�@�1@׾w@�v�@��
@ёh@�{@̴9@�33@�=q@�O�@ȴ9@�I�@� �@��m@ǝ�@�C�@��@��`@�K�@�/@�K�@�^5@��@���@���@�r�@�(�@��w@��P@�K�@�=q@���@���@��@�@��+@�/@��;@���@��H@�M�@���@��P@�?}@���@���@��`@�Ĝ@��9@��9@���@�r�@��@�=q@��j@�ƨ@�K�@��@��!@�@���@���@�5?@�p�@�dZ@���@�=q@�J@���@�`B@��@��j@�9X@��@���@�hs@�O�@��@���@��j@���@��u@�r�@�(�@��
@��@�l�@�\)@�S�@�"�@��H@��+@�J@��-@�O�@��j@��@�^5@��@��u@��m@�\)@��H@��@��@�(�@���@��@�|�@�l�@�l�@�l�@�dZ@�dZ@�\)@�\)@�S�@�S�@�S�@�S�@�S�@�K�@�C�@�33@�+@��@�
=@���@���@��@��y@��H@�ȴ@��!@�E�@�bN@���@��P@�\)@��H@�ff@���@��-@��7@�p�@�X@�G�@�G�@�?}@�/@�&�@�&�@��@��@�%@��/@��u@�b@���@���@��@�\)@���@��-@�&�@�V@��`@�Ĝ@���@��@�r�@�j@�Z@�(�@�  @~�@}��@}O�@}/@|��@|�@{�m@{�
@{ƨ@{��@{o@z^5@y��@y�#@y��@y�@xĜ@x�u@x�@xbN@x �@x �@xb@x �@x �@x �@x �@x �@w�@w��@w�@w�P@w\)@wl�@wK�@wK�@w;d@w�@w+@v��@v�@v�@v�@vȴ@vȴ@vȴ@v�@v�@v�@v�@v�@vff@m�T@g�@^��@Vv�@O\)@Fȴ@@ �@:J@7�@.ff@*M�@$(�@�+@�^@
=@o@?}@Ĝ@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBXBW
BW
BW
BW
BW
BW
BVBT�BS�BQ�BQ�BQ�BR�BS�BR�BR�BR�BR�BQ�BR�BS�BR�BR�BQ�BP�BN�BG�B1'B�BPBB��B�B�ZB��B��B�dB��B�VB�B~�Bz�Bu�Bs�Br�Bp�Bl�BgmBffBe`BdZBaHBdZBhsBhsBl�Bq�Bk�BYB8RB�fB�3B��Bx�B^5B?}B{B
�ZB
�
B
��B
��B
��B
ɺB
ŢB
�jB
�B
��B
�{B
�PB
�7B
�B
� B
z�B
o�B
S�B
C�B
?}B
<jB
2-B
)�B
!�B
uB
JB
B	��B	��B	��B	�B	�B	�B	�ZB	��B	��B	ŢB	�LB	�-B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�bB	�VB	�7B	{�B	s�B	m�B	iyB	`BB	YB	VB	S�B	R�B	O�B	J�B	?}B	:^B	6FB	1'B	,B	%�B	$�B	 �B	�B	�B		7B	B��B�B�B�sB�NB�;B�#B�B��B��B��BɺBŢB��B�RB�-B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B�{B�VB�=B�+B�B�B}�Bt�Bp�Bo�Bl�BhsBcTB_;B^5B]/B\)BW
BP�BN�BN�BM�BL�BK�BJ�BJ�BI�BH�BG�BF�BC�BA�B>wB=qB=qB=qB=qB=qB;dB7LB5?B33B1'B/B-B+B)�B'�B&�B%�B$�B$�B#�B#�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B#�B#�B#�B$�B%�B%�B&�B&�B&�B(�B)�B,B.B/B/B2-B6FB7LB8RB:^B;dB@�BG�BH�BH�BH�BI�BI�BI�BI�BJ�BJ�BO�BS�BXBYBYBZB\)B_;BffBiyBjBs�Bv�By�By�B{�B|�B}�B~�B� B�%B�VB�bB�hB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�RB��BÖBǮB��B��B��B�)B�TB�sB�yB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B	B	
=B	PB	\B	{B	�B	�B	�B	!�B	"�B	#�B	$�B	$�B	%�B	%�B	&�B	&�B	&�B	'�B	'�B	(�B	,B	2-B	2-B	6FB	7LB	8RB	=qB	H�B	M�B	N�B	P�B	Q�B	R�B	S�B	T�B	T�B	VB	XB	YB	^5B	e`B	gmB	hsB	k�B	n�B	o�B	o�B	o�B	p�B	s�B	w�B	y�B	z�B	{�B	� B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�+B	�+B	�+B	�+B	�1B	�7B	�=B	�DB	�JB	�JB	�PB	�PB	�PB	�\B	�VB	�bB	�hB	�hB	�hB	�oB	�oB	�oB	�oB	�oB	�oB	�oB	�oB	��B	�jB	��B	�sB	��B

=B
�B
$�B
-B
2-B
>wB
C�B
K�B
R�B
YB
\)B
bNB
iyB
n�B
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BW�BV�BV�BV�BV�BV�BV�BU�BT�BS�BQ�BQ�BQ�BR�BS�BR�BR�BR�BR�BQ�BR�BS�BR�BR�BQ�BP�BN�BG�B1B�B8B �B��B�B�>B��BͶB�HB��B�9B��B~�Bz�Bu�Bs�Br�Bp�BlnBgQBfHBeCBd<Ba(Bd<BhTBhTBlmBq�BkiBX�B85B�FB�B��Bx�B^B?cB_B
�@B
��B
��B
͹B
̱B
ɝB
ňB
�LB
��B
��B
�_B
�6B
�B
�B
�B
z�B
o�B
S�B
C~B
?aB
<RB
2B
)�B
!�B
]B
4B
�B	��B	��B	��B	�B	�B	�tB	�BB	��B	��B	ŋB	�5B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�vB	�hB	�OB	�AB	�"B	{�B	s�B	m~B	ieB	`.B	YB	U�B	S�B	R�B	O�B	J�B	?kB	:KB	63B	1B	+�B	%�B	$�B	 �B	�B	pB		(B	�B��B�B�B�cB�?B�-B�B��B��B��B̼BɪBŔB�sB�CB� B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�nB�IB�2B�B�B��B}�Bt�Bp�Bo�BlBhfBcHB_-B^,B]!B\BW BP�BN�BN�BM�BL�BK�BJ�BJ�BI�BH�BG�BF�BC�BA}B>mB=fB=hB=JB=hB=iB;YB7AB55B3*B1B/B-B*�B)�B'�B&�B%�B$�B$�B#�B#�B"�B!�B �B�B�B�B�B�B�B�B�B|B�B{B�BoB�BzB�ByBBB�B�B�B�BcB{B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B#�B#�B#�B$�B%�B%�B&�B&�B&�B(�B)�B+�B.B/B/B2B67B7>B8DB:PB;SB@rBG�BH�BH�BH�BI�BI�BI�BI�BJ�BJ�BO�BS�BW�BYBYBZB\B_*BfRBigBjoBs�Bv�By�By�B{�B|�B}�B~�B�B�B�AB�PB�WB�[B�`B�gB�mB�mB�sB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�<B�lBÂBǗBʫBͻB��B�B�=B�ZB�`B�aB�bB�`B�gB�iB�gB�gB�iB�nB�lB�kB�nB�nB�lB�mB�qB�sB�zB�yB�~B�B�B�B�B�B�B�B	�B	
#B	5B	BB	aB	xB	�B	�B	!�B	"�B	#�B	$�B	$�B	%�B	%�B	&�B	&�B	&�B	'�B	'�B	(�B	+�B	2B	2B	6+B	70B	85B	=VB	H�B	M�B	N�B	P�B	Q�B	R�B	S�B	T�B	T�B	U�B	W�B	X�B	^B	eEB	gOB	hUB	khB	n}B	o�B	o�B	o�B	p�B	s�B	w�B	y�B	z�B	{�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�&B	�+B	�+B	�1B	�4B	�1B	�=B	�6B	�DB	�JB	�JB	�GB	�OB	�OB	�OB	�RB	�PB	�PB	�SB	�RB	�cB	�KB	��B	�SB	��B

B
iB
$�B
,�B
2B
>VB
CvB
K�B
R�B
X�B
\B
b+B
iWB
nuB
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.3 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708242016053117082420160531170824  AO  ARCAADJP                                                                    20150327192317    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150327192317  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150327192317  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170824  IP                  G�O�G�O�G�O�                