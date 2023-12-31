CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:43Z AOML 3.0 creation; 2016-06-01T00:08:17Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230843  20160531170817  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               JA   AO  4055_7112_074                   2C  D   APEX                            5374                            041511                          846 @��o��1   @��p}� @:[��S���dz�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    JA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�3D�FfD�� D��3D� D�Y�D��fD�� D�3D�C3D��3D�ٚD�	�D�@ Dڜ�D�� D�3D�I�D�fD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @ə�A��A$��AD��Ad��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B	33B33B33B!33B)33B133B933BA33BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B���B���B���C L�CL�CL�CL�CL�C
L�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2fgC4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�Cp33CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fD 3D �3D3D�3D3D��D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Dt�fDy� D��D�P D���D���D��D�c4D�� D�ٚD��D�L�D���D��4D�4D�I�DڦgD�ٚD��D�S4D� D�#4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�  A�  A�A�A�A�%A�%A�1A�1A�
=A�
=A�
=A�
=A�
=A���A��TA��hA���A�bA�t�A���A���A���A��PA�hsA���A���A�%A�jA�5?A�5?A�+A�"�A��A�JA���A��A��yA��`A��HA��HA��;A��;A��/A��#A��A��
A���A���A���A��wA���A�x�A��A��PA�A� �A��RA�x�A�O�A��yA�S�A�7LA���A�+A��mA�(�A���A��PA���A�O�A�33A�=qA�O�A��PA�r�A�n�A��A�bA�hA~�A~z�A}�
Az�Ay
=AwG�AvA�Au��At��AtE�ArĜAp�Ao`BAnbAl~�AlJAkhsAk�Aj�!AjffAj1Ai/Ah~�Af��AfM�Ad��Ac��Ab��AaA`jA^�`A^I�A]��A\��A\bA[
=AZ�AYx�AXv�AT��ARv�AQXAPz�AP�AO�hAN�/ANQ�AM�FAM33AL�/AL�DALM�AKdZAJ�!AJ$�AI�PAHr�AFbNADA@��A?hsA?VA>�A>z�A>VA=�#A=�FA=��A=�PA=;dA<-A8bNA7VA6ȴA5�A5?}A4��A3��A2z�A0�\A.9XA-�7A,9XA+\)A*�HA*��A*~�A*A�A*-A*JA)�wA)l�A)%A(v�A(Q�A'�#A'A'�hA'|�A'dZA'S�A'/A&��A%��A%A%�-A%�A%?}A$A�A#dZA"�yA"��A!��A!l�A ĜA �uA VA�#A�A5?AA�^Ax�A��A�mAO�A�\A%A��A��A�AXAdZA
=A�An�A�A`BA	��A	C�A�/Az�A$�A�AAz�A33A�uAn�A1'A��A��AG�A�A ~�A {@�"�@��h@���@��!@��D@��!@�b@��@�!@�7@�@�&�@��@�-@噚@�Ĝ@� �@�@�33@�o@�ȴ@���@�j@���@�J@�  @؛�@��@ԋD@�7L@�V@�$�@�@�9X@ɡ�@��`@�I�@��;@�ȴ@�5?@Ų-@�p�@ě�@�9X@Ý�@�C�@��@+@�-@���@��@��9@�z�@�ƨ@�l�@���@�t�@���@�`B@�G�@�/@���@�1'@���@�K�@�+@�"�@�"�@��@��H@���@�V@���@�%@�z�@�1'@���@�
=@���@�5?@���@�hs@��@�j@��@��w@��@�M�@�-@�{@���@��@��m@��@���@��@�7L@��@���@�K�@��@�ff@�{@�`B@��u@�A�@�1@���@�K�@��@��\@�x�@� �@���@��;@��P@�33@���@�n�@�hs@� �@��H@�$�@���@��@�X@�7L@�&�@��@���@���@�V@��7@�?}@�%@��@��@�r�@� �@�  @��@��
@�l�@�33@�@���@��!@�E�@�@��@�Z@��@��R@���@���@��u@�I�@� �@���@�"�@��y@��R@�^5@��@���@��7@�?}@���@���@���@�z�@�Z@��@��F@��y@���@�n�@�M�@�-@���@���@���@�?}@��j@��u@��D@��u@��@�r�@�r�@�r�@�bN@�Q�@�A�@�9X@�(�@�b@�b@�1@���@��
@���@�dZ@�C�@�33@��@�
=@�@���@��@��H@���@�v�@��@���@�?}@�Ĝ@���@�A�@�P@~ȴ@~$�@~E�@~ff@~E�@~{@}��@}��@}�h@}p�@|��@|(�@{��@{"�@z��@y��@y�#@y��@y��@y��@y�^@y%@x  @w|�@w;d@w;d@w+@v�y@vȴ@v�+@v�+@v�+@vv�@sC�@j^5@b�!@W|�@P��@J^5@D��@?;d@9��@5?}@/��@,�D@(�9@ Ĝ@~�@�P@�m@��@	�@��@�!111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�  A�  A�A�A�A�%A�%A�1A�1A�
=A�
=A�
=A�
=A�
=A���A��TA��hA���A�bA�t�A���A���A���A��PA�hsA���A���A�%A�jA�5?A�5?A�+A�"�A��A�JA���A��A��yA��`A��HA��HA��;A��;A��/A��#A��A��
A���A���A���A��wA���A�x�A��A��PA�A� �A��RA�x�A�O�A��yA�S�A�7LA���A�+A��mA�(�A���A��PA���A�O�A�33A�=qA�O�A��PA�r�A�n�A��A�bA�hA~�A~z�A}�
Az�Ay
=AwG�AvA�Au��At��AtE�ArĜAp�Ao`BAnbAl~�AlJAkhsAk�Aj�!AjffAj1Ai/Ah~�Af��AfM�Ad��Ac��Ab��AaA`jA^�`A^I�A]��A\��A\bA[
=AZ�AYx�AXv�AT��ARv�AQXAPz�AP�AO�hAN�/ANQ�AM�FAM33AL�/AL�DALM�AKdZAJ�!AJ$�AI�PAHr�AFbNADA@��A?hsA?VA>�A>z�A>VA=�#A=�FA=��A=�PA=;dA<-A8bNA7VA6ȴA5�A5?}A4��A3��A2z�A0�\A.9XA-�7A,9XA+\)A*�HA*��A*~�A*A�A*-A*JA)�wA)l�A)%A(v�A(Q�A'�#A'A'�hA'|�A'dZA'S�A'/A&��A%��A%A%�-A%�A%?}A$A�A#dZA"�yA"��A!��A!l�A ĜA �uA VA�#A�A5?AA�^Ax�A��A�mAO�A�\A%A��A��A�AXAdZA
=A�An�A�A`BA	��A	C�A�/Az�A$�A�AAz�A33A�uAn�A1'A��A��AG�A�A ~�A {@�"�@��h@���@��!@��D@��!@�b@��@�!@�7@�@�&�@��@�-@噚@�Ĝ@� �@�@�33@�o@�ȴ@���@�j@���@�J@�  @؛�@��@ԋD@�7L@�V@�$�@�@�9X@ɡ�@��`@�I�@��;@�ȴ@�5?@Ų-@�p�@ě�@�9X@Ý�@�C�@��@+@�-@���@��@��9@�z�@�ƨ@�l�@���@�t�@���@�`B@�G�@�/@���@�1'@���@�K�@�+@�"�@�"�@��@��H@���@�V@���@�%@�z�@�1'@���@�
=@���@�5?@���@�hs@��@�j@��@��w@��@�M�@�-@�{@���@��@��m@��@���@��@�7L@��@���@�K�@��@�ff@�{@�`B@��u@�A�@�1@���@�K�@��@��\@�x�@� �@���@��;@��P@�33@���@�n�@�hs@� �@��H@�$�@���@��@�X@�7L@�&�@��@���@���@�V@��7@�?}@�%@��@��@�r�@� �@�  @��@��
@�l�@�33@�@���@��!@�E�@�@��@�Z@��@��R@���@���@��u@�I�@� �@���@�"�@��y@��R@�^5@��@���@��7@�?}@���@���@���@�z�@�Z@��@��F@��y@���@�n�@�M�@�-@���@���@���@�?}@��j@��u@��D@��u@��@�r�@�r�@�r�@�bN@�Q�@�A�@�9X@�(�@�b@�b@�1@���@��
@���@�dZ@�C�@�33@��@�
=@�@���@��@��H@���@�v�@��@���@�?}@�Ĝ@���@�A�@�P@~ȴ@~$�@~E�@~ff@~E�@~{@}��@}��@}�h@}p�@|��@|(�@{��@{"�@z��@y��@y�#@y��@y��@y��@y�^@y%@x  @w|�@w;d@w;d@w+@v�y@vȴ@v�+@v�+@v�+@vv�@sC�@j^5@b�!@W|�@P��@J^5@D��@?;d@9��@5?}@/��@,�D@(�9@ Ĝ@~�@�P@�m@��@	�@��@�!111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�mB�mB�mB�mB�sB�mB�mB�sB�sB�sB�sB�mB�mB�fB�TB�;B��B{�B_;BN�B'�B�BuBoB\BJBbBVBBBBBB  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�ZB��BÖB��B[#B9XB�BVB��B�B��Bm�BO�B=qB�BB
�B
�mB
��B
��B
�B
�PB
x�B
ffB
O�B
C�B
>wB
:^B
5?B
-B
�B
hB
%B	��B	��B	�B	�B	�NB	�
B	��B	��B	ŢB	B	�}B	�qB	�dB	�XB	�FB	�'B	��B	��B	��B	��B	�oB	�VB	�7B	�B	y�B	v�B	r�B	o�B	l�B	iyB	ffB	bNB	ZB	M�B	F�B	B�B	?}B	=qB	;dB	9XB	6FB	49B	33B	1'B	0!B	-B	)�B	&�B	$�B	 �B	�B	�B	JB	+B	B	B	B	B	B	B	B	B	  B��B��B�TB�5B�)B�B�B��B��BɺB��B�jB�XB�?B�3B�-B�-B�'B�'B�'B�!B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�\B�JB�7B�+B�B�B�B|�B{�By�Bx�Bv�Bt�Br�Bo�BjBffBcTB^5BYBS�BQ�BP�BL�BF�BC�B>wB=qB=qB;dB;dB9XB7LB5?B49B2-B2-B1'B.B,B+B+B)�B'�B&�B%�B#�B"�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuB{BoBbBuB�B�B�B�B�B�B�B"�B#�B#�B#�B%�B&�B'�B&�B(�B(�B)�B+B+B,B,B,B.B.B-B.B.B,B2-B49B5?B5?B49B49B5?B7LB9XB:^B:^B:^B:^B:^B;dB:^B:^B:^B;dB;dB<jB=qB>wB?}BA�BA�BB�BC�BE�BE�BG�BJ�BJ�BJ�BJ�BK�BO�BQ�BW
BXBYBYBZB`BBaHBcTBdZBffBjBk�Bl�Bl�Bn�Bp�Bp�Bt�B{�B|�B|�B}�B� B�B�B�1B�\B��B��B��B��B��B��B��B��B��B��B�B�9B�FB�LB�^B�dB�dB�qB�wB�}B�}BBÖBĜBƨBƨBɺB��B��B�B�)B�HB�mB�B�B��B��B��B��B��B	  B	B	B	1B	1B	
=B	JB	VB	hB	uB	{B	�B	�B	�B	!�B	#�B	$�B	%�B	'�B	(�B	+B	.B	33B	5?B	5?B	5?B	5?B	6FB	6FB	6FB	7LB	7LB	8RB	9XB	9XB	:^B	:^B	;dB	;dB	<jB	>wB	A�B	B�B	C�B	D�B	E�B	E�B	F�B	F�B	G�B	H�B	J�B	M�B	O�B	T�B	ZB	[#B	^5B	cTB	ffB	jB	iyB	hsB	iyB	iyB	k�B	l�B	l�B	l�B	o�B	r�B	t�B	v�B	x�B	{�B	|�B	|�B	|�B	|�B	|�B	� B	�B	�+B	�1B	�7B	�7B	�=B	�=B	�JB	�JB	�JB	�PB	��B	�FB	��B	�B
B
bB
�B
"�B
-B
7LB
=qB
A�B
F�B
O�B
XB
\)B
aHB
gmB
o�B
s�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�ZB�ZB�ZB�ZB�\B�ZB�ZB�\B�_B�\B�\B�ZB�]B�QB�@B�*B��B{�B_#BN�B'�BhB\BTBEB1BKB=BB �B�B�B �B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�}B�<B��B�yB�gB[B9:B�B8B��B��B��BmvBO�B=WB�B�B
�B
�SB
��B
�iB
��B
�6B
x�B
fMB
O�B
C}B
>_B
:EB
5)B
,�B
�B
QB
B	��B	��B	�B	�zB	�8B	��B	��B	˲B	ōB	�zB	�hB	�]B	�OB	�EB	�/B	�B	��B	��B	��B	�zB	�]B	�CB	�#B	��B	y�B	v�B	r�B	o�B	luB	ieB	fSB	b:B	ZB	M�B	F�B	B~B	?kB	=`B	;PB	9FB	64B	4'B	3"B	1B	0B	,�B	)�B	&�B	$�B	 �B	�B	pB	8B	B	B		B	
B	B	�B	�B	�B	 �B��B��B��B�FB�'B�B�B� B��B��BɫB�xB�[B�GB�2B�%B� B�B�B�B�B�B�B�B��B� B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�lB�\B�PB�<B�)B�B�B�B��B|�B{�By�Bx�Bv�Bt�Br�Bo�BjvBf\BcGB^*BYBS�BQ�BP�BL�BF�BC�B>kB=iB=JB;XB;[B9LB7AB53B40B2"B2#B1B.B+�B*�B*�B)�B'�B&�B%�B#�B"�B �B�B�BtBwB�BgBdB�BcB�B�B�B�B�B�BaB[BpBjBTBdB>BfBcB`BB�B�B�B�B"�B#�B#�B#�B%�B&�B'�B&�B(�B(�B)�B*�B*�B+�B+�B+�B.B.B-B.B.B+�B2B4,B51B50B4*B4,B52B7>B9JB:PB:PB:PB:PB:OB;UB:NB:NB:MB;TB;VB<[B=bB>hB?oBA{BAzBB�BC�BE�BE�BG�BJ�BJ�BJ�BJ�BK�BO�BQ�BV�BW�BY	BYBZB`2Ba7BcBBdHBfXBjlBkvBlwBlyBn�Bp�Bp�Bt�B{�B|�B|�B}�B�B��B�B� B�JB�lB��B��B��B��B��B��B��B��B��B�B�%B�1B�6B�IB�KB�LB�\B�`B�gB�hB�wB�BćBƒBƐBɣB̷B��B��B�B�.B�UB�vB�B��B��B��B��B��B��B	�B	B	B	B	
"B	1B	<B	MB	[B	cB	nB	xB	�B	!�B	#�B	$�B	%�B	'�B	(�B	*�B	-�B	3B	5$B	5$B	5$B	5#B	6*B	6*B	6*B	7/B	72B	86B	9;B	9;B	:BB	:DB	;IB	;JB	<NB	>ZB	AnB	BrB	CyB	D�B	E�B	E�B	F�B	F�B	G�B	H�B	J�B	M�B	O�B	T�B	ZB	[B	^B	c8B	fKB	jaB	i]B	hWB	i[B	i\B	kkB	lmB	lnB	llB	o�B	r�B	t�B	v�B	x�B	{�B	|�B	|�B	|�B	|�B	|�B	�B	� B	�B	�B	�B	�B	� B	�B	�-B	�-B	�-B	�0B	�{B	�%B	̬B	�hB
 �B
BB
kB
"�B
,�B
7+B
=MB
AfB
F�B
O�B
W�B
\B
a$B
gIB
ozB
s�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.3 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708172016053117081720160531170817  AO  ARCAADJP                                                                    20140721230843    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230843  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230843  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170817  IP                  G�O�G�O�G�O�                