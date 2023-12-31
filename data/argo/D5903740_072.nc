CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:42Z AOML 3.0 creation; 2016-06-01T00:08:17Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230842  20160531170817  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               HA   AO  4055_7112_072                   2C  D   APEX                            5374                            041511                          846 @��K�i�1   @��L"�P@:��n���dl�C��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    HA   A   A   @�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_fD_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy�fD���D�0 D��3D��3D�3D�,�D��3D��fD�3D�I�D���DǶfD�fD�33D�i�D�fD�3D�FfD�s3D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@���A��A$��AD��Ad��A�ffA�ffA�ffA�ffA�ffAљ�A�ffA�ffB33B	33B33B33B!33B)33B133B933BA33BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B왚B�B���B���B���C L�CL�CL�CL�CL�C
L�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�Cf33ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D�D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_�D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�fDy��D�gD�9�D���D���D��D�6gD���D�� D��D�S4D��4D�� D� D�<�D�s4D�� D��D�P D�|�D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��mA��mA��yA��yA��TA��TA��yA��yA��mA��`A��A��A��A��A��A��A��A��yA���A�ȴA���A��HA��mA��A��mA�ȴA�ƨA���A���A�ȴA�ȴA���A��RA��RA��^A��-A��-A��9A��!A���A���A���A���A���A���A��uA��PA��\A��PA��PA��PA��+A�~�A�x�A�n�A�^5A�33A��A��A���A��FA�{A��jA��FA�^5A�&�A�v�A�;dA�JA��#A���A�ƨA�C�A�l�A���A�-A�S�A�?}A�1'A���A��TA��`A�?}A�1'A�?}A���A��wA��PA��#A�
=A��A���A�$�A���A�  A���A���A}��A|  A{hsAz��Azr�AxI�Av�RAv~�Au�TAu;dAs��Aq�TAo\)Al��AiK�Ag��Ag�Af�/Af�\AfI�Af5?Ae�Ad��AdAb��Ab{A`  A^{A[��A[�FA[/AZ^5AYAX��AXM�AVĜAT��AS�AR9XARAQ��AQ��AQp�AQ%AP-AO�AM&�AK&�AH��AGx�AF�jAE%AD �ABffAAhsA?��A>$�A<�RA<{A;;dA:�jA9�A9��A9��A9��A9XA9VA8�9A7C�A6z�A5�mA5�-A5dZA5oA4��A3�#A2��A1��A0�+A/��A/33A.ZA,�`A+�;A+33A*��A*�A*bNA* �A)�A)��A)��A)hsA);dA(��A'��A'XA&r�A&(�A%�^A$��A#�A#XA"��A"5?A!O�A n�Al�A�A��AA�A1AG�A�HAjA{A�PA�AXAA�PA�`AĜA�A�A�#AXA
�HA
M�A	%A�
Ax�At�A+Az�A^5AoA9XA�FA �@�33@�-@�x�@���@�(�@�C�@�E�@�G�@�1'@�33@���@�-@�7L@�@�@�bN@�R@��T@�V@�I�@�\)@�ȴ@�`B@�hs@�bN@��@�dZ@�ȴ@���@��@�+@؛�@�+@ԣ�@�dZ@�"�@��H@ҧ�@�n�@�M�@�5?@ѡ�@�Ĝ@���@��y@��T@�C�@�z�@� �@�K�@�@Õ�@�O�@��@��@��j@��u@�ƨ@�\)@���@�^5@�{@�p�@��u@��y@��@�9X@��H@�$�@��@��^@�?}@�1'@��F@�l�@�ff@��-@��7@�7L@��j@�z�@�1'@��;@��P@��@���@�$�@�@�A�@��@��H@�@�`B@��j@�(�@���@�V@��9@�r�@��P@���@�-@���@���@�|�@��R@�v�@�^5@�-@��T@��7@�hs@��@���@�z�@�1'@���@���@�
=@��\@���@�O�@��9@���@�C�@���@�ff@�{@��@���@��^@��h@�x�@�/@��@��9@�z�@�Q�@�A�@�1@��
@��@��@�"�@�v�@��7@�?}@���@�9X@�|�@���@��@���@�X@��@��@�Ĝ@���@��@�r�@�Z@�A�@�9X@�(�@� �@�1@��@��m@��m@��
@��@��@�|�@�t�@�t�@�l�@�dZ@�\)@�K�@�33@��y@���@�$�@�O�@���@���@��u@��@��F@�t�@�C�@�+@�"�@�@�$�@�O�@�V@��/@���@�I�@�(�@�  @��m@�ƨ@���@�l�@�;d@��@���@���@�~�@�ff@��#@�%@���@���@���@���@���@���@��D@��@�j@;d@}@}�h@}�@}?}@}V@|��@|�@|��@|�j@|�D@|Z@|1@{�F@{t�@{@z^5@y�#@yX@xĜ@x��@x�u@x�@w�w@w��@w�P@wl�@w\)@vV@nff@g\)@_��@W�@O��@K"�@E��@A��@<�@6{@1��@+�
@'\)@$Z@ȴ@��@@@ �@C�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��mA��mA��yA��yA��TA��TA��yA��yA��mA��`A��A��A��A��A��A��A��A��yA���A�ȴA���A��HA��mA��A��mA�ȴA�ƨA���A���A�ȴA�ȴA���A��RA��RA��^A��-A��-A��9A��!A���A���A���A���A���A���A��uA��PA��\A��PA��PA��PA��+A�~�A�x�A�n�A�^5A�33A��A��A���A��FA�{A��jA��FA�^5A�&�A�v�A�;dA�JA��#A���A�ƨA�C�A�l�A���A�-A�S�A�?}A�1'A���A��TA��`A�?}A�1'A�?}A���A��wA��PA��#A�
=A��A���A�$�A���A�  A���A���A}��A|  A{hsAz��Azr�AxI�Av�RAv~�Au�TAu;dAs��Aq�TAo\)Al��AiK�Ag��Ag�Af�/Af�\AfI�Af5?Ae�Ad��AdAb��Ab{A`  A^{A[��A[�FA[/AZ^5AYAX��AXM�AVĜAT��AS�AR9XARAQ��AQ��AQp�AQ%AP-AO�AM&�AK&�AH��AGx�AF�jAE%AD �ABffAAhsA?��A>$�A<�RA<{A;;dA:�jA9�A9��A9��A9��A9XA9VA8�9A7C�A6z�A5�mA5�-A5dZA5oA4��A3�#A2��A1��A0�+A/��A/33A.ZA,�`A+�;A+33A*��A*�A*bNA* �A)�A)��A)��A)hsA);dA(��A'��A'XA&r�A&(�A%�^A$��A#�A#XA"��A"5?A!O�A n�Al�A�A��AA�A1AG�A�HAjA{A�PA�AXAA�PA�`AĜA�A�A�#AXA
�HA
M�A	%A�
Ax�At�A+Az�A^5AoA9XA�FA �@�33@�-@�x�@���@�(�@�C�@�E�@�G�@�1'@�33@���@�-@�7L@�@�@�bN@�R@��T@�V@�I�@�\)@�ȴ@�`B@�hs@�bN@��@�dZ@�ȴ@���@��@�+@؛�@�+@ԣ�@�dZ@�"�@��H@ҧ�@�n�@�M�@�5?@ѡ�@�Ĝ@���@��y@��T@�C�@�z�@� �@�K�@�@Õ�@�O�@��@��@��j@��u@�ƨ@�\)@���@�^5@�{@�p�@��u@��y@��@�9X@��H@�$�@��@��^@�?}@�1'@��F@�l�@�ff@��-@��7@�7L@��j@�z�@�1'@��;@��P@��@���@�$�@�@�A�@��@��H@�@�`B@��j@�(�@���@�V@��9@�r�@��P@���@�-@���@���@�|�@��R@�v�@�^5@�-@��T@��7@�hs@��@���@�z�@�1'@���@���@�
=@��\@���@�O�@��9@���@�C�@���@�ff@�{@��@���@��^@��h@�x�@�/@��@��9@�z�@�Q�@�A�@�1@��
@��@��@�"�@�v�@��7@�?}@���@�9X@�|�@���@��@���@�X@��@��@�Ĝ@���@��@�r�@�Z@�A�@�9X@�(�@� �@�1@��@��m@��m@��
@��@��@�|�@�t�@�t�@�l�@�dZ@�\)@�K�@�33@��y@���@�$�@�O�@���@���@��u@��@��F@�t�@�C�@�+@�"�@�@�$�@�O�@�V@��/@���@�I�@�(�@�  @��m@�ƨ@���@�l�@�;d@��@���@���@�~�@�ff@��#@�%@���@���@���@���@���@���@��D@��@�j@;d@}@}�h@}�@}?}@}V@|��@|�@|��@|�j@|�D@|Z@|1@{�F@{t�@{@z^5@y�#@yX@xĜ@x��@x�u@x�@w�w@w��@w�P@wl�@w\)@vV@nff@g\)@_��@W�@O��@K"�@E��@A��@<�@6{@1��@+�
@'\)@$Z@ȴ@��@@@ �@C�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBW
BVBVBVBT�BT�BVBVBT�BT�BVBVBW
BW
BW
BW
BVBVBT�BS�BT�BVBVBVBT�BS�BS�BS�BS�BS�BS�BR�BQ�BP�BQ�BP�BP�BP�BO�BN�BM�BM�BM�BM�BM�BM�BL�BL�BL�BL�BL�BK�BJ�BI�BH�BF�B@�B7LB�B�BB��B�{B[#B)�B	7B�BȴBB�wB�XB�RB�FB��B��B�Bv�BgmBffBe`B^5BO�B=qB�B
�#B
ɺB
ÖB
�qB
��B
�JB
~�B
n�B
[#B
Q�B
O�B
N�B
L�B
E�B
0!B
"�B
�B
�B
{B
1B	��B	��B	��B	�B	�fB	�B	ɺB	�XB	��B	��B	��B	��B	��B	��B	��B	��B	�bB	�=B	�B	~�B	u�B	l�B	cTB	bNB	_;B	\)B	XB	S�B	P�B	I�B	B�B	;dB	9XB	8RB	7LB	6FB	49B	1'B	-B	'�B	 �B	�B	\B		7B	%B��B��B�B�B�`B�BB�)B�B�B�B��B�
B�B�B�
B��B��B��BȴBĜBÖB��B��B��B��B�}B�dB�RB�FB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�PB�DB�7B�+B�B�B}�By�Bv�Bt�Br�Bq�Bo�Bn�Bl�BjBgmBaHBT�BJ�BG�BF�BE�BD�BD�BB�BA�B@�B=qB;dB9XB9XB8RB6FB33B/B.B,B+B(�B'�B&�B&�B%�B$�B#�B"�B"�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B!�B!�B!�B#�B(�B(�B(�B(�B(�B)�B)�B,B,B,B,B,B/B0!B33B6FB8RB8RB8RB9XB<jB<jB=qB?}BA�BB�BB�BD�BD�BE�BF�BG�BH�BI�BJ�BI�BO�BS�BS�BVBYB[#B]/BbNBiyBjBk�Bn�Br�Bt�By�Bz�B�B�B�%B�+B�1B�7B�DB�JB�PB�\B�bB�oB�uB��B��B��B��B��B��B��B�B�-B�9B�FB�LB�RB�RB�XB�^B�jB�qB�}B��B��BBÖBĜBŢBƨBȴB��B��B��B�
B�#B�HB�mB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	  B	  B	B	B	B	B	B	B	B	+B	DB	uB	�B	�B	�B	�B	�B	!�B	#�B	$�B	$�B	$�B	+B	2-B	49B	5?B	6FB	:^B	:^B	<jB	=qB	>wB	?}B	A�B	B�B	D�B	E�B	F�B	H�B	H�B	L�B	S�B	W
B	W
B	W
B	W
B	W
B	YB	ZB	ZB	ZB	bNB	jB	k�B	k�B	l�B	m�B	m�B	m�B	n�B	n�B	o�B	p�B	q�B	s�B	t�B	u�B	x�B	{�B	}�B	� B	�B	�B	�B	�%B	�+B	�+B	�+B	�1B	�JB	�B	ŢB	�#B	��B
B
bB
�B
"�B
(�B
2-B
8RB
A�B
H�B
K�B
Q�B
YB
`BB
gmB
o�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BV�BU�BU�BU�BT�BT�BU�BU�BT�BT�BU�BU�BV�BV�BV�BV�BU�BU�BT�BS�BT�BU�BU�BU�BT�BS�BS�BS�BS�BS�BS�BR�BQ�BP�BQ�BP�BP�BP�BO�BN�BM�BM�BM�BM�BM�BM�BL�BL�BL�BL�BL�BK�BJ�BI�BH�BF�B@iB75B�B�'B��B�cB[B)�B	B�fBȘB�tB�\B�<B�5B�(B��B�eB��Bv�BgQBfFBeCB^BO�B=UB�B
�B
ɟB
�}B
�XB
��B
�0B
~�B
n~B
[B
Q�B
O�B
N�B
L�B
E�B
0	B
"�B
�B
�B
aB
B	��B	��B	��B	�B	�PB	�B	ɤB	�BB	��B	��B	��B	��B	��B	��B	�uB	�mB	�OB	�,B	�B	~�B	u�B	lxB	cAB	b9B	_+B	\B	W�B	S�B	P�B	I�B	B}B	;SB	9FB	8BB	7:B	63B	4(B	1B	,�B	'�B	 �B	�B	LB		'B	B��B��B�B�vB�PB�4B�B�B�B�B��B��B�B�B��B��B��B��BȦBĎBÉB�}B�yB�sB�xB�pB�TB�CB�9B�&B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�xB�lB�aB�VB�DB�6B�+B�B�B��B}�By�Bv�Bt�Br�Bq�Bo�Bn�Bl~BjsBgbBa=BT�BJ�BG�BF�BE�BD�BD�BB�BA~B@xB=hB;YB9MB9NB8HB6<B3+B/B.	B+�B*�B(�B'�B&�B&�B%�B$�B#�B"�B"�B!�B �B �B�B�B�B�B�B�B�B|BtB�B�BuB�B[B\BaBYBfB�BaB�B�B�B�ByB�B~B�BB�B�BBB�BB�B!�B!�B!�B!�B#�B(�B(�B(�B(�B(�B)�B)�B+�B+�B+�B+�B+�B/B0B3&B67B8FB8DB8AB9HB<\B<[B=bB?kBAxBBBBBD�BD�BE�BF�BG�BH�BI�BJ�BI�BO�BS�BS�BU�BYB[B]Bb<BihBjlBkrBn�Br�Bt�By�Bz�B��B�
B�B�B�B�#B�3B�7B�<B�IB�NB�]B�`B�nB�B��B��B��B��B��B��B�B�%B�1B�7B�=B�=B�AB�IB�SB�ZB�eB�kB�sB�zBÀBąBŊBƑBȝB̸B��B��B��B�B�0B�SB�xB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	 �B	 �B	 �B	 �B	�B	�B	B	B	)B	YB	iB	kB	wB	�B	�B	!�B	#�B	$�B	$�B	$�B	*�B	2B	4 B	5#B	6,B	:CB	:DB	<PB	=TB	>[B	?cB	ApB	BsB	DB	E�B	F�B	H�B	H�B	L�B	S�B	V�B	V�B	V�B	V�B	V�B	X�B	Y�B	ZB	ZB	b0B	jaB	khB	khB	lnB	mrB	msB	msB	n|B	nzB	o�B	p�B	q�B	s�B	t�B	u�B	x�B	{�B	}�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�.B	��B	ńB	�B	��B
�B
@B
}B
"�B
(�B
2B
81B
AfB
H�B
K�B
Q�B
X�B
`B
gJB
o{B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.3 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708172016053117081720160531170817  AO  ARCAADJP                                                                    20140721230842    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230842  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230842  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170817  IP                  G�O�G�O�G�O�                