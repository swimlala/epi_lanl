CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-13T03:15:39Z AOML 3.0 creation; 2016-08-07T21:51:26Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160213031539  20160807145126  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               hA   AO  5287_9017_104                   2C  D   APEX                            6529                            072314                          846 @ו<�7}S1   @ו=UUt@1��l�C��dӥ�S��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    hA   B   B   @�33@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C�fC  C  C�fC�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtL�Dy� D�  D�` D�� D���D���D�FfD�i�D��fD�	�D�I�D���D��fD� D�0 Dڐ D�� D�3D�VfD��D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@ə�A��A#33AD��Ad��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B	33B33B33B!33B)33B133B933BA33BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�Bؙ�B���B���B䙚B虚B왚B�B���B���B���C L�CL�CL�CfgCL�C
L�CL�C33CL�CL�C33C33CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8fgC:fgC<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt` Dy�3D�	�D�i�D���D��gD��gD�P D�s4D�� D�4D�S4D��4D�� D��D�9�Dڙ�D�ɚD��D�` D�gD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�Aͺ^A͝�A͑hAͅA�~�A�|�A�|�A�|�A�z�A�|�A�v�A�r�A�p�A�n�A�p�A�r�A�v�A�v�A�r�A�r�A�r�A�r�A�t�A�r�A�r�A�t�A�r�A�t�A�r�A�t�A�v�A�z�A�|�A�~�A͇+A͕�A͙�A͝�Aͣ�Aͥ�AͲ-AͶFAͼjA���AͼjAͼjAͺ^AͼjAͼjA͸RAͶFAͶFAͶFAͰ!AͰ!Aʹ9AͲ-AͰ!A͡�A͏\A͏\A͇+A�dZA���A��TA�+Aʴ9Aʗ�A��A�S�AüjA�&�A�dZA���A��FA�O�A�K�A�XA�33A��hA�Q�A��A�1A���A��#A��7A���A��A�9XA�t�A�ĜA�33A�S�A��A���A�ffA�z�A��A��!A���A���A���A�ZA�5?A��A�~�A��-A��A��A�1A�  A��;A��yA�$�A�=qA�ZA�A�A��uAoAz=qAw%At=qAqG�Al-Ae��Ad�DAa�#A]�7A[�wA[33AZ��AW;dAT��AT�uATVAS;dARv�AQ�7AP��AP-AO�AL�RAIt�ADA@��A=�
A:=qA6z�A4jA3/A1�A0M�A0�\A0 �A/��A/S�A-�A, �A*��A*A�A*�A)A'�^A%��A$�9A$5?A#��A#�A#dZA#C�A#�A"��A"A�A!/A M�A�yA?}AI�AA�
A�wA�wA�-Ap�A=qA�AVA`BA�#A��A�;AA�-A��A�7AhsAA~�A��A��AQ�AA�-A�/A�AdZA�`A�PA|�A`BAA|�A�7A�PA�A|�A|�A`BA�A��Ar�AjAVA  A��A�7A`BAn�A��A�A��A�RA�DAVA9XA �A��A�A
�jA
5?A	�TA	A	��A	7LA��A�A  A��Ax�A��AffAn�A�7@��m@��u@�1@�"�@��h@�t�@���@�r�@�P@�V@��@�Z@@�+@�7@��@�v�@��@�@��y@���@�`B@�u@�@�\)@�33@�R@�n�@旍@�@�1'@���@畁@�-@��@�@ް!@�+@��@�  @�ƨ@�ƨ@ׅ@�C�@֟�@�V@�J@թ�@�?}@Լj@��@��;@�S�@�-@с@�Ĝ@�S�@��y@��/@�dZ@�
=@�v�@���@�ȴ@��;@���@�&�@�hs@�%@̋D@̋D@�A�@�ƨ@˥�@˝�@˝�@�l�@�;d@�@ʰ!@�E�@ɺ^@��@ȣ�@ȣ�@�(�@ư!@��T@ũ�@š�@�@�ff@Ɵ�@Ɨ�@Ɵ�@�^5@Ł@�ƨ@�"�@��@�@�ff@�-@��#@�hs@���@��u@�j@��@��@�o@��\@���@�hs@�&�@���@�1'@��@��@�"�@���@��@��@��-@�O�@� �@��w@��@��@�S�@��y@��R@���@�M�@���@��^@���@�%@��`@�I�@���@�C�@��H@�ff@��@�&�@�7L@���@��@�"�@�+@�;d@�+@���@��R@��+@�V@�-@��#@��h@�p�@�O�@�/@�&�@�V@��@��j@��D@�Q�@�1'@�b@��@�33@�5?@��#@�p�@��@�Ĝ@�A�@���@��P@�
=@�~�@�E�@�{@��@���@�V@��9@�j@�9X@� �@�1@��
@�"�@���@�@��#@���@�X@�&�@���@��/@��u@�1'@�b@��m@��w@�K�@�@���@��R@���@�~�@��@�x�@�V@���@��9@��D@�b@���@���@��P@��P@��@�\)@���@�~�@�=q@�@��#@��@�&�@��\@��/@��-@��-@}��@u/@kƨ@e�@\z�@U��@L9X@C�@;�@1�^@+��@(�u@%/@ �u@�m@ȴ@�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�Aͺ^A͝�A͑hAͅA�~�A�|�A�|�A�|�A�z�A�|�A�v�A�r�A�p�A�n�A�p�A�r�A�v�A�v�A�r�A�r�A�r�A�r�A�t�A�r�A�r�A�t�A�r�A�t�A�r�A�t�A�v�A�z�A�|�A�~�A͇+A͕�A͙�A͝�Aͣ�Aͥ�AͲ-AͶFAͼjA���AͼjAͼjAͺ^AͼjAͼjA͸RAͶFAͶFAͶFAͰ!AͰ!Aʹ9AͲ-AͰ!A͡�A͏\A͏\A͇+A�dZA���A��TA�+Aʴ9Aʗ�A��A�S�AüjA�&�A�dZA���A��FA�O�A�K�A�XA�33A��hA�Q�A��A�1A���A��#A��7A���A��A�9XA�t�A�ĜA�33A�S�A��A���A�ffA�z�A��A��!A���A���A���A�ZA�5?A��A�~�A��-A��A��A�1A�  A��;A��yA�$�A�=qA�ZA�A�A��uAoAz=qAw%At=qAqG�Al-Ae��Ad�DAa�#A]�7A[�wA[33AZ��AW;dAT��AT�uATVAS;dARv�AQ�7AP��AP-AO�AL�RAIt�ADA@��A=�
A:=qA6z�A4jA3/A1�A0M�A0�\A0 �A/��A/S�A-�A, �A*��A*A�A*�A)A'�^A%��A$�9A$5?A#��A#�A#dZA#C�A#�A"��A"A�A!/A M�A�yA?}AI�AA�
A�wA�wA�-Ap�A=qA�AVA`BA�#A��A�;AA�-A��A�7AhsAA~�A��A��AQ�AA�-A�/A�AdZA�`A�PA|�A`BAA|�A�7A�PA�A|�A|�A`BA�A��Ar�AjAVA  A��A�7A`BAn�A��A�A��A�RA�DAVA9XA �A��A�A
�jA
5?A	�TA	A	��A	7LA��A�A  A��Ax�A��AffAn�A�7@��m@��u@�1@�"�@��h@�t�@���@�r�@�P@�V@��@�Z@@�+@�7@��@�v�@��@�@��y@���@�`B@�u@�@�\)@�33@�R@�n�@旍@�@�1'@���@畁@�-@��@�@ް!@�+@��@�  @�ƨ@�ƨ@ׅ@�C�@֟�@�V@�J@թ�@�?}@Լj@��@��;@�S�@�-@с@�Ĝ@�S�@��y@��/@�dZ@�
=@�v�@���@�ȴ@��;@���@�&�@�hs@�%@̋D@̋D@�A�@�ƨ@˥�@˝�@˝�@�l�@�;d@�@ʰ!@�E�@ɺ^@��@ȣ�@ȣ�@�(�@ư!@��T@ũ�@š�@�@�ff@Ɵ�@Ɨ�@Ɵ�@�^5@Ł@�ƨ@�"�@��@�@�ff@�-@��#@�hs@���@��u@�j@��@��@�o@��\@���@�hs@�&�@���@�1'@��@��@�"�@���@��@��@��-@�O�@� �@��w@��@��@�S�@��y@��R@���@�M�@���@��^@���@�%@��`@�I�@���@�C�@��H@�ff@��@�&�@�7L@���@��@�"�@�+@�;d@�+@���@��R@��+@�V@�-@��#@��h@�p�@�O�@�/@�&�@�V@��@��j@��D@�Q�@�1'@�b@��@�33@�5?@��#@�p�@��@�Ĝ@�A�@���@��P@�
=@�~�@�E�@�{@��@���@�V@��9@�j@�9X@� �@�1@��
@�"�@���@�@��#@���@�X@�&�@���@��/@��u@�1'@�b@��m@��w@�K�@�@���@��R@���@�~�@��@�x�@�V@���@��9@��D@�b@���@���@��P@��P@��@�\)@���@�~�@�=q@�@��#@��G�O�@��\@��/@��-@��-@}��@u/@kƨ@e�@\z�@U��@L9X@C�@;�@1�^@+��@(�u@%/@ �u@�m@ȴ@�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
}�B
}�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
{�B
|�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
� B
�B
�B
�B
�1B
�1B
�=B
�DB
�DB
�\B
�hB
�oB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�uB
�uB
�uB
�oB
�oB
�{B
��B
��B
�B
��B
ǮB
�B
��Bm�B�RB��B��B�B�/BE�B^5Bu�B��B��B��B��B�B�'B�9B�XB�dB�XB�LB�LB�FB�9B�!B�B��B��B�B��B��B��B�1Bu�B`BBG�B,B\B��B�ZB��B��B��B�PBx�B]/BO�B,B�B
�B
�XB
�PB
VB
 �B
1B	��B	�B	��B	��B	B	�3B	��B	n�B	ffB	YB	D�B	?}B	<jB	5?B	%�B	�B	�B	�B	uB	VB	
=B	%B	B��B�B�fB�
B��B��B��B��B��B�B�sB�B	PB	 �B	(�B	+B	6FB	:^B	;dB	A�B	E�B	H�B	M�B	F�B	M�B	]/B	aHB	e`B	gmB	hsB	iyB	m�B	p�B	v�B	z�B	�B	�=B	�hB	�{B	��B	��B	��B	��B	��B	��B	�9B	ɺB	��B	�HB	�B	�B	��B	��B
B
B
B
  B
B
B
	7B

=B

=B
+B
B	��B	��B	��B	�B	�B	��B
  B
VB
bB
hB
bB
hB
�B
�B
�B
(�B
)�B
+B
)�B
+B
)�B
(�B
'�B
'�B
&�B
%�B
&�B
&�B
&�B
&�B
&�B
%�B
$�B
"�B
#�B
#�B
#�B
"�B
"�B
�B
�B
�B
�B
hB
VB
B	��B	�`B	�;B	��B	ŢB	ÖB	�}B	�RB	�-B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	ȴB	ȴB	ǮB	��B	�B	�
B	�B	�B	�B	�#B	�NB	�fB	�sB	�B	��B
B	��B	�B	�mB	�TB	�
B	��B	ǮB	ƨB	ŢB	ŢB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	ǮB	ƨB	ƨB	ŢB	ĜB	��B	��B	��B	�qB	�dB	�dB	�XB	�dB	�jB	�qB	ÖB	��B	��B	�
B	�B	�)B	�/B	�/B	�BB	�TB	�ZB	�`B	�`B	�`B	�`B	�ZB	�ZB	�TB	�NB	�HB	�NB	�TB	�HB	�/B	�)B	�/B	�/B	�;B	�fB	�B	�B	�B	�B	�B	�yB	�sB	�mB	�fB	�`B	�ZB	�TB	�HB	�BB	�BB	�NB	�fB	�`B	�ZB	�NB	�5B	�/B	�/B	�;B	�5B	�;B	�#B	�B	�B	�B	�B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�;B	�;B	�;B	�BB	�;B	�;B	�BB	�HB	�`B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
VB
{B
�B
!�B
$�B
(�B
/B
49B
>wB
D�B
H�B
L�B
Q�B
[#B
bNB
e`B
iyB
l�B
p�B
t�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
}�B
}�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
{�B
|�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
�B
��B
��B
��B
�B
�B
�(B
�.B
�.B
�IB
�TB
�XB
�hB
�hB
�fB
�hB
�iB
�jB
�hB
�_B
�bB
�bB
�]B
�^B
�iB
�uB
��B
��B
�tB
ǚB
��B
��BmyB�:B˯B��B��B�BE�B^Bu�B�fB��B��B��B��B�B�B�@B�MB�?B�6B�2B�-B� B�	B��B��B��B��B��B��B��B�Bu�B`(BG�B+�B>B��B�?B̴B��B�|B�4Bx�B]BO�B+�BtB
�B
�>B
�7B
U�B
 �B
B	��B	�B	��B	̹B	�zB	�B	�nB	n�B	fSB	YB	D�B	?kB	<XB	5-B	%�B	�B	�B	�B	fB	HB	
/B	B	B��B�B�WB��B��B��B��B˸B��B�	B�bB�B	>B	 �B	(�B	*�B	64B	:IB	;RB	AxB	E�B	H�B	M�B	F�B	M�B	]B	a3B	eJB	gXB	h^B	ibB	mzB	p�B	v�B	z�B	��B	�%B	�OB	�fB	�zB	��B	��B	��B	��B	��B	�!B	ɡB	��B	�-B	�iB	�B	��B	��B
�B
�B
�B	��B
 �B
�B
	B

!B

 B
B
�B	��B	��B	��B	�uB	�}B	��B	��B
:B
HB
LB
HB
JB
cB
�B
�B
(�B
)�B
*�B
)�B
*�B
)�B
(�B
'�B
'�B
&�B
%�B
&�B
&�B
&�B
&�B
&�B
%�B
$�B
"�B
#�B
#�B
#�B
"�B
"�B
�B
�B
iB
dB
IB
9B
B	��B	�DB	�B	��B	ņB	�wB	�cB	�7B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�fB	ȗB	ȘB	ǐB	��B	��B	��B	��B	��B	��B	�B	�/B	�FB	�VB	�B	��B
 �B	��B	�B	�NB	�6B	��B	˫B	ǒB	ƋB	ŇB	ńB	ŅB	ńB	ƋB	ǐB	ǒB	ȕB	ǐB	ƋB	ƊB	ņB	�B	�lB	�fB	�eB	�SB	�HB	�HB	�:B	�IB	�LB	�UB	�wB	˪B	��B	��B	��B	�
B	�B	�B	�$B	�5B	�<B	�AB	�AB	�BB	�AB	�:B	�:B	�6B	�1B	�*B	�/B	�6B	�*B	�B	�B	�B	�B	�B	�GB	�bB	�jB	�yB	�~B	�tB	�[B	�SB	�MB	�FB	�>B	�:B	�5B	�'B	�$B	�%B	�,B	�GB	�AB	�9B	�0B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�%B	�B	�B	�#B	�*B	�@B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
 B
�G�O�B
7B
YB
gB
!�B
$�B
(�B
.�B
4B
>TB
DyB
H�B
L�B
Q�B
[ B
b,B
e<B
iUB
lhB
p�B
t�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.3 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451262016080714512620160807145126  AO  ARCAADJP                                                                    20160213031539    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160213031539  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160213031539  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145126  IP                  G�O�G�O�G�O�                