CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-26T02:16:34Z AOML 3.0 creation; 2016-08-07T21:51:19Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150626021634  20160807145119  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               <A   AO  5287_9017_060                   2C  D   APEX                            6529                            072314                          846 @�[7����1   @�[8'��@0Y�+�d��t�j1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    <A   B   B   @@  @�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC+�fC.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dys3D��D�FfD�ffD��fD�fD�C3D�� D�ɚD��fD�9�D�l�D�ٚD�  D�<�Dړ3D�� D�	�D�33D�ffD�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @S33@���@ə�A��A$��AC33Ad��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB��B	33B33B33B!33B)33B133B933BA33BI33BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�B���Bܙ�B���B䙚B虚B왚B�B���B���B���C L�CL�CfgCL�CL�C
33CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*33C,33C.L�C0L�C2fgC4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt� Dy�fD�gD�P D�p D�� D�  D�L�D���D��4D�  D�C4D�vgD��4D�	�D�FgDڜ�D�ٚD�4D�<�D�p D�i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�l�A�p�A�t�A�r�A�bNA�dZA�`BA�`BA�bNA�bNA�bNA�dZA�dZA�dZA�ffA�hsA�hsA�hsA�hsA�hsA�hsA�jA�\)A�-A�C�A��AׅA���Aէ�A�O�A�9XA�^5A��`A�JA��HAպ^Aղ-Aա�A�^5A�oAԾwA��AҬA�l�A�z�A���A�M�A��#A�ƨA��;A˛�A�ZA��A��A�E�A��AA�ffA�ĜA��TA�ƨA��hA��DA��;A���A�5?A��+A��A���A�ĜA�ZA�z�A�{A��hA���A�I�A��RA�(�A�9XA���A�VA��
A�Q�A���A�$�A��
A�(�A��A��uA���A�dZA��yA��!A�\)A��RA���A�p�A���A�t�A��`A�O�A��;A���A��;A��A��
A���A{�Ay�AxQ�At��AsS�Aq�#ApjAnĜAmhsAk��AjI�Ai?}Ae�Ab-A_VA]AW�^ATQ�AS%AN~�AL�AJ��AG�^AFbAEVABQ�A@$�A;�;A8A�A7�A6^5A4�yA3�FA1��A/�PA-��A*��A(��A'�A';dA&�A&z�A%t�A!A�;A1'A�\A|�A�A�hA�HA{A�A�hAA�AS�A�A�`A^5A��A=qA�yA\)A�7A
I�A��AA�A��AffA-A��A(�A��A�hA�AXA�AoA �HA �u@�C�@�/@�v�@��^@�A�@�5?@��u@�@��@�@�O�@�  @�C�@�
=@�ȴ@�E�@�h@���@�
=@�\@�@��#@�-@�?}@蛦@�(�@�v�@�r�@�b@�1@���@��@�;d@��@�bN@�ƨ@߾w@�K�@ާ�@�$�@ݙ�@��@�j@ۅ@�ȴ@�n�@ٺ^@��/@�j@��@�ƨ@�;d@��@֧�@թ�@Դ9@��@��;@���@��;@Ӿw@ӝ�@�33@�~�@�p�@мj@�(�@�  @�ƨ@υ@�o@�o@�~�@��#@�/@��`@�I�@�l�@ʟ�@�{@���@�ƨ@Ɵ�@�O�@�/@���@ă@��m@\@���@�z�@�j@�1'@���@��m@�ƨ@��@�+@���@�M�@��@��h@�7L@��@�z�@�b@���@�33@���@�^5@��T@���@�@�O�@��@�V@���@��`@��/@���@���@�(�@���@���@�\)@�33@��@�^5@�5?@��#@�@���@��h@�G�@��/@�r�@� �@��@��;@�ƨ@��w@�33@�v�@�M�@�E�@�-@���@�p�@��@��@���@�(�@��w@���@�33@�
=@���@�M�@���@�p�@�V@��@�9X@�b@�t�@�S�@��@�ȴ@�=q@��h@�&�@���@��/@�Ĝ@�9X@��;@�;d@��H@���@�n�@�E�@�-@��@��^@�`B@�7L@�&�@�&�@���@�bN@� �@��;@���@���@�5?@���@���@��7@�p�@�?}@���@�r�@��m@��F@���@�|�@�dZ@�33@��@��@���@��@��^@��-@��-@���@�x�@�p�@�hs@�`B@���@� �@���@��F@�"�@�ȴ@���@�ff@�5?@�J@��@�@�x�@�O�@�/@�%@��@���@���@�9X@�1@�1@�  @��@��m@�ƨ@�dZ@�o@��y@���@��!@��\@�5?@�@�O�@��@��9@�A�@�  @��;@��
@�ƨ@���@��@�S�@�@�v�@�{@�@�x�@�&�@��@��@��`@���@���@�r�@�Z@�I�@� �@��m@�l�@��@���@��+@�E�@���@��^@���@�hs@�/@��@��/@��9@�I�@�1'@�b@�ƨ@�dZ@�K�@�K�@�G�@�ƨ@~@t�@m��@a��@Xb@N$�@CS�@=�@6$�@0bN@)��@"J@z�@ �@��@7L@��@	�#@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�l�A�p�A�t�A�r�A�bNA�dZA�`BA�`BA�bNA�bNA�bNA�dZA�dZA�dZA�ffA�hsA�hsA�hsA�hsA�hsA�hsA�jA�\)A�-A�C�A��AׅA���Aէ�A�O�A�9XA�^5A��`A�JA��HAպ^Aղ-Aա�A�^5A�oAԾwA��AҬA�l�A�z�A���A�M�A��#A�ƨA��;A˛�A�ZA��A��A�E�A��AA�ffA�ĜA��TA�ƨA��hA��DA��;A���A�5?A��+A��A���A�ĜA�ZA�z�A�{A��hA���A�I�A��RA�(�A�9XA���A�VA��
A�Q�A���A�$�A��
A�(�A��A��uA���A�dZA��yA��!A�\)A��RA���A�p�A���A�t�A��`A�O�A��;A���A��;A��A��
A���A{�Ay�AxQ�At��AsS�Aq�#ApjAnĜAmhsAk��AjI�Ai?}Ae�Ab-A_VA]AW�^ATQ�AS%AN~�AL�AJ��AG�^AFbAEVABQ�A@$�A;�;A8A�A7�A6^5A4�yA3�FA1��A/�PA-��A*��A(��A'�A';dA&�A&z�A%t�A!A�;A1'A�\A|�A�A�hA�HA{A�A�hAA�AS�A�A�`A^5A��A=qA�yA\)A�7A
I�A��AA�A��AffA-A��A(�A��A�hA�AXA�AoA �HA �u@�C�@�/@�v�@��^@�A�@�5?@��u@�@��@�@�O�@�  @�C�@�
=@�ȴ@�E�@�h@���@�
=@�\@�@��#@�-@�?}@蛦@�(�@�v�@�r�@�b@�1@���@��@�;d@��@�bN@�ƨ@߾w@�K�@ާ�@�$�@ݙ�@��@�j@ۅ@�ȴ@�n�@ٺ^@��/@�j@��@�ƨ@�;d@��@֧�@թ�@Դ9@��@��;@���@��;@Ӿw@ӝ�@�33@�~�@�p�@мj@�(�@�  @�ƨ@υ@�o@�o@�~�@��#@�/@��`@�I�@�l�@ʟ�@�{@���@�ƨ@Ɵ�@�O�@�/@���@ă@��m@\@���@�z�@�j@�1'@���@��m@�ƨ@��@�+@���@�M�@��@��h@�7L@��@�z�@�b@���@�33@���@�^5@��T@���@�@�O�@��@�V@���@��`@��/@���@���@�(�@���@���@�\)@�33@��@�^5@�5?@��#@�@���@��h@�G�@��/@�r�@� �@��@��;@�ƨ@��w@�33@�v�@�M�@�E�@�-@���@�p�@��@��@���@�(�@��w@���@�33@�
=@���@�M�@���@�p�@�V@��@�9X@�b@�t�@�S�@��@�ȴ@�=q@��h@�&�@���@��/@�Ĝ@�9X@��;@�;d@��H@���@�n�@�E�@�-@��@��^@�`B@�7L@�&�@�&�@���@�bN@� �@��;@���@���@�5?@���@���@��7@�p�@�?}@���@�r�@��m@��F@���@�|�@�dZ@�33@��@��@���@��@��^@��-@��-@���@�x�@�p�@�hs@�`B@���@� �@���@��F@�"�@�ȴ@���@�ff@�5?@�J@��@�@�x�@�O�@�/@�%@��@���@���@�9X@�1@�1@�  @��@��m@�ƨ@�dZ@�o@��y@���@��!@��\@�5?@�@�O�@��@��9@�A�@�  @��;@��
@�ƨ@���@��@�S�@�@�v�@�{@�@�x�@�&�@��@��@��`@���@���@�r�@�Z@�I�@� �@��m@�l�@��@���@��+@�E�@���@��^@���@�hs@�/@��@��/@��9@�I�@�1'@�b@�ƨ@�dZ@�K�G�O�@�G�@�ƨ@~@t�@m��@a��@Xb@N$�@CS�@=�@6$�@0bN@)��@"J@z�@ �@��@7L@��@	�#@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	k�B	k�B	jB	jB	jB	jB	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	k�B	n�B	z�B	�B	�yB
bB
%B	�mB	�`B	�B
B
?}B
\)B
gmB
q�B
}�B
�VB
��B
�jB
��B
�mB�Br�Bv�B�BǮB�mBbBhB��BB
=B2-B9XB?}BI�BT�BaHB_;B]/B\)B_;BW
BP�BVBXBS�BO�B7LB.B0!B�BB��B��B��B��B�B�yB�5B��B�jB�3B��B�JBs�Bs�B�1B�{B� B;dB!�B
��B
�B
ƨB
�XB
�-B
�B
��B
q�B
I�B
7LB
,B
#�B
�B
B	�sB	�
B	��B	�dB	�'B	��B	��B	��B	�VB	�%B	~�B	w�B	ffB	T�B	F�B	;dB	%�B	�B	hB	B��B��B�B�B�mB�HB�)B�B�B�
B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B�B�B�
B�#B�#B�)B�#B�)B�;B�NB�`B�`B�fB�fB�mB�mB�sB�mB�ZB�NB�HB�;B�mB��B��B	B	B	%B	+B	%B	B	+B	1B	1B	%B	B	  B��B	B	  B��B��B��B��B	B	B	%B	1B	PB	oB	�B	�B	"�B	'�B	&�B	'�B	(�B	(�B	+B	-B	.B	-B	-B	,B	,B	,B	+B	+B	)�B	.B	1'B	33B	6FB	7LB	<jB	F�B	F�B	G�B	H�B	L�B	L�B	N�B	O�B	P�B	P�B	P�B	R�B	T�B	VB	YB	]/B	`BB	bNB	iyB	m�B	r�B	s�B	v�B	y�B	}�B	� B	�B	�B	�B	�B	�B	�B	�B	� B	�B	� B	� B	� B	�B	�B	�B	�B	�+B	�%B	�%B	�B	�B	�B	~�B	� B	� B	� B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�PB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�9B	�9B	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�LB	�RB	�^B	�dB	�dB	�jB	�dB	�qB	��B	��B	��B	��B	��B	��B	B	B	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�#B	�/B	�5B	�BB	�BB	�HB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B
DB
DB
DB
JB
PB
PB
VB
\B
\B
hB
hB
oB
{B
{B
{B
{B
�B
�B
�B
�B
!�B
'�B
/B
33B
:^B
@�B
H�B
P�B
VB
ZB
_;B
dZB
k�B
p�B
s�B
v�B
z�B
~�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	k{B	k}B	jyB	juB	jxB	jxB	kB	k{B	k{B	k{B	k{B	k}B	k{B	k{B	k{B	k{B	k{B	k{B	k{B	k}B	k}B	k}B	n�B	z�B	��B	�kB
QB
B	�`B	�SB	�yB
B
?kB
\B
g\B
q�B
}�B
�DB
��B
�UB
��B
�WB~Br�Bv�B��BǗB�TBIBPB��B �B
#B2B9@B?eBI�BT�Ba,B_ B]B\B_#BV�BP�BU�BW�BS�BO�B71B. B0	B�BB��B��B��B��B�B�_B�B��B�OB�B��B�,Bs�Bs�B�B�`B�B;KB!�B
��B
��B
ƎB
�@B
�B
��B
�mB
q�B
I�B
75B
+�B
#�B
xB
B	�\B	��B	��B	�PB	�B	��B	��B	�oB	�@B	�B	~�B	w�B	fRB	T�B	F�B	;SB	%�B	�B	ZB		B��B��B�B�vB�]B�<B�B�B�B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�*B�>B�MB�OB�SB�TB�ZB�[B�`B�YB�HB�=B�2B�(B�YB��B��B	 �B	B	B	B	B	B	B	B	B	B	�B��B��B	�B��B��B��B��B��B	�B	B	B	B	:B	XB	oB	�B	"�B	'�B	&�B	'�B	(�B	(�B	*�B	,�B	-�B	,�B	,�B	+�B	+�B	+�B	*�B	*�B	)�B	-�B	1B	3B	60B	74B	<RB	F�B	F�B	G�B	H�B	L�B	L�B	N�B	O�B	P�B	P�B	P�B	R�B	T�B	U�B	X�B	]B	`&B	b2B	i`B	mvB	r�B	s�B	v�B	y�B	}�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	��B	��B	�B	��B	�B	�	B	�B	�B	��B	��B	~�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�GB	�XB	�^B	�jB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�*B	�&B	�&B	�%B	�%B	�'B	�.B	�4B	�BB	�DB	�GB	�LB	�FB	�TB	�fB	�fB	�gB	�dB	�lB	�jB	�pB	�sB	ĀB	ƉB	ǑB	ȕB	ʤB	˧B	̯B	ͶB	λB	κB	ʹB	ʹB	θB	��B	ϿB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�$B	�"B	�)B	�.B	�0B	�3B	�:B	�AB	�BB	�@B	�@B	�@B	�AB	�EB	�FB	�TB	�ZB	�_B	�lB	�qB	�pB	�tB	�jB	�nB	�rB	�xB	�}B	�}B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
	B
B
B
	B
	B
	B
	B
	B
	B

B

B

B

B

B
#B
#B
&B
)B
-B
/B
3B
9B
9B
FB
FB
NB
[B
YB
ZB
YB
`B
`G�O�B
kB
!�B
'�B
.�B
3B
:<B
@aB
H�B
P�B
U�B
Y�B
_B
d7B
kcB
p�B
s�B
v�B
z�B
~�B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.3 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451192016080714511920160807145119  AO  ARCAADJP                                                                    20150626021634    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150626021634  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150626021634  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145119  IP                  G�O�G�O�G�O�                