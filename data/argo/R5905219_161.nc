CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-08-30T03:43:09Z creation;2022-08-30T03:43:10Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220830034309  20220830040206  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @��/�c1   @������@3š����d+333331   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�  A   A!��A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0ffB8ffB?��BH  BP  BXffB_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C �C�C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb�fDc  Dc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Djy�Dk  Dk� Dk��Dl� Dm  Dm� Dn  Dny�Do  Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�3D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ DÀ D�� D�3D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ D�|�D�� D�  D�@ Dˀ D�� D�  D�@ D̃3D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЃ3D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�<�DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D��3D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�C3D݀ D�� D�  D�@ Dހ D�� D�  D�<�D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D��D���D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�,�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@ə�A��A&fgAD��Ad��A�33A�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B	33B33B33B ��B)33B1��B9��B@��BI33BQ33BY��B`��Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�fgBș�B̙�BЙ�Bԙ�Bؙ�Bܙ�B���B䙚B虚B���B�B���B���B���C fgCfgCfgCL�CL�C
L�CL�CL�CL�CL�CL�CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(fgC*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CDL�CFL�CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D��D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D��D3D�3D3D�3D3D��D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-��D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=�D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB�DB�3DC3DC�3DD3DD�3DE�DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]�D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db�Db��Dc3Dc��Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di��Dj3Dj��Dk3Dk�3Dl�Dl�3Dm3Dm�3Dn3Dn��Do3Do�3Dp3Dp�3Dq3Dq��Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3Dx3Dx�3Dy3Dy�3Dz3Dz�3D{3D{�3D|3D|�3D}3D}�3D~3D~�3D3D�3D�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D��gD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D���D�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�gD�FgD���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�FgD���D�ɚD�	�D�I�D���D�ɚD�	�D�FgD���D�ɚD�	�D�I�D���D��gD�gD�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD��D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D��gD�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�L�D���D�ɚD��D�I�D���D�ɚD�	�D�L�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�FgD��gD�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�FgD���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D��gD�ɚD�	�D�I�D���D�ɚD�	�D�L�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�DgD�ɚD�	�D�I�DÉ�D�ɚD��D�I�Dĉ�D�ɚD�	�D�I�Dŉ�D�ɚD�	�D�I�DƉ�D�ɚD�	�D�I�Dǉ�D�ɚD�	�D�I�Dȉ�D�ɚD�	�D�I�Dɉ�D�ɚD�	�D�I�DʆgD�ɚD�	�D�I�Dˉ�D�ɚD�	�D�I�Ď�D�ɚD�	�D�I�D͉�D�ɚD�	�D�I�DΉ�D�ɚD�	�D�I�Dω�D�ɚD�	�D�I�DЌ�D�ɚD�	�D�I�Dщ�D�ɚD�	�D�I�D҉�D�ɚD�	�D�FgDӉ�D�ɚD�	�D�I�Dԉ�D�ɚD�	�D�I�DՉ�D�ɚD�	�D�I�D։�D�ɚD�	�D�I�D׉�D���D�	�D�I�D؉�D�ɚD�	�D�I�Dى�D�ɚD�	�D�I�Dډ�D�ɚD�	�D�I�Dۉ�D�ɚD�	�D�I�D܉�D�ɚD�	�D�L�D݉�D�ɚD�	�D�I�Dމ�D�ɚD�	�D�FgD߉�D�ɚD�	�D�I�D���D�ɚD�	�D�I�DቚD�ɚD�	�D�FgD≚D�ɚD�	�D�I�D㉚D���D�	�D�I�D䉚D�ɚD�	�D�I�D剚D�ɚD�	�D�I�D扚D�ɚD�	�D�I�D牚D�ɚD�	�D�I�D艚D�ɚD�	�D�I�D鉚D�ɚD�	�D�I�DꉚD�ɚD�	�D�I�D뉚D�ɚD�	�D�I�D쉚D�ɚD�	�D�I�D퉚D�ɚD�gD�I�DD��gD�gD�I�DD�ɚD�	�D�I�D���D�ɚD�	�D�I�D�D��gD�	�D�I�D�D�ɚD�	�D�I�D�D�ɚD�	�D�I�D�D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�6g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AہA�~�A�v�A�`BA�v�A�VA�K�A�I�A�I�A�G�A�C�A�?}A�?}A�;dA�;dA�;dA�;dA�5?A�1'A�+A��yAؗ�Aֲ-A�z�A�  A��`A��#A�(�A��AɾwA�/A�\)A�ƨA��A�E�A���A�A�{A�hsA���A�p�A�"�A�;dA�ĜA�M�A��A���A�
=A�A���A��;A��A�`BA�I�A��`A�E�A�?}A���A���A�l�A�"�A�33A�I�A��A���A�O�A�bNA�(�A�JA�+A�A��^A�I�A���A�7LA��-A���A���A�|�A�^5A��A�G�A��HA��A�Q�A�p�A��DA�
=A�  A�\)A��uA���A���A�S�A�A{�-Av��Atn�As\)As
=Ar�Aq��Ap$�Ak�;Ai�wAg��AbȴA`jA_K�A\JAZ�AZ�+AXAT�AR�AP(�ALE�AKx�AJ��AI��AGx�ACl�ABn�AA/A>jA<�!A;�#A;�-A;?}A:z�A8A�A6��A5�
A4�uA2ffA0ZA/�-A/XA.�RA.ffA-��A-l�A,�A*��A)�PA)"�A(=qA'|�A&^5A%��A$�uA#A#%A"ffA!/A r�A�
A�A��AoA�uA  A~�A�A  A�mA��A(�A^5A�-AG�A��AĜAr�AVA;dAI�A%A�\AO�A%A�A�HA-A
~�A-A�PAK�A�AI�A�A�wA�`A��A�A�9A�PA �`A r�A @��@��@�{@��
@�S�@���@��\@�`B@���@���@��@�  @��@��@�z�@�w@���@�V@��@�S�@�
=@�\@���@�A�@��@�-@���@���@�$�@�^5@܃@�ff@�/@�j@�bN@�A�@�z�@�r�@�z�@ج@�V@١�@���@�"�@ڏ\@�M�@�@ف@��`@�I�@��y@�$�@ղ-@�dZ@ҸR@�{@щ7@��@Ѓ@ϥ�@�+@Ο�@ͩ�@��@�Ĝ@�bN@˝�@�hs@ǝ�@���@�^5@�Ĝ@ă@�bN@�1'@þw@�X@���@�z�@�A�@�dZ@�~�@���@��@���@�9X@���@��
@�"�@��R@��\@�M�@�J@�@��@��^@��7@��@�j@�l�@��+@��@��@���@���@��^@�%@�Z@�  @���@�C�@��R@�v�@�M�@��@���@���@�V@��9@�Z@���@�t�@�+@���@��R@���@�v�@��h@��u@���@�t�@�
=@�ȴ@���@�V@�-@���@���@�p�@�hs@�&�@���@���@��j@��u@�bN@�A�@�1'@�1@�ƨ@���@�o@���@��R@���@���@���@���@��\@�5?@��h@�/@��@�%@���@��@��`@���@���@�(�@���@��@��@�dZ@�
=@���@�5?@���@���@�?}@���@�(�@���@���@���@��m@���@�\)@�o@�o@��@��!@�v�@��@��h@�`B@�/@�Ĝ@��@�z�@�bN@��@��P@�n�@��@��T@���@�O�@��`@��/@���@��j@���@�z�@�Z@�1'@��@�  @��w@���@�n�@�M�@�M�@�V@�V@�=q@��^@�x�@��@��@��/@��9@�z�@���@���@��w@��F@���@��P@�|�@��@�J@�^5@�J@���@��h@�x�@�X@��@��@�V@��/@���@���@���@���@�V@�V@�V@�%@���@�I�@�9X@�b@��m@��;@���@���@�S�@���@�ff@��@�p�@�G�@�&�@��@��@��@�r�@�A�@�t�@�@���@�n�@��@���@��7@�p�@���@��u@�z�@�1@��
@��w@�33@�~�@�ff@�ff@�M�@�=q@�-@�{@�@��#@���@��h@�/@�r�@�(�@��m@��w@���@��@�dZ@�K�@�"�@�o@�
=@��H@���@�=q@��@�@�@���@���@�x�@�&�@��@��`@��`@��/@���@�Ĝ@�bN@� �@�b@�1@�@l�@~��@~��@~5?@}�T@}p�@}/@}�@}V@|�@|�@|��@|z�@|1@{��@{33@z�@y�@y��@y��@y%@x�u@w�;@wK�@w;d@w�@v�@vv�@v5?@u�T@u�h@u`B@u�@t��@t��@t9X@s��@sƨ@s��@sC�@r��@r�@q��@qhs@p��@p��@p�@pA�@o�@o|�@o+@n��@n�R@nv�@n@m��@mp�@m�@lI�@kƨ@kt�@k33@j�H@j��@jn�@j=q@i��@iX@hbN@hA�@hA�@hA�@h1'@h1'@h1'@h  @g|�@g�@f�+@e@eO�@e�@d��@c�m@cS�@b�H@b��@b�\@b^5@b-@a�^@a�7@aG�@a�@a%@`��@`�9@` �@_�@_�@_��@_|�@_K�@^��@^��@^ȴ@^5?@]�@]�T@]@]�-@]/@\��@\�@[�m@[t�@Z��@Z��@Z^5@Y��@Yx�@Y7L@Y�@X��@X�u@X �@W\)@V�y@V�R@V��@V��@V�+@V@U�h@U`B@U?}@T�D@T1@S�F@R�@R�\@R=q@Q�@Q�^@Q%@O�@O\)@OK�@O�@N�@Nȴ@N��@NV@N5?@N{@M��@M�@LI�@K�F@KdZ@KC�@Ko@J�@J��@Jn�@J-@J�@JJ@I�#@I��@IX@I%@H�9@H1'@G�P@G+@F�R@F��@F��@Fv�@FV@F$�@E�T@E@Ep�@E?}@E/@E/@E�@D�/@D��@Dj@DI�@Cƨ@C33@B-@A�^@@b@?l�@?\)@?�@>��@>5?@>{@=�@=�-@=�@=�@<�j@;�m@;ƨ@;dZ@:�H@:�\@:M�@:J@9��@9�7@9G�@9�@8�`@8��@8��@8��@8��@8r�@8Q�@7�w@7;d@6��@6�+@6ff@6@5@5?}@4��@4�j@3��@3dZ@3C�@333@3o@2�H@2~�@2J@1��@1�#@1�^@1�7@1�@0�@/�w@/�@/�P@/l�@/�@.�y@.��@.V@.$�@-�@-��@-�-@-��@-?}@-V@,�/@,��@,Z@,9X@,(�@+�m@+�@+"�@*�@*��@*��@*�\@*^5@*-@*J@*J@)��@)hs@)X@)G�@)7L@)�@(Ĝ@(��@(�@(Q�@( �@'�@'��@'��@'|�@'l�@'K�@'
=@&�+@&$�@&@%�@%@%/@$�/@$��@$�j@$��@$9X@#��@#dZ@#C�@#"�@"�@"��@"~�@"=q@!��@!��@!7L@!%@ �`@ ��@ �@ bN@ A�@ 1'@ b@ b@   @�w@��@�P@|�@K�@�y@��@ff@$�@��@�h@`B@V@�j@��@j@��@��@�@t�@"�@�H@�\@n�@-@�@�@�#@��@�^@�7@hs@7L@�@%@�`@�@Q�@1'@  @�w@�@�@�@��@�P@l�@K�@;d@��@�@�@�+@E�@@@��@�h@�@O�@V@��@�j@��@�@�D@I�@1@ƨ@�F@��@��@��@��@��@dZ@S�@33@o@@��@n�@=q@�@��@��@��@�#@��@�7@hs@7L@&�@&�@�@��@Ĝ@�9@bN@A�@1'@b@�@�;@��@;d@+@�@�+@v�@ff@E�@$�@�T@@�@`B@O�@/@�@V@��@�@j@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AہA�~�A�v�A�`BA�v�A�VA�K�A�I�A�I�A�G�A�C�A�?}A�?}A�;dA�;dA�;dA�;dA�5?A�1'A�+A��yAؗ�Aֲ-A�z�A�  A��`A��#A�(�A��AɾwA�/A�\)A�ƨA��A�E�A���A�A�{A�hsA���A�p�A�"�A�;dA�ĜA�M�A��A���A�
=A�A���A��;A��A�`BA�I�A��`A�E�A�?}A���A���A�l�A�"�A�33A�I�A��A���A�O�A�bNA�(�A�JA�+A�A��^A�I�A���A�7LA��-A���A���A�|�A�^5A��A�G�A��HA��A�Q�A�p�A��DA�
=A�  A�\)A��uA���A���A�S�A�A{�-Av��Atn�As\)As
=Ar�Aq��Ap$�Ak�;Ai�wAg��AbȴA`jA_K�A\JAZ�AZ�+AXAT�AR�AP(�ALE�AKx�AJ��AI��AGx�ACl�ABn�AA/A>jA<�!A;�#A;�-A;?}A:z�A8A�A6��A5�
A4�uA2ffA0ZA/�-A/XA.�RA.ffA-��A-l�A,�A*��A)�PA)"�A(=qA'|�A&^5A%��A$�uA#A#%A"ffA!/A r�A�
A�A��AoA�uA  A~�A�A  A�mA��A(�A^5A�-AG�A��AĜAr�AVA;dAI�A%A�\AO�A%A�A�HA-A
~�A-A�PAK�A�AI�A�A�wA�`A��A�A�9A�PA �`A r�A @��@��@�{@��
@�S�@���@��\@�`B@���@���@��@�  @��@��@�z�@�w@���@�V@��@�S�@�
=@�\@���@�A�@��@�-@���@���@�$�@�^5@܃@�ff@�/@�j@�bN@�A�@�z�@�r�@�z�@ج@�V@١�@���@�"�@ڏ\@�M�@�@ف@��`@�I�@��y@�$�@ղ-@�dZ@ҸR@�{@щ7@��@Ѓ@ϥ�@�+@Ο�@ͩ�@��@�Ĝ@�bN@˝�@�hs@ǝ�@���@�^5@�Ĝ@ă@�bN@�1'@þw@�X@���@�z�@�A�@�dZ@�~�@���@��@���@�9X@���@��
@�"�@��R@��\@�M�@�J@�@��@��^@��7@��@�j@�l�@��+@��@��@���@���@��^@�%@�Z@�  @���@�C�@��R@�v�@�M�@��@���@���@�V@��9@�Z@���@�t�@�+@���@��R@���@�v�@��h@��u@���@�t�@�
=@�ȴ@���@�V@�-@���@���@�p�@�hs@�&�@���@���@��j@��u@�bN@�A�@�1'@�1@�ƨ@���@�o@���@��R@���@���@���@���@��\@�5?@��h@�/@��@�%@���@��@��`@���@���@�(�@���@��@��@�dZ@�
=@���@�5?@���@���@�?}@���@�(�@���@���@���@��m@���@�\)@�o@�o@��@��!@�v�@��@��h@�`B@�/@�Ĝ@��@�z�@�bN@��@��P@�n�@��@��T@���@�O�@��`@��/@���@��j@���@�z�@�Z@�1'@��@�  @��w@���@�n�@�M�@�M�@�V@�V@�=q@��^@�x�@��@��@��/@��9@�z�@���@���@��w@��F@���@��P@�|�@��@�J@�^5@�J@���@��h@�x�@�X@��@��@�V@��/@���@���@���@���@�V@�V@�V@�%@���@�I�@�9X@�b@��m@��;@���@���@�S�@���@�ff@��@�p�@�G�@�&�@��@��@��@�r�@�A�@�t�@�@���@�n�@��@���@��7@�p�@���@��u@�z�@�1@��
@��w@�33@�~�@�ff@�ff@�M�@�=q@�-@�{@�@��#@���@��h@�/@�r�@�(�@��m@��w@���@��@�dZ@�K�@�"�@�o@�
=@��H@���@�=q@��@�@�@���@���@�x�@�&�@��@��`@��`@��/@���@�Ĝ@�bN@� �@�b@�1@�@l�@~��@~��@~5?@}�T@}p�@}/@}�@}V@|�@|�@|��@|z�@|1@{��@{33@z�@y�@y��@y��@y%@x�u@w�;@wK�@w;d@w�@v�@vv�@v5?@u�T@u�h@u`B@u�@t��@t��@t9X@s��@sƨ@s��@sC�@r��@r�@q��@qhs@p��@p��@p�@pA�@o�@o|�@o+@n��@n�R@nv�@n@m��@mp�@m�@lI�@kƨ@kt�@k33@j�H@j��@jn�@j=q@i��@iX@hbN@hA�@hA�@hA�@h1'@h1'@h1'@h  @g|�@g�@f�+@e@eO�@e�@d��@c�m@cS�@b�H@b��@b�\@b^5@b-@a�^@a�7@aG�@a�@a%@`��@`�9@` �@_�@_�@_��@_|�@_K�@^��@^��@^ȴ@^5?@]�@]�T@]@]�-@]/@\��@\�@[�m@[t�@Z��@Z��@Z^5@Y��@Yx�@Y7L@Y�@X��@X�u@X �@W\)@V�y@V�R@V��@V��@V�+@V@U�h@U`B@U?}@T�D@T1@S�F@R�@R�\@R=q@Q�@Q�^@Q%@O�@O\)@OK�@O�@N�@Nȴ@N��@NV@N5?@N{@M��@M�@LI�@K�F@KdZ@KC�@Ko@J�@J��@Jn�@J-@J�@JJ@I�#@I��@IX@I%@H�9@H1'@G�P@G+@F�R@F��@F��@Fv�@FV@F$�@E�T@E@Ep�@E?}@E/@E/@E�@D�/@D��@Dj@DI�@Cƨ@C33@B-@A�^@@b@?l�@?\)@?�@>��@>5?@>{@=�@=�-@=�@=�@<�j@;�m@;ƨ@;dZ@:�H@:�\@:M�@:J@9��@9�7@9G�@9�@8�`@8��@8��@8��@8��@8r�@8Q�@7�w@7;d@6��@6�+@6ff@6@5@5?}@4��@4�j@3��@3dZ@3C�@333@3o@2�H@2~�@2J@1��@1�#@1�^@1�7@1�@0�@/�w@/�@/�P@/l�@/�@.�y@.��@.V@.$�@-�@-��@-�-@-��@-?}@-V@,�/@,��@,Z@,9X@,(�@+�m@+�@+"�@*�@*��@*��@*�\@*^5@*-@*J@*J@)��@)hs@)X@)G�@)7L@)�@(Ĝ@(��@(�@(Q�@( �@'�@'��@'��@'|�@'l�@'K�@'
=@&�+@&$�@&@%�@%@%/@$�/@$��@$�j@$��@$9X@#��@#dZ@#C�@#"�@"�@"��@"~�@"=q@!��@!��@!7L@!%@ �`@ ��@ �@ bN@ A�@ 1'@ b@ b@   @�w@��@�P@|�@K�@�y@��@ff@$�@��@�h@`B@V@�j@��@j@��@��@�@t�@"�@�H@�\@n�@-@�@�@�#@��@�^@�7@hs@7L@�@%@�`@�@Q�@1'@  @�w@�@�@�@��@�P@l�@K�@;d@��@�@�@�+@E�@@@��@�h@�@O�@V@��@�j@��@�@�D@I�@1@ƨ@�F@��@��@��@��@��@dZ@S�@33@o@@��@n�@=q@�@��@��@��@�#@��@�7@hs@7L@&�@&�@�@��@Ĝ@�9@bN@A�@1'@b@�@�;@��@;d@+@�@�+@v�@ff@E�@$�@�T@@�@`B@O�@/@�@V@��@�@j@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BW
BXBYBW
BXBXBXBXBXBXBXBXBXBXBXBXBXBW
BW
BVBQ�B8RB �B%�BC�Bo�Bp�B~�B�1B�JB�oB�hB�PB�=B�7B�B�B�+B�PB�PB�bB�1B�B}�Bx�Bv�BaHB`BB\)BZBW
BQ�BM�BI�BC�B>wB=qBG�BcTBn�Bw�B�B|�Bt�Bk�BQ�BF�B5?B!�BDB�;B��B�LB��B�uBy�B_;BJ�B1'B�BoB%BB
��B
�B
�/B
��B
�^B
��B
��B
�oB
�%B
\)B
L�B
@�B
33B
DB	��B	�B	�B	�B	�mB	�/B	ǮB	�LB	��B	�hB	z�B	t�B	dZB	\)B	[#B	K�B	?}B	1'B	'�B	uB	JB		7B	B��B�B�fB�HB�;B�B��B��B��B��B��BÖB��B��B�wB�XB�FB�?B�9B�3B�3B�!B�'B�-B�B�B�B�B�B�B�!B�9B�-B�-B�'B�'B�!B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�B�B�!B�-B�3B�9B�9B�?B�RB�qB�qB�}BŢBŢBŢBǮBǮBƨB��B�#B�#B��B��B��B�B�/B�BB�HB�HB�ZB�B��B��B	B	PB	PB	VB	oB	�B	�B	�B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	#�B	'�B	(�B	)�B	+B	+B	-B	+B	)�B	)�B	'�B	(�B	+B	-B	0!B	2-B	33B	49B	8RB	9XB	9XB	9XB	:^B	;dB	;dB	<jB	=qB	B�B	E�B	G�B	H�B	G�B	H�B	H�B	I�B	I�B	L�B	O�B	P�B	R�B	W
B	[#B	\)B	]/B	_;B	`BB	bNB	ffB	hsB	jB	m�B	q�B	s�B	u�B	w�B	w�B	x�B	~�B	�B	�+B	�=B	�PB	�VB	�bB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�?B	�LB	�RB	�RB	�XB	�XB	�XB	�^B	�jB	��B	��B	B	ĜB	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�)B	�5B	�5B	�;B	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�`B	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
+B
	7B
1B
1B
	7B
	7B
1B
	7B

=B

=B

=B

=B
\B
hB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
 �B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
-B
.B
.B
.B
.B
/B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
VB
T�B
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
ffB
gmB
gmB
gmB
hsB
iyB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
r�B
r�B
s�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
u�B
u�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
~�B
}�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�=B
�=B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�=B
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�JB
�PB
�PB
�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BW
BXBYBW
BXBXBXBXBXBXBXBXBXBXBXBXBXBW
BW
BVBQ�B8RB �B%�BC�Bo�Bp�B~�B�1B�JB�oB�hB�PB�=B�7B�B�B�+B�PB�PB�bB�1B�B}�Bx�Bv�BaHB`BB\)BZBW
BQ�BM�BI�BC�B>wB=qBG�BcTBn�Bw�B�B|�Bt�Bk�BQ�BF�B5?B!�BDB�;B��B�LB��B�uBy�B_;BJ�B1'B�BoB%BB
��B
�B
�/B
��B
�^B
��B
��B
�oB
�%B
\)B
L�B
@�B
33B
DB	��B	�B	�B	�B	�mB	�/B	ǮB	�LB	��B	�hB	z�B	t�B	dZB	\)B	[#B	K�B	?}B	1'B	'�B	uB	JB		7B	B��B�B�fB�HB�;B�B��B��B��B��B��BÖB��B��B�wB�XB�FB�?B�9B�3B�3B�!B�'B�-B�B�B�B�B�B�B�!B�9B�-B�-B�'B�'B�!B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�B�B�!B�-B�3B�9B�9B�?B�RB�qB�qB�}BŢBŢBŢBǮBǮBƨB��B�#B�#B��B��B��B�B�/B�BB�HB�HB�ZB�B��B��B	B	PB	PB	VB	oB	�B	�B	�B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	#�B	'�B	(�B	)�B	+B	+B	-B	+B	)�B	)�B	'�B	(�B	+B	-B	0!B	2-B	33B	49B	8RB	9XB	9XB	9XB	:^B	;dB	;dB	<jB	=qB	B�B	E�B	G�B	H�B	G�B	H�B	H�B	I�B	I�B	L�B	O�B	P�B	R�B	W
B	[#B	\)B	]/B	_;B	`BB	bNB	ffB	hsB	jB	m�B	q�B	s�B	u�B	w�B	w�B	x�B	~�B	�B	�+B	�=B	�PB	�VB	�bB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�?B	�LB	�RB	�RB	�XB	�XB	�XB	�^B	�jB	��B	��B	B	ĜB	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�)B	�5B	�5B	�;B	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�`B	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
+B
	7B
1B
1B
	7B
	7B
1B
	7B

=B

=B

=B

=B
\B
hB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
 �B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
-B
.B
.B
.B
.B
/B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
VB
T�B
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
ffB
gmB
gmB
gmB
hsB
iyB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
r�B
r�B
s�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
u�B
u�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
~�B
}�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�=B
�=B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�=B
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�JB
�PB
�PB
�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20220830124301  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220830034309  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220830034310  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220830034310                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220830124315  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220830124315  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220830040206                      G�O�G�O�G�O�                