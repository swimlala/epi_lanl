CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-14T21:35:24Z creation;2018-09-14T21:35:27Z conversion to V3.1;2019-12-23T06:15:11Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180914213524  20200120021523  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               YA   JA  I2_0675_089                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؁n\) 1   @؁n�}( @7�N;�5��cE��&��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @   @�  @�  A   A!��A@  A`  A~ffA�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ DƼ�D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��fD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@4z�@�=q@�=qA�A&�RAE�Ae�A�A�A��\A��\A\Aҏ\A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�B`�HBiG�BqG�ByG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�C8RCQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~k�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN��DO�DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dx{Dx�{Dy{Dy�{Dz{Dz�{D{{D{�{D|{D|�{D}{D}�{D~{D~�{D{D�{D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D=D��=D�
=D�J=DÊ=D��=D�
=D�J=DĊ=D��=D�
=D�J=DŊ=D��=D�
=D�J=DƊ=D��
D�
=D�J=DǊ=D��=D�
=D�J=DȊ=D��=D�
=D�J=DɊ=D��=D�
=D�J=Dʊ=D��=D�
=D�J=Dˊ=D��=D�
=D�J=D̊=D��=D�
=D�J=D͊=D��=D�
=D�J=DΊ=D��=D�
=D�J=Dϊ=D��=D�
=D�J=DЊ=D��=D�
=D�J=Dъ=D��=D�
=D�J=DҊ=D��=D�
=D�J=Dӊ=D��=D�
=D�J=DԊ=D��=D�
=D�J=DՊ=D��=D�
=D�J=D֊=D��=D�
=D�J=D׊=D��=D�
=D�J=D؊=D��=D�
=D�J=Dي=D��=D�
=D�J=Dڊ=D��=D�
=D�J=Dۊ=D��=D�
=D�J=D܊=D��=D�
=D�J=D݊=D��=D�
=D�J=Dފ=D��=D�
=D�J=Dߊ=D��=D�
=D�J=D��=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D��=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��qD�ФD��q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�XA�XA�XA�XA�VA�ZA�^5A�^5A�^5A�`BA�`BA�bNA�bNA�bNA�dZA�ffA�ffA�dZA�dZA�ffA�dZA�dZA�dZA�ffA�ffA�dZA�dZA�jA�n�A�n�A�jAΓuA�p�A���A�%A�\)A���A��mA�t�A�n�A�1A�-A��9A��/A�dZA���A�A�A��9A���A�A�A���A��uA�1'A���A��wA�+A�bA���A���A�M�A��A�K�A�&�A�1A�A�v�A�+A��FA�VA�t�A�  A�C�A��wA�7LA�`BA��A�(�A�C�A���A��wA��A�A�K�A��DA�A���A�/A�+A�Q�A���A�33A�ƨA��\A���A��jA���A���A��A���A���A�{A�^5A��HA�n�A��+A�XA�&�A�A�A�  A��yA��+A��jA��TA���A�%A���A���A���A��A�9XA��^A�"�A�I�A��TA�$�A}�Az5?Aw�;Av�jAu7LAr��ApVAm��AmO�Alz�Aj��Ah�uAf^5AdffAa�A`�A_��A_oA\�AZz�AYt�AYAXQ�AW�AU7LARM�AQ�AP�+AO��AN��ANffAM�wALz�AJA�AH��AHn�AF5?AE�ACK�AA��A?�A=�hA9A7��A7+A4�9A3�A2��A25?A1�TA0�yA0=qA.VA-&�A,�A*  A(1A&ffA$�`A$1A"�A �`A�
A&�A�DA�Ax�AVA�A�yA��A�9A��A%A-A��A�7A�\Al�A�A�A�RAQ�AK�A��A�A�A�;A
ȴA
��A
��A
��A
��A
5?A	|�A�/A�\AA7LA�DA�-AVA��A\)A;dA
=A��Ar�A1A\)A/A ȴA Q�@���@�j@�v�@�{@��@��F@���@��@��@�A�@��@�l�@�h@�@�w@�x�@�z�@� �@�C�@旍@�hs@㝲@�j@��@�-@ݺ^@���@ۅ@���@�@�&�@�K�@���@��@�7L@�1@�%@Ь@��@�ȴ@Ο�@�5?@��@ͩ�@�j@�5?@ț�@�(�@� �@�1@Ǿw@�K�@���@Ƨ�@��@���@ř�@�?}@�&�@�Ĝ@��@�ff@�O�@��@�b@��\@�/@���@�Z@�K�@�&�@��D@���@��@��@�M�@�{@��@�G�@� �@���@�C�@���@�ȴ@���@���@���@�V@���@�r�@� �@��P@�33@���@���@�{@��7@��m@�M�@��7@�%@�1'@��@�;d@��@�V@���@�G�@��9@���@��@�1@��;@���@��@���@�v�@��@�?}@���@��u@�A�@��;@���@���@�\)@�K�@�;d@��@�@���@�v�@�5?@��#@���@���@��@�b@�1@�j@���@�@���@��!@��!@��R@�ȴ@��H@���@��H@�n�@�~�@���@��/@���@���@��D@�A�@�9X@�I�@�(�@��
@���@���@��@���@�x�@�/@��@�%@��@��@�5?@���@��^@��@���@���@���@�b@�33@���@�ȴ@��\@���@���@��\@�n�@�
=@�33@��y@��+@���@���@��h@�X@�O�@�&�@���@��`@���@�I�@�A�@�A�@�I�@�I�@�I�@�Q�@�1'@� �@��@��F@���@�|�@�+@��+@��@���@���@�p�@�G�@�7L@�&�@��j@��u@�Z@��m@��@��@���@��F@���@�C�@�ȴ@��\@�M�@�$�@��@�p�@�&�@��@�V@���@�r�@�I�@�(�@�1@�dZ@��@���@���@�5?@��^@��@��`@��@��D@�r�@�Z@��@�  @��@��
@��P@�l�@�\)@�o@�ȴ@���@�$�@�@��h@�hs@�7L@��@��j@���@��u@�z�@�bN@�9X@� �@�  @��@l�@�@~�y@~ff@~5?@}�-@}/@|��@|�@|I�@|1@{�@z�!@z�@y��@y��@x��@xQ�@w\)@w�@v�@vE�@v{@v@u�T@u�-@u��@uO�@t�@tZ@t9X@t(�@s�
@s��@r��@rM�@rJ@q��@qhs@q%@pQ�@p1'@p  @o�@nff@n@mp�@m�@l��@l�j@l��@k�m@kƨ@k�F@kt�@ko@j�H@j�!@jM�@i�@ihs@i&�@h��@h�9@h��@hr�@hA�@g��@g|�@gK�@g�@f�y@f��@fV@fV@f$�@e�-@e�@d�/@d�@dz�@d9X@dj@dI�@c��@c�F@ct�@cdZ@cS�@b�H@b~�@b^5@bM�@a��@aX@a&�@a�@a%@`�`@`bN@`  @_�@_l�@^��@^ff@^5?@^{@^{@^@]�@]��@]�h@]O�@]?}@\�j@\1@[�@[S�@[33@[o@Z�H@Z^5@Y��@Y�@Y�#@Y�#@Y�^@Y�7@Y&�@Y%@X�9@XbN@X �@W�@W\)@V�R@VV@U@U��@U�@Tz�@T�@S��@R��@Q��@Qhs@P��@PĜ@P�9@P��@P�u@PbN@Pb@P  @P  @O�@O|�@OK�@O
=@Nȴ@NV@M�T@M?}@L�/@L1@K"�@J��@J�\@Jn�@J�@I�^@Ihs@I7L@I�@H�9@H1'@G�@G�;@G��@G+@Fv�@F$�@E��@E/@EV@D��@D��@Dz�@C�
@CdZ@CS�@CC�@C33@B�@B�\@B^5@A��@AX@A&�@A%@@�`@@��@@�9@@�u@@r�@@ �@?��@?��@?|�@?K�@?+@?+@?�@>��@>�+@>5?@>@=@=`B@<��@<j@<9X@<(�@<�@<�@;�m@;ƨ@;�F@;�@;33@:�@:�H@:��@:�\@:�@9��@9X@9G�@8��@8�@8bN@7�;@7�P@7\)@7�@6�y@6��@6E�@65?@6{@6@5�@5��@5@5��@5V@4�D@4�@3ƨ@3S�@2��@2��@2n�@2M�@1�@1��@1x�@1hs@1&�@0��@0Q�@0  @/�w@/�P@.��@.��@.$�@-�T@-�-@-�@-�@-V@,��@,�/@,�D@,Z@,9X@,�@+�m@+��@+o@*�!@*^5@*=q@*�@)�@)��@)G�@)�@)%@(��@(��@(r�@(b@'�@'�w@'�@'��@'|�@'l�@&�@&E�@&$�@&{@%��@%`B@$��@$�D@$j@$Z@$(�@$1@#��@#o@#@"��@"�!@"��@"M�@"�@"J@!�@!�#@!�^@!��@!�7@!X@ ��@ �u@ �@ r�@ Q�@  �@ b@ b@�@�P@\)@K�@K�@��@�R@��@$�@�T@�@O�@/@�@��@�/@�/@��@j@9X@��@��@S�@33@@�@�@�@��@M�@-@��@X@7L@%@r�@Q�@A�@ �@  @�w@|�@l�@l�@\)@K�@;d@�@�@�@ȴ@�R@�+@$�@�@��@@@�-@p�@O�@/@�@��@��@�j@��@Z@�@ƨ@��@�@C�@C�@@�@��@M�@-@�@�@�7@��@��@��@r�@Q�@1'@b@�@�P@;d@;d@+@+@+@+@+@�@��@�y@�R@@��@�-@��@�@?}@�@V@��@��@z�@��@ƨ@��@t�@dZ@"�@
��@
��@
^5@
M�@
=q@
�@
J@	��@	��@	�#@	x�@	%@r�@1'1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�XA�XA�XA�XA�VA�ZA�^5A�^5A�^5A�`BA�`BA�bNA�bNA�bNA�dZA�ffA�ffA�dZA�dZA�ffA�dZA�dZA�dZA�ffA�ffA�dZA�dZA�jA�n�A�n�A�jAΓuA�p�A���A�%A�\)A���A��mA�t�A�n�A�1A�-A��9A��/A�dZA���A�A�A��9A���A�A�A���A��uA�1'A���A��wA�+A�bA���A���A�M�A��A�K�A�&�A�1A�A�v�A�+A��FA�VA�t�A�  A�C�A��wA�7LA�`BA��A�(�A�C�A���A��wA��A�A�K�A��DA�A���A�/A�+A�Q�A���A�33A�ƨA��\A���A��jA���A���A��A���A���A�{A�^5A��HA�n�A��+A�XA�&�A�A�A�  A��yA��+A��jA��TA���A�%A���A���A���A��A�9XA��^A�"�A�I�A��TA�$�A}�Az5?Aw�;Av�jAu7LAr��ApVAm��AmO�Alz�Aj��Ah�uAf^5AdffAa�A`�A_��A_oA\�AZz�AYt�AYAXQ�AW�AU7LARM�AQ�AP�+AO��AN��ANffAM�wALz�AJA�AH��AHn�AF5?AE�ACK�AA��A?�A=�hA9A7��A7+A4�9A3�A2��A25?A1�TA0�yA0=qA.VA-&�A,�A*  A(1A&ffA$�`A$1A"�A �`A�
A&�A�DA�Ax�AVA�A�yA��A�9A��A%A-A��A�7A�\Al�A�A�A�RAQ�AK�A��A�A�A�;A
ȴA
��A
��A
��A
��A
5?A	|�A�/A�\AA7LA�DA�-AVA��A\)A;dA
=A��Ar�A1A\)A/A ȴA Q�@���@�j@�v�@�{@��@��F@���@��@��@�A�@��@�l�@�h@�@�w@�x�@�z�@� �@�C�@旍@�hs@㝲@�j@��@�-@ݺ^@���@ۅ@���@�@�&�@�K�@���@��@�7L@�1@�%@Ь@��@�ȴ@Ο�@�5?@��@ͩ�@�j@�5?@ț�@�(�@� �@�1@Ǿw@�K�@���@Ƨ�@��@���@ř�@�?}@�&�@�Ĝ@��@�ff@�O�@��@�b@��\@�/@���@�Z@�K�@�&�@��D@���@��@��@�M�@�{@��@�G�@� �@���@�C�@���@�ȴ@���@���@���@�V@���@�r�@� �@��P@�33@���@���@�{@��7@��m@�M�@��7@�%@�1'@��@�;d@��@�V@���@�G�@��9@���@��@�1@��;@���@��@���@�v�@��@�?}@���@��u@�A�@��;@���@���@�\)@�K�@�;d@��@�@���@�v�@�5?@��#@���@���@��@�b@�1@�j@���@�@���@��!@��!@��R@�ȴ@��H@���@��H@�n�@�~�@���@��/@���@���@��D@�A�@�9X@�I�@�(�@��
@���@���@��@���@�x�@�/@��@�%@��@��@�5?@���@��^@��@���@���@���@�b@�33@���@�ȴ@��\@���@���@��\@�n�@�
=@�33@��y@��+@���@���@��h@�X@�O�@�&�@���@��`@���@�I�@�A�@�A�@�I�@�I�@�I�@�Q�@�1'@� �@��@��F@���@�|�@�+@��+@��@���@���@�p�@�G�@�7L@�&�@��j@��u@�Z@��m@��@��@���@��F@���@�C�@�ȴ@��\@�M�@�$�@��@�p�@�&�@��@�V@���@�r�@�I�@�(�@�1@�dZ@��@���@���@�5?@��^@��@��`@��@��D@�r�@�Z@��@�  @��@��
@��P@�l�@�\)@�o@�ȴ@���@�$�@�@��h@�hs@�7L@��@��j@���@��u@�z�@�bN@�9X@� �@�  @��@l�@�@~�y@~ff@~5?@}�-@}/@|��@|�@|I�@|1@{�@z�!@z�@y��@y��@x��@xQ�@w\)@w�@v�@vE�@v{@v@u�T@u�-@u��@uO�@t�@tZ@t9X@t(�@s�
@s��@r��@rM�@rJ@q��@qhs@q%@pQ�@p1'@p  @o�@nff@n@mp�@m�@l��@l�j@l��@k�m@kƨ@k�F@kt�@ko@j�H@j�!@jM�@i�@ihs@i&�@h��@h�9@h��@hr�@hA�@g��@g|�@gK�@g�@f�y@f��@fV@fV@f$�@e�-@e�@d�/@d�@dz�@d9X@dj@dI�@c��@c�F@ct�@cdZ@cS�@b�H@b~�@b^5@bM�@a��@aX@a&�@a�@a%@`�`@`bN@`  @_�@_l�@^��@^ff@^5?@^{@^{@^@]�@]��@]�h@]O�@]?}@\�j@\1@[�@[S�@[33@[o@Z�H@Z^5@Y��@Y�@Y�#@Y�#@Y�^@Y�7@Y&�@Y%@X�9@XbN@X �@W�@W\)@V�R@VV@U@U��@U�@Tz�@T�@S��@R��@Q��@Qhs@P��@PĜ@P�9@P��@P�u@PbN@Pb@P  @P  @O�@O|�@OK�@O
=@Nȴ@NV@M�T@M?}@L�/@L1@K"�@J��@J�\@Jn�@J�@I�^@Ihs@I7L@I�@H�9@H1'@G�@G�;@G��@G+@Fv�@F$�@E��@E/@EV@D��@D��@Dz�@C�
@CdZ@CS�@CC�@C33@B�@B�\@B^5@A��@AX@A&�@A%@@�`@@��@@�9@@�u@@r�@@ �@?��@?��@?|�@?K�@?+@?+@?�@>��@>�+@>5?@>@=@=`B@<��@<j@<9X@<(�@<�@<�@;�m@;ƨ@;�F@;�@;33@:�@:�H@:��@:�\@:�@9��@9X@9G�@8��@8�@8bN@7�;@7�P@7\)@7�@6�y@6��@6E�@65?@6{@6@5�@5��@5@5��@5V@4�D@4�@3ƨ@3S�@2��@2��@2n�@2M�@1�@1��@1x�@1hs@1&�@0��@0Q�@0  @/�w@/�P@.��@.��@.$�@-�T@-�-@-�@-�@-V@,��@,�/@,�D@,Z@,9X@,�@+�m@+��@+o@*�!@*^5@*=q@*�@)�@)��@)G�@)�@)%@(��@(��@(r�@(b@'�@'�w@'�@'��@'|�@'l�@&�@&E�@&$�@&{@%��@%`B@$��@$�D@$j@$Z@$(�@$1@#��@#o@#@"��@"�!@"��@"M�@"�@"J@!�@!�#@!�^@!��@!�7@!X@ ��@ �u@ �@ r�@ Q�@  �@ b@ b@�@�P@\)@K�@K�@��@�R@��@$�@�T@�@O�@/@�@��@�/@�/@��@j@9X@��@��@S�@33@@�@�@�@��@M�@-@��@X@7L@%@r�@Q�@A�@ �@  @�w@|�@l�@l�@\)@K�@;d@�@�@�@ȴ@�R@�+@$�@�@��@@@�-@p�@O�@/@�@��@��@�j@��@Z@�@ƨ@��@�@C�@C�@@�@��@M�@-@�@�@�7@��@��@��@r�@Q�@1'@b@�@�P@;d@;d@+@+@+@+@+@�@��@�y@�R@@��@�-@��@�@?}@�@V@��@��@z�@��@ƨ@��@t�@dZ@"�@
��@
��@
^5@
M�@
=q@
�@
J@	��@	��@	�#@	x�@	%@r�@1'1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{BoBhBoBhB�B9XB5?B6FB>wBE�BH�BJ�B\)BZB`BBe`Bn�BjBk�Bn�BiyBiyBp�Bu�Bz�Bu�Bx�B}�B�B� B�B�B� B� B~�B~�B~�B{�B{�B{�By�Bv�Br�Bp�Bm�BdZBdZB�7B�uB��B��B��B�JBw�Be`BM�B49B&�B�B�B�B�B�BPB��B�B�fB�HB�B��B��B��B��B�=Bs�Bm�B_;BJ�B8RB�BDB
�BB
��B
ĜB
�3B
�!B
�B
�B
��B
��B
�oB
�B
z�B
r�B
hsB
W
B
<jB
+B
#�B
�B
+B	��B	�NB	�/B	�B	��B	�jB	�B	��B	�JB	�B	{�B	v�B	k�B	bNB	]/B	YB	S�B	M�B	C�B	49B	1'B	+B	$�B	 �B	�B	�B	�B	DB��B��B�B�BB��B��B��B�LB��B��B��B��B��B��B��B��B��B��B��B�\B�7B~�Bq�Be`B[#BT�BO�BI�BD�BC�BB�B@�B=qB;dB8RB5?B2-B1'B/B/B.B.B+B)�B'�B'�B&�B&�B%�B&�B%�B&�B(�B)�B/B1'B1'B1'B1'B2-B2-B2-B1'B2-B33B2-B2-B1'B2-B1'B1'B0!B0!B0!B0!B0!B/B/B-B-B-B(�B(�B'�B%�B$�B&�B#�B#�B"�B$�B#�B"�B"�B#�B"�B"�B#�B#�B#�B%�B'�B'�B&�B&�B&�B'�B&�B&�B'�B(�B(�B(�B(�B+B/B/B0!B2-B2-B33B2-B2-B5?B;dB>wB=qB=qB=qB?}BA�BA�BD�BG�BH�BI�BI�BI�BJ�BL�BP�BR�BS�BS�BXBXBXBYBYB]/B]/B^5B`BB`BBbNBcTBcTBffBjBm�Bp�Bt�Bt�Bt�Bx�Bz�B}�B�B�B�%B�DB�VB�\B�hB�{B��B��B��B��B��B��B�B�-B�3B�?B�XB�jB��B��B��BB��BBȴB��B��B��B��B��B��B��B��B�
B�)B�HB�NB�NB�`B�mB�B�B�B��B��B��B��B��B��B	B	%B	B	1B	
=B	VB	\B	oB	�B	�B	�B	�B	�B	"�B	'�B	(�B	)�B	,B	0!B	2-B	49B	8RB	>wB	?}B	A�B	K�B	L�B	H�B	J�B	K�B	Q�B	XB	^5B	ffB	hsB	iyB	jB	iyB	iyB	k�B	m�B	p�B	s�B	y�B	|�B	� B	�B	�%B	�1B	�PB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�?B	�?B	�FB	�RB	�jB	�qB	�wB	�}B	��B	��B	��B	��B	B	B	ĜB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�NB	�TB	�TB	�`B	�`B	�`B	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
1B
1B
	7B

=B

=B

=B
DB
DB
JB
JB
JB
PB
PB
PB
PB
VB
VB
\B
\B
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
oB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
$�B
$�B
$�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
-B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
49B
49B
49B
49B
49B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
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
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
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
B�B
C�B
C�B
C�B
C�B
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
H�B
H�B
H�B
H�B
I�B
I�B
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
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
k�B
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
n�B
n�B
o�B
p�B
p�B
p�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BaBaBaBaBaBaBaBaBaBaBaBaBaBaBaBaBaBaBaBaBaBaBaBaBaBaBaBaBaBaBTBNBTBNByB9>B5%B6+B>]BE�BH�BJ�B\BZB`'BeFBn}BjeBkkBn}Bi_Bi_Bp�Bu�Bz�Bu�Bx�B}�B��B�B��B��B�B�B~�B~�B~�B{�B{�B{�By�Bv�Br�Bp�BmwBd@Bd@B�B�[B�sB��B��B�Bw�BeFBM�B4B&�B�BmBgB�BmB6B��B�B�2B�-B��B��BˬB�OB��B�#Bs�BmwB_!BJ�B88B�B)B
�'B
бB
āB
�B
�B
��B
��B
��B
��B
�:B
�B
z�B
r�B
hXB
V�B
<PB
*�B
#�B
yB
�B	��B	�B	�B	��B	͟B	�PB	��B	��B	�0B	��B	{�B	v�B	kkB	b4B	]B	X�B	S�B	M�B	C{B	4B	1B	*�B	$�B	 �B	�B	qB	sB	B��B��B�WB�B��BʌB�iB�B��B��B��B��B��B��B��B��B��B�kB�mB�BB�B~�BqvBe,B[	BT�BO�BI�BDgBC{BBuB@iB=<B;0B8B5B2B1B/ B/ B-�B-�B*�B)�B'�B'�B&�B&�B%�B&�B%�B&�B(�B)�B.�B1B0�B0�B1B1�B1�B2B1B2B3B2B2B0�B1�B1B1B0B/�B0B0B0B/ B.�B,�B,�B,�B(�B(�B'�B%�B$�B&�B#�B#�B"�B$�B#�B"�B"�B#�B"�B"�B#�B#�B#�B%�B'�B'�B&�B&�B&�B'�B&�B&�B'�B(�B(�B(�B(�B*�B.�B.�B0B2B2B3B2B1�B5B;JB>]B=<B=VB=VB?cBAUBAoBD�BG�BH�BI�BI�BI�BJ�BL�BP�BR�BS�BS�BW�BW�BW�BX�BX�B]B\�B^B`B`'Bb4Bc:Bc:Bf2BjeBm]Bp�Bt�Bt�Bt�Bx�Bz�B}�B��B��B�B�B�"B�BB�4B�aB�SB��B��B��B��B��B��B��B��B�B�$B�6B�OB�UB�OB�[B�oB�[BȚBʌBʌB˒BΥB͟B͟BΥB��B��B�B�-B�B�B�FB�8B�eB�cB�vB��B��B��B��B��B��B	B	�B	�B	�B	
	B	<B	(B	:B	gB	YB	qB	�B	�B	"�B	'�B	(�B	)�B	+�B	0B	2B	4B	8B	>BB	?HB	AUB	K�B	L�B	H�B	J�B	K�B	Q�B	W�B	^B	f2B	hXB	i_B	jKB	i_B	i_B	kkB	m]B	poB	s�B	y�B	|�B	�B	��B	�B	��B	�B	�BB	�FB	�B	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�%B	�%B	�B	�B	�PB	�<B	�BB	�HB	�oB	�UB	�UB	�oB	�[B	�uB	�gB	�tB	ƎB	�zB	ȚB	ɆB	ʦB	˒B	ˬB	˒B	̘B	̘B	ΥB	��B	ϫB	ϫB	бB	ҽB	��B	��B	ҽB	��B	��B	��B	��B	��B	�B	��B	�B	�B	�'B	�'B	�B	�4B	�4B	� B	� B	�FB	�,B	�FB	�RB	�>B	�XB	�_B	�eB	�QB	�WB	�qB	�qB	�wB	�cB	�cB	�iB	�iB	�B	�oB	�oB	�B	�vB	�|B	�|B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
�B
B
B
B
�B
	B

#B

#B

#B
)B
B
0B
0B
B
B
B
B
B
"B
<B
(B
(B
(B
(B
BB
(B
HB
.B
.B
4B
4B
4B
4B
:B
:B
FB
aB
MB
gB
gB
mB
SB
SB
mB
YB
sB
sB
YB
mB
SB
SB
yB
B
B
eB
B
kB
eB
B
�B
kB
kB
kB
�B
kB
kB
qB
�B
�B
xB
xB
xB
~B
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
!�B
!�B
"�B
"�B
"�B
"�B
$�B
$�B
$�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
(�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
,�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
/�B
1B
1B
0�B
0�B
2B
2B
2�B
3B
4B
4B
4B
4B
4B
5B
6+B
6B
6+B
6B
6+B
6B
6B
7B
8B
8B
8B
88B
8B
8B
88B
88B
9$B
9>B
9>B
:*B
:*B
:DB
:*B
:DB
:*B
;JB
;0B
;0B
;0B
<6B
=VB
=VB
=VB
=<B
=VB
=<B
=<B
>BB
>]B
>]B
>BB
?cB
?HB
?cB
?cB
?cB
@iB
@OB
@OB
AUB
AoB
AUB
AoB
AUB
B[B
BuB
BuB
BuB
BuB
B[B
BuB
B[B
BuB
C{B
CaB
CaB
C{B
DgB
DgB
E�B
E�B
FtB
FtB
FtB
FtB
F�B
GzB
GzB
G�B
GzB
GzB
H�B
H�B
H�B
H�B
I�B
I�B
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
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
T�B
T�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
ZB
Y�B
Y�B
Y�B
ZB
Y�B
ZB
Y�B
[	B
Z�B
Z�B
[	B
[	B
[	B
Z�B
Z�B
[	B
[	B
Z�B
[	B
[�B
\B
\B
\B
[�B
\B
[�B
\B
\B
\�B
]B
\�B
]B
^B
^B
^B
^B
^B
^B
_!B
_B
_!B
`B
`'B
`B
`B
`B
`'B
aB
a-B
a-B
a-B
aB
aB
a-B
aB
b4B
bB
b4B
b4B
bB
bB
b4B
b4B
c:B
c:B
c B
c:B
c:B
c B
d@B
d&B
d&B
d&B
d@B
d&B
d&B
d&B
d&B
eFB
e,B
eFB
e,B
fLB
f2B
f2B
fLB
f2B
fLB
f2B
gRB
gRB
g8B
h>B
h>B
hXB
iDB
i_B
iDB
iDB
jeB
jeB
kQB
jKB
kkB
kkB
kkB
kkB
kQB
kQB
kQB
kQB
kQB
lqB
lqB
lWB
lqB
lWB
lWB
m]B
mwB
mwB
mwB
m]B
n}B
n}B
ncB
ncB
n}B
n}B
ncB
ncB
n}B
n}B
oiB
n}B
ncB
ncB
ncB
oiB
poB
p�B
p�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.32(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809200040552018092000405520180920004055201809210035382018092100353820180921003538JA  ARFMdecpA19c                                                                20180915063512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180914213524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180914213525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180914213525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180914213526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180914213526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180914213526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180914213526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180914213526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180914213527                      G�O�G�O�G�O�                JA  ARUP                                                                        20180914215549                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180915155258  CV  JULD            G�O�G�O�F�q                JM  ARGQJMQC2.0                                                                 20180915155258  CV  JULD_LOCATION   G�O�G�O�F�~                JM  ARCAJMQC2.0                                                                 20180919154055  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180919154055  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180920153538  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021523                      G�O�G�O�G�O�                