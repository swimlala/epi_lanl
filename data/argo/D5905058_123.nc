CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-02-07T00:36:37Z creation;2019-02-07T00:36:41Z conversion to V3.1;2019-12-23T06:07:23Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190207003637  20200120021523  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               {A   JA  I2_0675_123                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @إ�R}( 1   @إ�`� @7�z�G��c5333331   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@���A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ Dü�D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ DƼ�D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�C3Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�p�@�
=A�A%�AE�Ae�A��\A�\)A��\A��\A\Aҏ\A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,k�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>k�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�D {D �{DD�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dx{Dx�{Dy{Dy�{Dz{Dz�{D{{D{�{D|{D|�{D}{D}�{D~{D~�{D{D�{D�
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
=D�J=D��
D��=D�
=D�J=D��=D��=D�
D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��qD�
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
=D�J=D��qD��=D�
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
=D�J=DÊ=D��
D�
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
=D�MqDϊ=D��=D�
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
=D�J=D��=D��=D�
=D�Mq1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨA���A���A���A���A��`A��`A��A���A���A���A�  A�A�1A�JA�bA�bA�bA�bA�bA�bA�JA�JA�VA�JA�JA�JA�JA�
=A�%A�%A�%A�%A�%A�1A�
=A�
=A�
=A�JA�VA�bA�oA�{A�{A��A��A��A��A��A�$�A�$�A�oA��A��PA��A��uA��A�|�A��A��-A�^5A�&�A���A�`BA�ĜA�M�A�?}A�ffA�A�A�A��A��wA�bNA�G�A�7LA��9A�dZA�-A�{A��A��^A���A��PA�;dA���A��A��TA�ĜA�`BA��A��HA�p�A��FA��\A���A�x�A�;dA���A�;dA�=qA�~�A��\A��yA�~�A�
=A�^5A���A��jA��A��A�+A���A�-A�t�A���A�(�A��-A�VA��A�33A��TA���A��hA��uA��DA�l�A|�Ay�^Ax�uAw�#Aw&�AsApbNAn�AjbNAgx�Ae�^Ac��Aa�^A]33AY�wAW�AU�#AT �ASdZAR�`AQ�;AQ��AQ+AP��AP1'AO�-AM��AL �AI�AG�;AGp�AG+AFz�AE�TAEdZAD�9AD�ABA�A@r�A?C�A>5?A;�#A;33A:��A8�RA7&�A6�uA4ȴA2��A1�A/��A.��A-XA,�jA+�-A*�A*�A)|�A)%A'�PA&�A&�DA&A$�A"1'A �/A7LAM�A`BAG�A|�A��A �A�AoA�PA��AȴA��AoA�!A|�A��AI�A�
A��A�A
ȴA
M�A	�FA	+A1'AXA~�A�A+A�A��AbNA"�A�-AO�A ��@�S�@���@��^@��@�-@�G�@��@���@���@�7L@���@�ȴ@��@�dZ@�7@�/@�V@�Z@�^5@�`B@��@��/@�I�@�b@�"�@���@�V@���@�Q�@��@��@�O�@�Z@�l�@�^5@��@ѩ�@�v�@�V@Ѻ^@��/@ϥ�@�^5@�|�@�S�@ʸR@�7L@ț�@ǝ�@���@�@�V@ċD@�b@�ƨ@�33@�$�@��7@�1'@���@��@�?}@���@���@��@�`B@��`@���@���@��u@�1@�;d@��@�`B@�j@�t�@���@��!@�v�@���@�Ĝ@�ƨ@��@�dZ@��y@��@�&�@� �@�+@���@�V@�{@�J@�@��^@�p�@��@��@��u@�z�@�A�@�dZ@��H@��!@�M�@��#@�V@��@�9X@��;@���@���@���@�K�@�
=@��y@��!@���@��@���@�x�@�O�@�V@��/@��@�I�@��@���@�l�@�33@���@���@�=q@���@���@�hs@�7L@�V@��/@���@��@�Z@���@��F@�|�@�\)@��@��@�ff@��@��@�@�O�@�Ĝ@�I�@��@���@�33@��H@���@�v�@��@�x�@�/@�V@���@��@���@���@��@��@��@��9@�z�@�A�@� �@��@��@��@��;@��;@�ƨ@�t�@�K�@�t�@�\)@�+@���@�-@�-@�E�@�=q@�M�@�^5@���@���@�ff@�5?@�5?@��-@�G�@�O�@�/@���@���@��@���@��`@���@�Ĝ@��@�j@�I�@�Q�@���@�o@��@��@�l�@�\)@�K�@�S�@���@��@���@��T@�7L@��j@���@��D@�z�@�I�@��;@��w@��@�\)@�;d@�o@��y@��H@�ȴ@�v�@�M�@�5?@��@�J@�@�J@���@���@��7@�x�@�O�@�?}@��@��/@��9@�z�@�Z@�Z@�I�@�9X@�1'@�(�@� �@��@���@���@���@�dZ@�+@�@���@��+@�^5@�@��@��^@�hs@�G�@�&�@�V@��/@�1@�1@�ƨ@�|�@�S�@�;d@�+@�o@��y@���@��+@��@���@���@�p�@�G�@���@��@��/@��9@��@�(�@�@K�@
=@~�@~��@~�+@~E�@}�T@}p�@}V@|z�@{�F@{33@{o@z�!@z^5@y��@yG�@y&�@y%@x�u@xA�@w��@w\)@v�@vff@v$�@u��@t�/@t9X@s��@s"�@r��@r-@q�#@qhs@p��@pA�@o�P@oK�@n�@nv�@m�@m?}@l��@l�/@l�j@lz�@lI�@k�m@k�@kC�@ko@j�H@j��@j^5@i��@iG�@i%@h��@h��@h�@hQ�@h  @g�@g�;@g�w@g�P@gK�@f��@f$�@e��@e@e��@e`B@e�@d�/@d��@dI�@d1@c��@b�@bn�@b-@a��@a��@a�7@ahs@aG�@`��@`��@`b@_�@_;d@_+@_;d@_+@_�@^��@^{@]�-@]O�@\z�@[�
@[dZ@[o@[@Z�H@Z��@Zn�@Y��@Yhs@X��@X��@XĜ@X�9@X��@XQ�@W�@W�P@V�y@U�@T��@T�j@TI�@T�@S��@S�
@S�@S"�@R��@R=q@Q�^@Q7L@P��@P  @O��@OK�@O�@Nȴ@Nv�@M�T@M�h@MV@L�D@Lj@L�@Kƨ@K��@Kt�@K33@J��@J�\@J~�@J�@I�#@I�^@Ix�@IG�@H��@HĜ@H��@H�@G�@G�w@G|�@Gl�@Gl�@G�@F��@F$�@E�h@E`B@E?}@D��@DZ@D(�@C�F@C"�@B�H@B^5@BM�@A�^@A��@A&�@@�9@@�u@@A�@?�@?��@?�@?�P@?|�@?l�@?+@>�@>�+@>E�@=��@=�h@=O�@=/@<��@<z�@<�@;ƨ@;��@;@:�@:�H@:��@:�!@:~�@:=q@9��@9�^@9x�@9X@9�@8�@8r�@8b@7�@7|�@7\)@7K�@7
=@6��@6V@5�@5@5��@5/@4��@4�@4j@4Z@4I�@4(�@4�@3��@3ƨ@3��@3dZ@333@3o@2�H@2��@2�\@2^5@2J@1�^@1%@0��@0��@0�u@0�@0Q�@0  @/��@/�w@/l�@/\)@/+@.��@.v�@.$�@.@-�T@-��@-`B@-�@-V@,��@,��@,��@,Z@,(�@+��@+�m@+�@+33@+@*��@*��@*^5@*=q@*�@)�@)�7@)7L@)&�@)�@)%@(��@(Q�@( �@'�@'�w@'��@'|�@';d@&��@&�@&�+@&E�@&@%�T@%��@%�@%?}@$�/@$��@$�D@$9X@#��@#ƨ@#��@#�@#S�@"�H@"��@"^5@"�@!��@!hs@!�@ ��@ ��@ �9@ �u@ �@ Q�@  �@   @��@�@�@��@|�@K�@��@�y@ȴ@�R@��@5?@5?@{@�@��@�-@�h@�@p�@`B@?}@/@�@�@�D@j@Z@9X@�m@�
@ƨ@ƨ@�F@�F@��@C�@o@��@^5@J@��@��@hs@G�@7L@&�@Ĝ@bN@A�@1'@ �@  @�;@�@l�@+@�y@ȴ@��@�+@E�@@�-@p�@/@�@��@�/@��@��@�D@9X@��@ƨ@��@�@dZ@S�@C�@33@��@�!@�\@M�@�@�@�@�@-@�@�@��@�#@��@�7@hs@7L@�@%@�`@��@r�@1'@b@�@�P@K�@
=@�@ȴ@��@ff@V@5?@5?@$�@�@@p�@/@�@V@�/@�j@z�@Z@�@�m@t�@t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨA���A���A���A���A��`A��`A��A���A���A���A�  A�A�1A�JA�bA�bA�bA�bA�bA�bA�JA�JA�VA�JA�JA�JA�JA�
=A�%A�%A�%A�%A�%A�1A�
=A�
=A�
=A�JA�VA�bA�oA�{A�{A��A��A��A��A��A�$�A�$�A�oA��A��PA��A��uA��A�|�A��A��-A�^5A�&�A���A�`BA�ĜA�M�A�?}A�ffA�A�A�A��A��wA�bNA�G�A�7LA��9A�dZA�-A�{A��A��^A���A��PA�;dA���A��A��TA�ĜA�`BA��A��HA�p�A��FA��\A���A�x�A�;dA���A�;dA�=qA�~�A��\A��yA�~�A�
=A�^5A���A��jA��A��A�+A���A�-A�t�A���A�(�A��-A�VA��A�33A��TA���A��hA��uA��DA�l�A|�Ay�^Ax�uAw�#Aw&�AsApbNAn�AjbNAgx�Ae�^Ac��Aa�^A]33AY�wAW�AU�#AT �ASdZAR�`AQ�;AQ��AQ+AP��AP1'AO�-AM��AL �AI�AG�;AGp�AG+AFz�AE�TAEdZAD�9AD�ABA�A@r�A?C�A>5?A;�#A;33A:��A8�RA7&�A6�uA4ȴA2��A1�A/��A.��A-XA,�jA+�-A*�A*�A)|�A)%A'�PA&�A&�DA&A$�A"1'A �/A7LAM�A`BAG�A|�A��A �A�AoA�PA��AȴA��AoA�!A|�A��AI�A�
A��A�A
ȴA
M�A	�FA	+A1'AXA~�A�A+A�A��AbNA"�A�-AO�A ��@�S�@���@��^@��@�-@�G�@��@���@���@�7L@���@�ȴ@��@�dZ@�7@�/@�V@�Z@�^5@�`B@��@��/@�I�@�b@�"�@���@�V@���@�Q�@��@��@�O�@�Z@�l�@�^5@��@ѩ�@�v�@�V@Ѻ^@��/@ϥ�@�^5@�|�@�S�@ʸR@�7L@ț�@ǝ�@���@�@�V@ċD@�b@�ƨ@�33@�$�@��7@�1'@���@��@�?}@���@���@��@�`B@��`@���@���@��u@�1@�;d@��@�`B@�j@�t�@���@��!@�v�@���@�Ĝ@�ƨ@��@�dZ@��y@��@�&�@� �@�+@���@�V@�{@�J@�@��^@�p�@��@��@��u@�z�@�A�@�dZ@��H@��!@�M�@��#@�V@��@�9X@��;@���@���@���@�K�@�
=@��y@��!@���@��@���@�x�@�O�@�V@��/@��@�I�@��@���@�l�@�33@���@���@�=q@���@���@�hs@�7L@�V@��/@���@��@�Z@���@��F@�|�@�\)@��@��@�ff@��@��@�@�O�@�Ĝ@�I�@��@���@�33@��H@���@�v�@��@�x�@�/@�V@���@��@���@���@��@��@��@��9@�z�@�A�@� �@��@��@��@��;@��;@�ƨ@�t�@�K�@�t�@�\)@�+@���@�-@�-@�E�@�=q@�M�@�^5@���@���@�ff@�5?@�5?@��-@�G�@�O�@�/@���@���@��@���@��`@���@�Ĝ@��@�j@�I�@�Q�@���@�o@��@��@�l�@�\)@�K�@�S�@���@��@���@��T@�7L@��j@���@��D@�z�@�I�@��;@��w@��@�\)@�;d@�o@��y@��H@�ȴ@�v�@�M�@�5?@��@�J@�@�J@���@���@��7@�x�@�O�@�?}@��@��/@��9@�z�@�Z@�Z@�I�@�9X@�1'@�(�@� �@��@���@���@���@�dZ@�+@�@���@��+@�^5@�@��@��^@�hs@�G�@�&�@�V@��/@�1@�1@�ƨ@�|�@�S�@�;d@�+@�o@��y@���@��+@��@���@���@�p�@�G�@���@��@��/@��9@��@�(�@�@K�@
=@~�@~��@~�+@~E�@}�T@}p�@}V@|z�@{�F@{33@{o@z�!@z^5@y��@yG�@y&�@y%@x�u@xA�@w��@w\)@v�@vff@v$�@u��@t�/@t9X@s��@s"�@r��@r-@q�#@qhs@p��@pA�@o�P@oK�@n�@nv�@m�@m?}@l��@l�/@l�j@lz�@lI�@k�m@k�@kC�@ko@j�H@j��@j^5@i��@iG�@i%@h��@h��@h�@hQ�@h  @g�@g�;@g�w@g�P@gK�@f��@f$�@e��@e@e��@e`B@e�@d�/@d��@dI�@d1@c��@b�@bn�@b-@a��@a��@a�7@ahs@aG�@`��@`��@`b@_�@_;d@_+@_;d@_+@_�@^��@^{@]�-@]O�@\z�@[�
@[dZ@[o@[@Z�H@Z��@Zn�@Y��@Yhs@X��@X��@XĜ@X�9@X��@XQ�@W�@W�P@V�y@U�@T��@T�j@TI�@T�@S��@S�
@S�@S"�@R��@R=q@Q�^@Q7L@P��@P  @O��@OK�@O�@Nȴ@Nv�@M�T@M�h@MV@L�D@Lj@L�@Kƨ@K��@Kt�@K33@J��@J�\@J~�@J�@I�#@I�^@Ix�@IG�@H��@HĜ@H��@H�@G�@G�w@G|�@Gl�@Gl�@G�@F��@F$�@E�h@E`B@E?}@D��@DZ@D(�@C�F@C"�@B�H@B^5@BM�@A�^@A��@A&�@@�9@@�u@@A�@?�@?��@?�@?�P@?|�@?l�@?+@>�@>�+@>E�@=��@=�h@=O�@=/@<��@<z�@<�@;ƨ@;��@;@:�@:�H@:��@:�!@:~�@:=q@9��@9�^@9x�@9X@9�@8�@8r�@8b@7�@7|�@7\)@7K�@7
=@6��@6V@5�@5@5��@5/@4��@4�@4j@4Z@4I�@4(�@4�@3��@3ƨ@3��@3dZ@333@3o@2�H@2��@2�\@2^5@2J@1�^@1%@0��@0��@0�u@0�@0Q�@0  @/��@/�w@/l�@/\)@/+@.��@.v�@.$�@.@-�T@-��@-`B@-�@-V@,��@,��@,��@,Z@,(�@+��@+�m@+�@+33@+@*��@*��@*^5@*=q@*�@)�@)�7@)7L@)&�@)�@)%@(��@(Q�@( �@'�@'�w@'��@'|�@';d@&��@&�@&�+@&E�@&@%�T@%��@%�@%?}@$�/@$��@$�D@$9X@#��@#ƨ@#��@#�@#S�@"�H@"��@"^5@"�@!��@!hs@!�@ ��@ ��@ �9@ �u@ �@ Q�@  �@   @��@�@�@��@|�@K�@��@�y@ȴ@�R@��@5?@5?@{@�@��@�-@�h@�@p�@`B@?}@/@�@�@�D@j@Z@9X@�m@�
@ƨ@ƨ@�F@�F@��@C�@o@��@^5@J@��@��@hs@G�@7L@&�@Ĝ@bN@A�@1'@ �@  @�;@�@l�@+@�y@ȴ@��@�+@E�@@�-@p�@/@�@��@�/@��@��@�D@9X@��@ƨ@��@�@dZ@S�@C�@33@��@�!@�\@M�@�@�@�@�@-@�@�@��@�#@��@�7@hs@7L@�@%@�`@��@r�@1'@b@�@�P@K�@
=@�@ȴ@��@ff@V@5?@5?@$�@�@@p�@/@�@V@�/@�j@z�@Z@�@�m@t�@t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BcTBhsBm�BiyBffB}�B|�B�DB�\B�hB�oB�uB�{B��B��B��B��B��B��B�B�B�B�'B�9B�?B�?B�FB�LB�XB�dB�jB�jB�jB�qB�qB�qB�wB�qB�qB�wB�}B��BBBBÖBBÖBĜBȴB��B�)B�HB�mB�B�B��B��B��B��B��B�B�B�B�B��B��B��BDB
=B
=BDB
=B	7B1B
=B
=B1B	7B	7B
=B
=B
=BDBDB
=B
=B
=BVBbBoBhBVBB��B�B�)BǮB�-B��B��B�+B|�Bv�Bq�BhsBbNB]/BR�BB�B-B%�B#�B�B  B
�HB
�)B
ɺB
ÖB
�LB
�!B
��B
��B
�B
jB
0!B
DB	��B	�B	�NB	�B	ƨB	�B	��B	~�B	gmB	ZB	I�B	9XB	�B	  B��B�B�fB�NB�BB�#B�B�B�B��B��BǮB�wB��B�XB�LB�RB�jB�}B�qB�dB�RB�'B��B��B��B��B��B��B�\B�%B� Bu�Bk�BdZBbNBbNB\)B[#BXBW
BYBZB[#B]/BZBYBXBT�BQ�BJ�BJ�BG�BD�BC�B@�B=qB=qB<jB:^B:^B8RB6FB6FB5?B49B49B33B33B33B2-B1'B1'B0!B/B/B/B.B.B,B,B+B+B)�B+B'�B'�B'�B'�B)�B&�B&�B(�B&�B(�B&�B&�B)�B'�B'�B)�B)�B.B7LB9XB9XB;dB9XB9XB:^B<jBE�BG�BG�BH�BJ�BJ�BC�B<jB9XB<jB=qBA�B@�B@�BF�BI�BH�BF�BH�BP�BI�BK�BL�BK�BI�BI�BI�BM�BO�BP�BP�BQ�BT�BXBZB\)B^5B^5B`BBaHBaHBcTBffBk�Bl�Bm�Bq�Bu�Bw�Bz�By�Bw�By�B|�B|�B~�B�B�JB�VB�PB�PB�PB�VB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�3B�9B�LB�XB�}BÖBŢBǮB��B��B��B��B��B�B�B�B�5B�;B�TB�fB�B�B�B��B��B��B	B	B	
=B	\B	{B	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	$�B	'�B	)�B	,B	.B	1'B	1'B	2-B	2-B	6FB	8RB	=qB	?}B	A�B	D�B	E�B	E�B	F�B	I�B	L�B	O�B	Q�B	Q�B	S�B	W
B	YB	[#B	\)B	]/B	^5B	_;B	_;B	aHB	bNB	bNB	dZB	ffB	hsB	iyB	jB	k�B	n�B	q�B	s�B	u�B	v�B	z�B	|�B	}�B	� B	�B	�B	�+B	�7B	�DB	�PB	�\B	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�?B	�FB	�LB	�RB	�RB	�RB	�RB	�LB	�RB	�jB	�jB	�qB	�qB	�wB	�wB	�}B	��B	ÖB	ŢB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�`B	�fB	�fB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B
	7B

=B

=B

=B
DB
JB
JB
PB
PB
VB
\B
\B
\B
\B
\B
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
,B
,B
,B
,B
,B
-B
-B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
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
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
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
I�B
I�B
I�B
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
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
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
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
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
\)B
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
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
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
e`B
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
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
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
m�B
m�B
m�B
m�B
m�B
n�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bc:BhXBmwBi_BfLB}�B|�B�)B�BB�NB�TB�[B�aB�yB��B��B��B��B��B��B��B��B�B�B�%B�%B�+B�2B�>B�JB�PB�PB�PB�VB�VB�VB�]B�VB�VB�]B�cB�oB�uB�uB�uB�{B�uB�{BāBȚB͹B�B�-B�RB�kB�B��B��B��B��B��B�B��B�wB�kB��B��B��B)B
#B
#B)B
#B	BB
#B
#BB	B	B
#B
#B
#B)B)B
#B
#B
#B<BHBTBNB<BB��B�kB�BǔB��B��B�gB��B|�Bv�Bq�BhXBb4B]BR�BB[B,�B%�B#�B�B
��B
�-B
�B
ɠB
�{B
�2B
�B
��B
�B
��B
jKB
0B
)B	��B	�kB	�4B	��B	ƎB	��B	��B	~�B	g8B	ZB	I�B	9>B	�B��B��B�}B�LB�B�'B�	B�B��B��B��B��BǔB�]B�iB�>B�2B�8B�PB�HB�<B�0B�8B�B��B��B�~B�B��B��B�BB�B�Bu�BkkBd@BbBb4B[�B[	BW�BV�BX�BZB[	B]BY�BX�BW�BT�BQ�BJ�BJ�BG�BDgBC{B@iB=<B=<B<PB:*B:DB88B6B6B5%B4B4B3B2�B3B1�B0�B1B/�B.�B/ B/ B-�B-�B+�B+�B*�B*�B)�B*�B'�B'�B'�B'�B)�B&�B&�B(�B&�B(�B&�B&�B)�B'�B'�B)�B)�B-�B7B9$B9>B;JB9>B9>B:*B<6BEmBG�BG�BH�BJ�BJ�BCaB<PB9>B<6B=VBAoB@iB@iBF�BI�BH�BFtBH�BP�BI�BK�BL�BK�BI�BI�BI�BM�BO�BP�BP�BQ�BT�BW�BZB[�B^B^B`'Ba-BaBc:Bf2BkkBlqBm]Bq�Bu�Bw�Bz�By�Bw�By�B|�B|�B~�B�B�B�<B�B�6B�6B�"B�.B�aB�SB�eB�xB��B��B��B��B��B��B��B��B��B��B��B��B�B�2B�$B�HB�aB�mBǔBʦB̳BбB��B��B��B��B��B�B�!B�:B�2B�QB�iB�B��B��B��B	 �B	�B	
	B	(B	FB	FB	SB	YB	_B	eB	B	xB	 �B	!�B	"�B	$�B	'�B	)�B	+�B	-�B	0�B	1B	2B	1�B	6B	8B	=<B	?cB	AoB	D�B	EmB	EmB	F�B	I�B	L�B	O�B	Q�B	Q�B	S�B	V�B	X�B	[	B	[�B	\�B	^B	_B	_!B	aB	bB	b4B	d@B	fLB	hXB	iDB	jeB	kkB	n}B	q�B	s�B	u�B	v�B	z�B	|�B	}�B	�B	��B	�B	�B	�B	�)B	�6B	�BB	�BB	�:B	�FB	�mB	�yB	�kB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�%B	�+B	�B	�8B	�8B	�B	�8B	�B	�B	�PB	�PB	�VB	�<B	�BB	�BB	�HB	�oB	�{B	ňB	�tB	�zB	ɆB	ɠB	ʦB	ʦB	˒B	̘B	͹B	ΥB	ΥB	οB	бB	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�B	�B	� B	�:B	� B	�@B	�@B	�,B	�2B	�2B	�RB	�RB	�8B	�XB	�KB	�WB	�WB	�wB	�wB	�wB	�cB	�B	�B	�oB	��B	��B	��B	�oB	�vB	�|B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
B
B
B
B
�B
	B
	B

#B

	B

	B
)B
0B
0B
6B
B
<B
(B
BB
(B
(B
(B
HB
.B
4B
NB
NB
NB
NB
NB
TB
:B
[B
[B
[B
[B
FB
FB
FB
aB
MB
gB
MB
mB
YB
YB
YB
sB
_B
yB
yB
_B
B
eB
eB
kB
kB
kB
kB
�B
qB
xB
xB
xB
�B
qB
xB
xB
~B
~B
~B
�B
�B
�B
�B
�B
 �B
!�B
!�B
"�B
"�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
*�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
-�B
-�B
/ B
/ B
.�B
0B
0B
0B
0B
1B
0�B
1B
0�B
1B
2B
2B
2B
1�B
3B
2�B
2�B
3B
4B
4B
4B
4B
4B
5%B
5%B
6B
6B
6B
6B
7B
7B
88B
88B
9$B
9>B
9$B
9$B
9$B
:*B
;JB
;0B
;0B
<PB
<6B
<6B
<6B
<PB
<6B
<6B
=<B
=VB
=VB
>BB
>BB
>]B
>BB
>BB
>BB
?HB
?HB
?HB
@iB
@OB
@iB
@OB
@OB
@OB
@OB
@iB
@iB
AUB
AUB
AUB
B[B
BuB
B[B
CaB
CaB
CaB
C{B
C{B
D�B
DgB
DgB
DgB
D�B
EmB
E�B
EmB
E�B
EmB
FtB
F�B
FtB
FtB
FtB
F�B
GzB
GzB
GzB
GzB
GzB
G�B
G�B
H�B
H�B
I�B
I�B
I�B
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
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
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
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
ZB
ZB
Y�B
ZB
Y�B
ZB
ZB
Z�B
[	B
Z�B
[	B
Z�B
Z�B
Z�B
[�B
[�B
\B
[�B
[�B
\B
[�B
[�B
[�B
[�B
]B
\�B
]B
]B
]B
]B
^B
^B
^B
^B
^B
^B
^B
^B
^B
^B
_B
_!B
`B
`B
`B
`'B
`B
`'B
a-B
aB
aB
aB
bB
b4B
bB
bB
bB
b4B
c B
c:B
c:B
c B
c B
c:B
d&B
d&B
d@B
d&B
d@B
d&B
eFB
e,B
e,B
eFB
eFB
f2B
fLB
f2B
f2B
f2B
fLB
f2B
g8B
gRB
g8B
gRB
g8B
gRB
gRB
gRB
g8B
hXB
hXB
hXB
h>B
h>B
hXB
h>B
h>B
h>B
h>B
iDB
h>B
iDB
i_B
iDB
i_B
jeB
jeB
jKB
jKB
jKB
kQB
kQB
kkB
kQB
kQB
kkB
kkB
kQB
lWB
lWB
lqB
lWB
lqB
mwB
mwB
m]B
mwB
mwB
n}B
nc1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.32(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201902120042442019021200424420190212004244201902130025472019021300254720190213002547JA  ARFMdecpA19c                                                                20190207093635  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190207003637  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190207003639  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190207003639  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190207003640  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190207003640  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190207003640  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190207003640  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190207003641  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190207003641                      G�O�G�O�G�O�                JA  ARUP                                                                        20190207005724                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190207153454  CV  JULD            G�O�G�O�F�-�                JM  ARCAJMQC2.0                                                                 20190211154244  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190211154244  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190212152547  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021523                      G�O�G�O�G�O�                