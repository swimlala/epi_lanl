CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-14T00:36:19Z creation;2018-05-14T00:36:22Z conversion to V3.1;2019-12-23T06:21:59Z update;     
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
resolution        =���   axis      Z        l  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  st   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  �L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ې   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180514003619  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               <A   JA  I2_0675_060                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�bxZt�1   @�by�[ @7���ڹ��b-�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D	y�D	��D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�=qA�A%�AE�Ae�A��\A��\A��\A��\A\Aҏ\A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	D	�D
D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL��DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dx{Dx�{Dy{Dy�{Dz{Dz�{D{{D{�{D|{D|�{D}{D}�{D~{D~�{D{D�{D�
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
=D�J=DƊ=D��=D�
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
=D�J=D��=D��qD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��9A��\A�x�A�`BA�O�A�?}A�33A�&�A�"�A�oA���A��HA��-A���A��hA��7A��A�r�A��A��A���A�dZA�bNA�K�A�=qA��A��TA���A�n�A�"�A���A���A��TA��/A�hsA�/A�"�A�A��RA�l�A�9XA�
=A�l�A��A� �A�ȴA�jA�(�A�VA���A��;A���A��A���A�K�A���A��DA�+A���A���A�x�A�A���A�v�A�ZA�JA��mA�?}A�/A�I�A�ZA�;dA�v�A�K�A�7LA���A�%A�l�A���A�A�1'A�VA�ffA��A��A��TA�A�l�A���A�G�A�A�VA��A��;A��yA�9XA��
A���A���A�t�A�hsA�9XA�(�A�A��9A�I�A�t�A��#A~�9A|��Az�`Ax�+Av�At�yAr��Aq�mAo�
AnbAmAj�9Af�Ae
=Ad9XA`�A^�A\�A[?}AZ �AYAW;dAUp�AT�AS�hAQ�AOANjAMp�AL��AK�AI��AH �AF��ADQ�AC�PABjAAhsA@ĜA>��A=�7A=oA<��A;�FA:M�A9�-A8��A6�A6{A5�A4VA3�A1�A1S�A0�jA0�A/VA.�`A.ȴA.A�A,�RA+�A*�`A*n�A*(�A(�yA(�A&ffA$A#|�A!��A!K�A ��A;dA�A1A��A�mA�HA�wA��A�A�A1'A?}A�A��Al�A��A9XA;dA9XA�hAoA	��A�AM�AAt�A��A�mA�A�A�jA��A��A��A1'A �HA $�@���@��D@���@�@���@�(�@�33@�b@��y@�&�@�A�@�!@��-@��@�+@���@陚@��@�w@��@��@�@�hs@��@��;@�-@�Q�@۶F@�t�@ڗ�@��`@պ^@�bN@ӕ�@��@���@җ�@с@�j@�1'@��@ϥ�@��@Η�@�{@́@�Z@˝�@���@�z�@��@�v�@���@�G�@� �@�|�@�E�@��T@���@�?}@���@��@�$�@��@���@�j@�l�@��@��@�%@�z�@�Z@� �@���@�E�@�p�@�Ĝ@��u@�9X@��
@��@�C�@���@��\@�%@�C�@��y@���@���@�\)@�v�@��#@��@��m@�;d@�=q@�@��-@�&�@�z�@�b@�|�@���@�n�@�@��@��@���@��@��@��@�Z@� �@��@���@�E�@�@��@�%@�9X@�;d@��R@���@�@���@�bN@�(�@�I�@�j@���@���@���@��@���@���@��@��j@��@�bN@�A�@�(�@��m@�\)@��\@���@�{@�p�@���@���@���@���@�  @��@��+@�ff@�M�@�E�@�M�@�M�@�~�@�"�@�~�@�E�@���@�?}@��j@�A�@��@�  @��;@���@��@�K�@�
=@��R@�v�@�{@��-@�hs@�O�@�7L@�V@��/@��u@�Z@��@��
@���@�|�@�S�@�"�@�
=@���@�~�@�ff@�M�@�=q@�$�@���@��-@��-@��-@���@��7@�hs@�O�@�G�@��@��@���@���@�Ĝ@���@�j@�j@�Z@�A�@��m@��
@���@���@�^5@�M�@�{@��T@���@��@�O�@�7L@��@�%@��9@��@�j@�Z@�I�@�9X@�1@���@�t�@�\)@�33@�@���@�v�@�M�@�$�@���@���@��h@�`B@��@�V@���@��/@�Ĝ@��u@�b@��;@��@�\)@�;d@�o@���@���@���@��!@��\@�v�@�-@���@�/@��@���@���@��j@�r�@�9X@��m@��P@�\)@��@���@�E�@��@���@���@�X@��@���@���@���@�Z@�A�@�9X@� �@��@�w@+@
=@~v�@}�@}@}p�@}/@|�/@|Z@{�m@{t�@{S�@{33@z��@zn�@zJ@y��@y��@y��@y��@y&�@y%@y%@x��@xbN@xb@w��@wl�@v�y@v�R@v�+@vE�@uO�@uV@t�/@tZ@t9X@t1@s�@s33@r��@rM�@r�@rJ@q��@q&�@p�`@p��@p�@o�P@n�y@nv�@m@mO�@l�@l�@l�@l��@k�F@kdZ@j�@j��@j��@j�!@j��@jJ@ix�@h��@h�u@h�@g�;@g\)@g\)@gK�@g�@f��@f��@fv�@fff@fV@f5?@ep�@d��@d�D@dI�@d1@c�F@c��@c��@c�@c33@b��@b�\@bM�@b=q@b-@bJ@a��@a�#@a�7@a7L@a%@`��@`�@_�@_l�@_
=@^��@^��@^V@]�h@\��@\��@\Z@[�m@[@Z=q@Z�@Y��@Y&�@XA�@V�y@V��@U��@U�h@U�h@U�T@U�@U�@UV@Tz�@T�@Sƨ@S�@SS�@S33@R��@R��@RM�@RJ@Q��@Q��@QG�@P��@P�`@P��@Q7L@QG�@QX@QX@Q%@PĜ@P �@O�P@N��@N@M�-@M`B@M/@L�/@LI�@K��@K@Jn�@J-@I�^@I�7@I7L@H��@H�@HbN@H1'@G�;@G�w@G�@G��@G��@G��@G��@G+@F�@F�+@FV@F{@E�-@E`B@EO�@E?}@E?}@E?}@E/@EV@D��@D�/@D��@D9X@C�
@C�@B�@B�!@B^5@B-@B�@B�@BJ@A��@A�@A��@AG�@@��@@�9@@�@@bN@?�@?�w@?�P@?K�@?+@?�@>��@>��@>{@=�@=��@=?}@<��@<�@<Z@<(�@<1@;ƨ@;��@;t�@;"�@:�!@:~�@:-@9��@9��@9x�@9X@97L@9�@8�@8Q�@8A�@8b@7�@7|�@7;d@7+@7;d@6$�@5�h@5O�@5�@4�@4��@4I�@4�@3�m@3�F@3dZ@2�H@2~�@2M�@2=q@2�@2J@1�#@1�7@1G�@1�@0��@0r�@0b@/�@/��@/��@/K�@/
=@.�y@.ȴ@.��@.ff@.{@.@-��@-��@-�@-O�@,�/@,�D@,Z@,I�@,1@+��@+"�@+@*�!@*��@*~�@*-@)�@)X@)�@(��@(Ĝ@(��@(r�@( �@'�@'�;@'��@'��@';d@'
=@&�@&�R@&��@&E�@%�T@%@%@%��@%�@%p�@%`B@%O�@%?}@%�@$�/@$z�@$(�@#�
@#��@#dZ@#"�@#o@#@"�!@"M�@"-@!��@!�#@!��@!��@!��@!��@!hs@!7L@!%@ ��@ �9@ ��@ r�@ A�@  �@|�@;d@+@
=@ȴ@��@v�@�@�h@O�@�@V@�@�/@�D@�@��@�
@ƨ@��@t�@dZ@S�@C�@@��@��@M�@J@�#@��@��@G�@&�@��@Ĝ@�9@�u@r�@Q�@1'@ �@b@��@|�@K�@
=@�y@�@ȴ@��@V@E�@$�@�T@�-@p�@V@�/@�j@��@j@Z@I�@(�@�@�m@��@t�@S�@"�@�H@^5@=q@J@�#@��@X@%@��@�u@bN@Q�@Q�@A�@1'@b@�@�@�P@\)@��@�y@��@�y@�y@�@5?@�@��@`B@/@�@��@��@��@�@I�@��@�F@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��9A��\A�x�A�`BA�O�A�?}A�33A�&�A�"�A�oA���A��HA��-A���A��hA��7A��A�r�A��A��A���A�dZA�bNA�K�A�=qA��A��TA���A�n�A�"�A���A���A��TA��/A�hsA�/A�"�A�A��RA�l�A�9XA�
=A�l�A��A� �A�ȴA�jA�(�A�VA���A��;A���A��A���A�K�A���A��DA�+A���A���A�x�A�A���A�v�A�ZA�JA��mA�?}A�/A�I�A�ZA�;dA�v�A�K�A�7LA���A�%A�l�A���A�A�1'A�VA�ffA��A��A��TA�A�l�A���A�G�A�A�VA��A��;A��yA�9XA��
A���A���A�t�A�hsA�9XA�(�A�A��9A�I�A�t�A��#A~�9A|��Az�`Ax�+Av�At�yAr��Aq�mAo�
AnbAmAj�9Af�Ae
=Ad9XA`�A^�A\�A[?}AZ �AYAW;dAUp�AT�AS�hAQ�AOANjAMp�AL��AK�AI��AH �AF��ADQ�AC�PABjAAhsA@ĜA>��A=�7A=oA<��A;�FA:M�A9�-A8��A6�A6{A5�A4VA3�A1�A1S�A0�jA0�A/VA.�`A.ȴA.A�A,�RA+�A*�`A*n�A*(�A(�yA(�A&ffA$A#|�A!��A!K�A ��A;dA�A1A��A�mA�HA�wA��A�A�A1'A?}A�A��Al�A��A9XA;dA9XA�hAoA	��A�AM�AAt�A��A�mA�A�A�jA��A��A��A1'A �HA $�@���@��D@���@�@���@�(�@�33@�b@��y@�&�@�A�@�!@��-@��@�+@���@陚@��@�w@��@��@�@�hs@��@��;@�-@�Q�@۶F@�t�@ڗ�@��`@պ^@�bN@ӕ�@��@���@җ�@с@�j@�1'@��@ϥ�@��@Η�@�{@́@�Z@˝�@���@�z�@��@�v�@���@�G�@� �@�|�@�E�@��T@���@�?}@���@��@�$�@��@���@�j@�l�@��@��@�%@�z�@�Z@� �@���@�E�@�p�@�Ĝ@��u@�9X@��
@��@�C�@���@��\@�%@�C�@��y@���@���@�\)@�v�@��#@��@��m@�;d@�=q@�@��-@�&�@�z�@�b@�|�@���@�n�@�@��@��@���@��@��@��@�Z@� �@��@���@�E�@�@��@�%@�9X@�;d@��R@���@�@���@�bN@�(�@�I�@�j@���@���@���@��@���@���@��@��j@��@�bN@�A�@�(�@��m@�\)@��\@���@�{@�p�@���@���@���@���@�  @��@��+@�ff@�M�@�E�@�M�@�M�@�~�@�"�@�~�@�E�@���@�?}@��j@�A�@��@�  @��;@���@��@�K�@�
=@��R@�v�@�{@��-@�hs@�O�@�7L@�V@��/@��u@�Z@��@��
@���@�|�@�S�@�"�@�
=@���@�~�@�ff@�M�@�=q@�$�@���@��-@��-@��-@���@��7@�hs@�O�@�G�@��@��@���@���@�Ĝ@���@�j@�j@�Z@�A�@��m@��
@���@���@�^5@�M�@�{@��T@���@��@�O�@�7L@��@�%@��9@��@�j@�Z@�I�@�9X@�1@���@�t�@�\)@�33@�@���@�v�@�M�@�$�@���@���@��h@�`B@��@�V@���@��/@�Ĝ@��u@�b@��;@��@�\)@�;d@�o@���@���@���@��!@��\@�v�@�-@���@�/@��@���@���@��j@�r�@�9X@��m@��P@�\)@��@���@�E�@��@���@���@�X@��@���@���@���@�Z@�A�@�9X@� �@��@�w@+@
=@~v�@}�@}@}p�@}/@|�/@|Z@{�m@{t�@{S�@{33@z��@zn�@zJ@y��@y��@y��@y��@y&�@y%@y%@x��@xbN@xb@w��@wl�@v�y@v�R@v�+@vE�@uO�@uV@t�/@tZ@t9X@t1@s�@s33@r��@rM�@r�@rJ@q��@q&�@p�`@p��@p�@o�P@n�y@nv�@m@mO�@l�@l�@l�@l��@k�F@kdZ@j�@j��@j��@j�!@j��@jJ@ix�@h��@h�u@h�@g�;@g\)@g\)@gK�@g�@f��@f��@fv�@fff@fV@f5?@ep�@d��@d�D@dI�@d1@c�F@c��@c��@c�@c33@b��@b�\@bM�@b=q@b-@bJ@a��@a�#@a�7@a7L@a%@`��@`�@_�@_l�@_
=@^��@^��@^V@]�h@\��@\��@\Z@[�m@[@Z=q@Z�@Y��@Y&�@XA�@V�y@V��@U��@U�h@U�h@U�T@U�@U�@UV@Tz�@T�@Sƨ@S�@SS�@S33@R��@R��@RM�@RJ@Q��@Q��@QG�@P��@P�`@P��@Q7L@QG�@QX@QX@Q%@PĜ@P �@O�P@N��@N@M�-@M`B@M/@L�/@LI�@K��@K@Jn�@J-@I�^@I�7@I7L@H��@H�@HbN@H1'@G�;@G�w@G�@G��@G��@G��@G��@G+@F�@F�+@FV@F{@E�-@E`B@EO�@E?}@E?}@E?}@E/@EV@D��@D�/@D��@D9X@C�
@C�@B�@B�!@B^5@B-@B�@B�@BJ@A��@A�@A��@AG�@@��@@�9@@�@@bN@?�@?�w@?�P@?K�@?+@?�@>��@>��@>{@=�@=��@=?}@<��@<�@<Z@<(�@<1@;ƨ@;��@;t�@;"�@:�!@:~�@:-@9��@9��@9x�@9X@97L@9�@8�@8Q�@8A�@8b@7�@7|�@7;d@7+@7;d@6$�@5�h@5O�@5�@4�@4��@4I�@4�@3�m@3�F@3dZ@2�H@2~�@2M�@2=q@2�@2J@1�#@1�7@1G�@1�@0��@0r�@0b@/�@/��@/��@/K�@/
=@.�y@.ȴ@.��@.ff@.{@.@-��@-��@-�@-O�@,�/@,�D@,Z@,I�@,1@+��@+"�@+@*�!@*��@*~�@*-@)�@)X@)�@(��@(Ĝ@(��@(r�@( �@'�@'�;@'��@'��@';d@'
=@&�@&�R@&��@&E�@%�T@%@%@%��@%�@%p�@%`B@%O�@%?}@%�@$�/@$z�@$(�@#�
@#��@#dZ@#"�@#o@#@"�!@"M�@"-@!��@!�#@!��@!��@!��@!��@!hs@!7L@!%@ ��@ �9@ ��@ r�@ A�@  �@|�@;d@+@
=@ȴ@��@v�@�@�h@O�@�@V@�@�/@�D@�@��@�
@ƨ@��@t�@dZ@S�@C�@@��@��@M�@J@�#@��@��@G�@&�@��@Ĝ@�9@�u@r�@Q�@1'@ �@b@��@|�@K�@
=@�y@�@ȴ@��@V@E�@$�@�T@�-@p�@V@�/@�j@��@j@Z@I�@(�@�@�m@��@t�@S�@"�@�H@^5@=q@J@�#@��@X@%@��@�u@bN@Q�@Q�@A�@1'@b@�@�@�P@\)@��@�y@��@�y@�y@�@5?@�@��@`B@/@�@��@��@��@�@I�@��@�F@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��BBB+B	7BPBhB�B"�B-B5?B8RBI�BffBt�Bx�Bx�By�B~�B� B�B� B�B�VB�oB�hB��B��B��B��B��B��B��B��B��B��B��B�!B�?B�jB�jB�'B�-B�9B�?B�!B��B��B�DB� Bv�BffB]/BM�B<jB(�B	7B�mB�#B��B��BǮB�^B�!B��B�Bw�B]/BN�B;dB$�B�B{BoB+B
�fB
��B
�B
��B
�B
�DB
�bB
�bB
�VB
�1B
~�B
dZB
S�B
?}B
)�B
�B	��B	�sB	�)B	��B	ÖB	�LB	��B	��B	�\B	s�B	`BB	XB	?}B	1'B	(�B	�B	�B	oB		7B	B	B��B�B�TB�B��BɺB��B�?B��B��B�{B�oB�PB�7B�B|�Bu�Bq�Bo�Bl�Be`BbNB`BB]/B[#B\)B[#BZBYBYBZBYBVBT�BS�BS�BR�BQ�BYB]/B[#BW
BQ�BK�B>wB9XB7LB7LB=qB9XB<jB:^B:^B9XB9XB5?B5?B33B2-B0!B/B.B-B(�B%�B$�B)�B,B&�B&�B&�B$�B%�B"�B"�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBuBoBuB{B{BoBhBuBoBoBuB�B�B�B�B�B�B�B�B�B�B�B �B$�B)�B,B-B.B.B.B0!B0!B0!B0!B1'B33B33B49B6FB8RB9XB<jB@�B@�BA�B@�BB�BE�BE�BI�BJ�BJ�BL�BN�BP�BR�BVBW
BYBXBZB\)B_;B`BBaHBbNBdZBhsBjBl�Bl�Bl�Bl�Bm�Bn�Bq�Bs�Bs�Bv�B{�B|�B|�B|�B� B�B�+B�1B�DB�PB�VB�\B�uB��B��B��B��B��B��B��B�B�-B�LB��B�B�B�B�B��B��B��B��B��B	B	B	B	B	B	+B		7B	\B	uB	�B	 �B	'�B	(�B	)�B	)�B	49B	;dB	<jB	<jB	<jB	<jB	=qB	=qB	=qB	?}B	D�B	C�B	B�B	A�B	D�B	F�B	G�B	I�B	H�B	I�B	K�B	M�B	O�B	P�B	R�B	T�B	]/B	]/B	]/B	]/B	_;B	dZB	hsB	iyB	jB	k�B	m�B	o�B	p�B	q�B	r�B	u�B	x�B	z�B	|�B	}�B	}�B	}�B	� B	�B	�B	�B	�+B	�7B	�=B	�DB	�PB	�PB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�FB	�FB	�LB	�RB	�XB	�XB	�dB	�jB	�jB	�qB	�wB	�}B	��B	��B	��B	��B	B	ÖB	ĜB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�HB	�NB	�NB	�HB	�NB	�NB	�NB	�TB	�ZB	�ZB	�`B	�fB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B
JB
PB
VB
VB
bB
bB
hB
hB
hB
oB
oB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
#�B
#�B
"�B
!�B
�B
�B
�B
�B
 �B
!�B
#�B
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
%�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
+B
,B
-B
.B
/B
/B
.B
.B
.B
,B
,B
,B
,B
+B
+B
(�B
(�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
+B
+B
+B
+B
,B
,B
-B
-B
-B
.B
.B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
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
;dB
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
?}B
?}B
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
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
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
H�B
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
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
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
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
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
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
ZB
ZB
ZB
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
_;B
_;B
_;B
_;B
_;B
_;B
_;B
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
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
iyB
iyB
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
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B�B�B��B�B�}B�qB�qB�qB�kB�kB�kB�kB�qB�kB�}B��B �B�BB	B6BNBB"�B,�B5%B88BI�BfLBt�Bx�Bx�By�B~�B�B��B�B�B�<B�TB�NB�mB��B��B��B��B�B��B��B��B��B��B�B�%B�PB�PB�B�B�B�%B�B��B�mB�)B�Bv�BfLB]BM�B<PB(�B	B�RB�	B͹B̳B�zB�DB�B�gB��Bw�B]BN�B;JB$�BmBaBTBB
�2B
�iB
��B
�kB
�B
�B
�HB
�.B
�<B
�B
~�B
d@B
S�B
?HB
)�B
gB	��B	�XB	��B	ˬB	�{B	�2B	��B	��B	�BB	s�B	`'B	W�B	?cB	1B	(�B	�B	YB	TB		B	B	�B��B�B�:B��B��BɠB�oB�B��B��B�aB�TB�6B�B��B|�Bu�Bq�BoiBlWBe,Bb4B`'B]BZ�B[�B[	BY�BX�BX�BZBX�BU�BT�BS�BS�BR�BQ�BX�B]BZ�BV�BQ�BK�B>]B9>B72B72B=<B9$B<6B:DB:*B9$B9>B5%B5%B3B1�B0B.�B-�B,�B(�B%�B$�B)�B+�B&�B&�B&�B$�B%�B"�B"�B �B�B�B�BqBxB�BqBxBqB_BmBSBSBSBmBaB@B[BTB@BFBFB:B4B[B:BTB@BYBeB�BqB�BkB~BxB~B�B�B �B$�B)�B+�B,�B-�B-�B-�B0B0B0B0B1B2�B3B4B6+B88B9>B<6B@iB@OBAoB@iBB[BE�BE�BI�BJ�BJ�BL�BN�BP�BR�BU�BV�BX�BW�BZB[�B_B`BaBbBd&Bh>BjeBlWBlqBlWBlWBm]Bn}BqvBs�Bs�Bv�B{�B|�B|�B|�B�B��B�B��B�)B�6B�"B�BB�@B�MB�yB��B��B��B��B��B��B�B�2B�OB��B�eB��B�|B��B��B��B��B��B	 �B	�B	�B	�B	�B	�B		B	(B	@B	YB	 �B	'�B	(�B	)�B	)�B	4B	;0B	<6B	<PB	<PB	<PB	=<B	=VB	=<B	?HB	DgB	C{B	B[B	AUB	DgB	FtB	GzB	I�B	H�B	I�B	K�B	M�B	O�B	P�B	R�B	T�B	]B	]B	\�B	\�B	_B	d&B	h>B	iDB	jeB	kQB	m]B	o�B	p�B	q�B	r|B	u�B	x�B	z�B	|�B	}�B	}�B	}�B	�B	��B	��B	�B	��B	�B	�#B	�B	�B	�B	�HB	�TB	�@B	�FB	�MB	�SB	�B	�kB	�kB	�qB	�qB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�+B	�B	�2B	�8B	�>B	�$B	�0B	�6B	�6B	�<B	�]B	�HB	�OB	�UB	�UB	�UB	�uB	�{B	āB	�zB	�zB	ȀB	ɆB	ʌB	̘B	͟B	οB	бB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�B	�B	�-B	�B	�4B	�B	�4B	�B	�B	� B	�@B	�@B	�,B	�2B	�DB	�XB	�XB	�_B	�KB	�QB	�cB	�B	�B	�oB	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
B
	B
	B
	B
	B
	B
	B
	B
	B
	B
	B

	B
B
6B
"B
"B
.B
.B
NB
4B
NB
TB
TB
@B
FB
FB
FB
aB
gB
gB
SB
sB
YB
sB
_B
_B
yB
_B
_B
B
eB
eB
kB
�B
�B
�B
�B
xB
�B
�B
�B
~B
�B
~B
�B
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
 �B
 �B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
#�B
#�B
"�B
!�B
�B
�B
�B
�B
 �B
!�B
#�B
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
%�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
*�B
+�B
,�B
-�B
/ B
/ B
-�B
-�B
-�B
+�B
+�B
+�B
+�B
*�B
*�B
(�B
(�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
.�B
/�B
0B
/�B
0B
/�B
0�B
1B
1�B
2�B
2�B
3B
4B
4B
5%B
5B
5B
5%B
5%B
6+B
6B
6+B
6B
6+B
72B
72B
72B
72B
88B
88B
88B
8B
88B
8B
9$B
9>B
9$B
:*B
:*B
:*B
:*B
:*B
:DB
;JB
;JB
;JB
;JB
;JB
<6B
<6B
=VB
=<B
=VB
>]B
>]B
>]B
>BB
?HB
?cB
?cB
?HB
?HB
?HB
?HB
?HB
?cB
@OB
@OB
@OB
AUB
AUB
AUB
B[B
BuB
BuB
CaB
CaB
CaB
C{B
D�B
D�B
DgB
D�B
DgB
DgB
EmB
EmB
EmB
F�B
F�B
FtB
FtB
F�B
GzB
G�B
G�B
G�B
H�B
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
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
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
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
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
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
Y�B
Y�B
Y�B
[	B
Z�B
[	B
Z�B
[	B
\B
\B
[�B
\B
[�B
\�B
]B
\�B
]B
]B
^B
^B
^B
^B
^B
^B
^B
_!B
_B
_B
_B
_!B
_B
_B
`B
`'B
`B
`B
`'B
aB
aB
a-B
aB
a-B
aB
a-B
bB
b4B
bB
b4B
c:B
c B
d@B
d@B
d@B
d&B
e,B
eFB
e,B
eFB
e,B
e,B
eFB
e,B
f2B
f2B
f2B
gRB
g8B
g8B
g8B
g8B
gRB
h>B
g8B
h>B
h>B
hXB
hXB
h>B
hXB
iDB
iDB
iDB
i_B
i_B
i_B
iDB
i_B
jKB
iDB
iDB
jeB
jeB
jKB
jeB
kkB
kQB
kkB
kQB
kkB
kQB
kQB
kQB
lqB
lWB
lqB
lW111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.32(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805190044072018051900440720180519004407201806042358072018060423580720180604235807JA  ARFMdecpA19c                                                                20180514093551  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180514003619  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180514003620  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180514003620  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180514003621  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180514003621  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180514003621  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180514003621  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180514003621  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180514003622                      G�O�G�O�G�O�                JA  ARUP                                                                        20180514005713                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180514153647  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180518154407  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180518154407  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604145807  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                