CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:09Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190609  20181005190609  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�����1   @�������@2 �n���c�;dZ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BG��BO��BX  B`ffBh  Bp  Bx  B�  B�  B�  B�33B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D	  D	�fD
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfDy�D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��Dy�D  D�fDfD� DfD�fD   D � D!  D!� D"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'y�D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2  D2�fD3  D3� D4  D4� D5  D5� D5��D6y�D7  D7� D7��D8� D9  D9� D:  D:� D;  D;� D<fD<� D=  D=�fD>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC�fDD  DDy�DD��DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DNy�DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D]��D^y�D_  D_� D_��D`y�D`��Da� DbfDb�fDc  Dc�fDd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dly�Dm  Dm� Dm��Dn� Do  Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt�fDufDu� Dv  Dv� Dw  Dw� Dw�fDy�=D�P�D��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@�=qA�A%�AE�Ae�A��\A��\A��\A��\A\Aҏ\A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BH�HBP�HBYG�Ba�BiG�BqG�ByG�B���B���B���B��
B���B���B�p�B���B��
B���B���B���B���B���B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�C8RCQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFk�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�Cl8RCnQ�CpQ�CrQ�CtQ�Cvk�CxQ�CzQ�C|Q�C~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�)C�(�C�5�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�D {D �{D{D�{D{D�{D{D�{D�D�{D{D�{D{D�{D{D�{D{D�{D	{D	��D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D��D�D�D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�DD�D{D��D�D�{D�D��D {D �{D!{D!�{D"�D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�D({D(�{D)�D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0��D1{D1�{D2{D2��D3{D3�{D4{D4�{D5{D5�{D6D6�D7{D7�{D8D8�{D9{D9�{D:{D:�{D;{D;�{D<�D<�{D={D=��D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�DC{DC��DD{DD�DEDE�{DF{DF�{DG{DG��DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DNDN�DO{DO�{DP{DP�{DQ{DQ��DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW��DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�D^D^�D_{D_�{D`D`�DaDa�{Db�Db��Dc{Dc��Dd{Dd�{De{De�{DfDf�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj�Dj�{Dk{Dk�{Dl{Dl�Dm{Dm�{DnDn�{Do{Do�{Dp{Dp�{Dq{Dq��Dr{Dr�{Ds{Ds�{Dt{Dt��Du�Du�{Dv{Dv�{Dw{Dw�{Dw��Dy��D�Z�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aʴ9AʶFAʴ9Aʴ9AʶFAʶFAʺ^Aʺ^Aʺ^Aʺ^AʾwA���A���A���AʾwAʾwAʼjAʶFAʡ�A�C�A��A��A��/AɅA�I�A��A���AȃA�VA�A�AǼjA���A�VA�\)A�XA�ƨA���AŲ-AŮAš�AŁA��A�l�A� �A��mA���AîAÓuA�A�I�A�A���A�ZA�1A�-A��DA�x�A���A��A�K�A��A���A�?}A��A�G�A�dZA���A�p�A�z�A��A���A�l�A�9XA���A�ĜA��jA�jA�O�A�?}A�1A���A�`BA��HA��FA�M�A�`BA�XA�Q�A��A���A��FA��wA�&�A�=qA��A�XA���A��A�/A��A��A�1'A�
=A��;A���A��#A��A�|�A���A�?}A��+A�S�A�+A��
A|��At��As�Apz�AnbAj��Agt�Ae��Ad{Aa�-A_l�A\AZ$�AWp�AT�+AQ��AMƨALbNAI��AG�AG�AE�AEXAC\)AB^5A@�`A=K�A:VA9t�A7C�A49XA1oA0�A/��A-XA,1'A+��A+7LA'�^A%�mA%hsA%A$I�A#�hA"�HA!��A ��A��A"�A�AdZAz�A9XA�mAl�A�A�A�A�A�A��A��AJAhsA�+A;dAȴAhsAI�A�;A��A�PAĜA��Al�Ax�A�#A�mA�AJAz�A�AƨA��A/A
(�A	O�A	VA��A��A�\A1A��A��A�FA�hA\)A7LA
=A ��A ^5A 5?@��@�M�@�&�@��/@䛦@�@��T@�7L@��@�+@���@���@�K�@�l�@��m@���@�j@�j@���@�7L@��@���@���@�;d@�ȴ@��@�Ĝ@��`@ܼj@��@ܛ�@�b@��@�t�@��@��@ّh@�O�@��@�z�@��m@�"�@�@�@�V@��@���@ԛ�@�z�@�j@�Q�@�9X@���@�l�@�ȴ@�E�@�-@�p�@���@��/@�9X@ϥ�@�+@Η�@�M�@�J@ͺ^@�&�@���@�Ĝ@�bN@�A�@��@��
@˶F@˝�@��@�~�@�-@��@�X@��`@ȣ�@ȓu@�j@�Q�@�z�@���@���@��`@ȋD@�1'@�;d@Ɨ�@�=q@�-@��@�{@��`@ě�@�r�@öF@�l�@�+@§�@�M�@�J@��@�@�-@�5?@�-@�J@��7@�?}@�j@�j@�Q�@�Z@�9X@�1@��
@�dZ@�V@��@���@��7@�`B@���@��@�1@���@���@�|�@�|�@���@�dZ@���@�@�hs@���@��j@��`@�Ĝ@��D@��m@���@��@��7@�r�@�ƨ@�"�@��@��@�
=@��@�M�@��T@���@�/@�Ĝ@�b@�+@�ff@��7@�G�@��@��@��w@��P@�\)@�+@���@�v�@���@�?}@��@��/@��@�K�@��\@�M�@�M�@�M�@�E�@�@���@�`B@�7L@��@���@��9@��u@�Z@�  @��@�;d@��@�E�@�x�@��@���@�Q�@�ƨ@�S�@�+@��@�{@���@���@��h@��7@��@�x�@�p�@�O�@�7L@�%@�1'@�|�@�|�@�t�@�;d@�+@�o@�o@��@�n�@�$�@��^@��@���@��^@�@���@��@��u@��@�I�@��m@�\)@��@���@���@��\@�M�@�-@��@��@���@�x�@��@���@�Ĝ@��u@�Z@�C�@��H@w��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aʴ9AʶFAʴ9Aʴ9AʶFAʶFAʺ^Aʺ^Aʺ^Aʺ^AʾwA���A���A���AʾwAʾwAʼjAʶFAʡ�A�C�A��A��A��/AɅA�I�A��A���AȃA�VA�A�AǼjA���A�VA�\)A�XA�ƨA���AŲ-AŮAš�AŁA��A�l�A� �A��mA���AîAÓuA�A�I�A�A���A�ZA�1A�-A��DA�x�A���A��A�K�A��A���A�?}A��A�G�A�dZA���A�p�A�z�A��A���A�l�A�9XA���A�ĜA��jA�jA�O�A�?}A�1A���A�`BA��HA��FA�M�A�`BA�XA�Q�A��A���A��FA��wA�&�A�=qA��A�XA���A��A�/A��A��A�1'A�
=A��;A���A��#A��A�|�A���A�?}A��+A�S�A�+A��
A|��At��As�Apz�AnbAj��Agt�Ae��Ad{Aa�-A_l�A\AZ$�AWp�AT�+AQ��AMƨALbNAI��AG�AG�AE�AEXAC\)AB^5A@�`A=K�A:VA9t�A7C�A49XA1oA0�A/��A-XA,1'A+��A+7LA'�^A%�mA%hsA%A$I�A#�hA"�HA!��A ��A��A"�A�AdZAz�A9XA�mAl�A�A�A�A�A�A��A��AJAhsA�+A;dAȴAhsAI�A�;A��A�PAĜA��Al�Ax�A�#A�mA�AJAz�A�AƨA��A/A
(�A	O�A	VA��A��A�\A1A��A��A�FA�hA\)A7LA
=A ��A ^5A 5?@��@�M�@�&�@��/@䛦@�@��T@�7L@��@�+@���@���@�K�@�l�@��m@���@�j@�j@���@�7L@��@���@���@�;d@�ȴ@��@�Ĝ@��`@ܼj@��@ܛ�@�b@��@�t�@��@��@ّh@�O�@��@�z�@��m@�"�@�@�@�V@��@���@ԛ�@�z�@�j@�Q�@�9X@���@�l�@�ȴ@�E�@�-@�p�@���@��/@�9X@ϥ�@�+@Η�@�M�@�J@ͺ^@�&�@���@�Ĝ@�bN@�A�@��@��
@˶F@˝�@��@�~�@�-@��@�X@��`@ȣ�@ȓu@�j@�Q�@�z�@���@���@��`@ȋD@�1'@�;d@Ɨ�@�=q@�-@��@�{@��`@ě�@�r�@öF@�l�@�+@§�@�M�@�J@��@�@�-@�5?@�-@�J@��7@�?}@�j@�j@�Q�@�Z@�9X@�1@��
@�dZ@�V@��@���@��7@�`B@���@��@�1@���@���@�|�@�|�@���@�dZ@���@�@�hs@���@��j@��`@�Ĝ@��D@��m@���@��@��7@�r�@�ƨ@�"�@��@��@�
=@��@�M�@��T@���@�/@�Ĝ@�b@�+@�ff@��7@�G�@��@��@��w@��P@�\)@�+@���@�v�@���@�?}@��@��/@��@�K�@��\@�M�@�M�@�M�@�E�@�@���@�`B@�7L@��@���@��9@��u@�Z@�  @��@�;d@��@�E�@�x�@��@���@�Q�@�ƨ@�S�@�+@��@�{@���@���@��h@��7@��@�x�@�p�@�O�@�7L@�%@�1'@�|�@�|�@�t�@�;d@�+@�o@�o@��@�n�@�$�@��^@��@���@��^@�@���@��@��u@��@�I�@��m@�\)@��@���@���@��\@�M�@�-@��@��@���@�x�@��@���@�Ĝ@��u@�Z@�C�@��H@w��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBBBBBBBBBBBÖBÖBÖBÖBÖBÖBÖBŢB��B�B	
=B	�B	aHB	��B	��B
uB
&�B
33B
R�B
p�B
�DB
�B
�LB
��B
��BBJBbBuB�B5?BI�BR�Bl�Bs�B�B�\B��B��B�B�?B�RB�XB�B�9B��B��B��B�B�yB��B��B	7B�B�B#�B&�B;dBP�BXB^5BgmBk�Bo�Bq�Bk�Bo�Bx�B|�B{�B{�Bx�Bz�B�7B�bB�Bw�Bk�BR�B:^B/B%�BB�;B�B�B�B��BiyB�B�B�B�B�BDB
�B
ÖB
�B
m�B
I�B
JB	ŢB	��B	u�B	A�B	:^B	D�B	@�B	=qB	33B	+B	$�B	�B	JB	  B��B�B�;B��B��B��B��B�TB�B�B�B�B�B�fB�)B�5B�TB�B�)B��BȴBǮBȴBƨBĜBB��B�}B�wB�qB�jB�jB�wBÖBɺB��B��B��B��B��B��B��B��B�/B�B�B�B�B�B��B��B��B	  B��B��B	  B	B	+B	DB	\B	PB	DB	PB	�B	$�B	(�B	)�B	-B	8RB	C�B	>wB	;dB	7LB	:^B	:^B	9XB	0!B	(�B	%�B	"�B	�B	�B	�B	�B	 �B	$�B	'�B	)�B	-B	/B	1'B	33B	YB	_;B	_;B	\)B	[#B	[#B	XB	W
B	W
B	ZB	ffB	jB	r�B	�B	�B	�B	�=B	�bB	�\B	�\B	�\B	�VB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�?B	�9B	�RB	�XB	�XB	�^B	�^B	�^B	�dB	�dB	�dB	�dB	�jB	�qB	�wB	�wB	�}B	�}B	�}B	��B	��B	��B	��B	��B	B	B	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�/B	�/B	�/B	�/B	�#B	�B	�#B	�)B	�5B	�BB	�/B	�/B	�/B	�5B	�HB	�TB	�TB	�TB	�TB	�TB	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
%B
%B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
1B
	7B
1B
�B
&�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 BBBBBBBBBBBBÖBÖBÖBÖBÖBÖBÖBŢB��B�B	
=B	�B	aHB	��B	��B
uB
&�B
33B
R�B
p�B
�DB
�B
�LB
��B
��BBJBbBuB�B5?BI�BR�Bl�Bs�B�B�\B��B��B�B�?B�RB�XB�B�9B��B��B��B�B�yB��B��B	7B�B�B#�B&�B;dBP�BXB^5BgmBk�Bo�Bq�Bk�Bo�Bx�B|�B{�B{�Bx�Bz�B�7B�bB�Bw�Bk�BR�B:^B/B%�BB�;B�B�B�B��BiyB�B�B�B�B�BDB
�B
ÖB
�B
m�B
I�B
JB	ŢB	��B	u�B	A�B	:^B	D�B	@�B	=qB	33B	+B	$�B	�B	JB	  B��B�B�;B��B��B��B��B�TB�B�B�B�B�B�fB�)B�5B�TB�B�)B��BȴBǮBȴBƨBĜBB��B�}B�wB�qB�jB�jB�wBÖBɺB��B��B��B��B��B��B��B��B�/B�B�B�B�B�B��B��B��B	  B��B��B	  B	B	+B	DB	\B	PB	DB	PB	�B	$�B	(�B	)�B	-B	8RB	C�B	>wB	;dB	7LB	:^B	:^B	9XB	0!B	(�B	%�B	"�B	�B	�B	�B	�B	 �B	$�B	'�B	)�B	-B	/B	1'B	33B	YB	_;B	_;B	\)B	[#B	[#B	XB	W
B	W
B	ZB	ffB	jB	r�B	�B	�B	�B	�=B	�bB	�\B	�\B	�\B	�VB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�?B	�9B	�RB	�XB	�XB	�^B	�^B	�^B	�dB	�dB	�dB	�dB	�jB	�qB	�wB	�wB	�}B	�}B	�}B	��B	��B	��B	��B	��B	B	B	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�/B	�/B	�/B	�/B	�#B	�B	�#B	�)B	�5B	�BB	�/B	�/B	�/B	�5B	�HB	�TB	�TB	�TB	�TB	�TB	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
%B
%B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
1B
	7B
1B
�B
&�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.32 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190609                              AO  ARCAADJP                                                                    20181005190609    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190609  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190609  QCF$                G�O�G�O�G�O�8000            