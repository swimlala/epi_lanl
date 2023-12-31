CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:19:47Z creation;2022-06-04T19:19:48Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191947  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               5A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�*���6�1   @�*�>F�@-�l�C���ci�$�/1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�ffB���B���B�  B�  B�  B�  B���B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B���B�  B�  C   C  C  C  C� C	��C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C033C1�fC4�C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�R@u�@��\@��\AG�A=G�A]G�A{�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B���B���B�\B�B�B�u�B���B���B���B���B�u�B��)B�u�B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B�u�B�u�B��B��B�u�B���B���B���C�{C�{C�{CT{C	�HC�{C��C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C0�C1��C3�C5�{C7�{C9�{C;��C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�DwuDw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D��\D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�=�D�}�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�z�D⺏D���D�:�D�z�D㺏D���D�:�D�z�D亏D���D�:�D�z�D序D���D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D躏D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��KA��;A��QA���A���A��PA���A���A��A��A�;A�_A��A��A��A��A�OA�kA��A�xA��A�	A�xA�.A��A��2A�ĜAչ�Aչ$Aշ�Aյ?AնzAխ�A�yrAԝ~A�h
A�8�A�WsA�K�A�o5A�{A��VA���A�4�A���A�x8A�q�A�)_A��A��A�#�A�~�A��A��aA���A�\�A�A���A���A���A�(A�YA�ǮA�(A��A�v`A�,qA�YA��<A��A�v+A�J�A�\A���A��A���A���A�7�A�.A��vA�!A�J�A��9A���A��A��?A��A��)A��SA�N<A���A}kQAx��Ar�]Ai�cAe��Aci�A^�)AZ��AVjAR�AP��AO�AAN�hAN�ALxAF�XAEMADW?AC33ABQAA��AA1'A>ߤA=dZA;� A91�A7�HA7�A6bNA5qA4	A3`�A0s�A.��A,ZA)��A(��A("hA'w�A'E�A'	A&~A$n�A$1'A"��A" �A!�[A ��A L�A��APHA�IA��A<6A��A��A��A�fA��AݘA"�A�AC�A��Al"AOvA��A��A�A�A��A.IA!-AiDAp�AYA�Ay�AMjA��AA��A<�A��A��A�AN<A�'A�A1'A$tA��A�ZA��A{JA
��A
�A	�AA	~A	_A��A��AԕA�A	YKA	�zA	��A	^5A	eA��AxlA-A�`AYKA֡A��Aj�A�6A��A��A��A�AR�A{JA4�A�UA6zA�/A�-AzxA�A�_AD�A ��A ��@���@�ȴ@�n�@��a@�a@�*0@��F@���@���@�C�@���@��9@��@�T�@��@�;@��@�h@�Mj@���@�	@@��@��@@�!�@�  @��@�5?@�p�@��@�*0@�!@�Ta@��]@�h�@�e@�1�@��/@�!@�{@��d@�@O@��v@�c�@�=@�!�@��Q@�4@މ�@�_@�Q@�Ft@ݞ�@ܭ�@��@�d�@��A@�a@�)_@�
�@��@�z@�1�@�($@ִ9@�p;@��@�&@��@��)@Բ�@�3�@���@�O@�8@���@�:*@ѽ�@�W?@���@��W@Ϭq@ϥ�@���@��@ͅ@̀4@�tT@�!�@˅@ʩ�@��)@�-w@�ں@ȑ�@�K^@��@�ݘ@Ǳ[@Ǔ@�.I@ƭ�@�:*@��@�x@���@�v`@Ē�@��g@��@�Ɇ@®}@�Xy@�@���@�K�@��@�{�@���@�ߤ@���@�YK@�خ@���@�O@��j@�:�@���@��f@��@���@���@���@�|�@�W?@�@O@��|@��e@���@�`�@�7@���@�E9@�#�@��@�Xy@��6@�x@�V@�ȴ@���@�7@���@�!-@��@���@��@�e�@�1�@��D@��0@���@�@�q�@��z@���@�J�@��}@�@���@�|@��@�bN@�)�@���@�<6@���@���@�� @�)�@��r@���@�#�@�	l@��@��o@�+k@���@��j@�!�@��@���@�j@��@�g�@��@��@��d@��=@�f�@�.I@��@��@��@���@�GE@��@��]@��r@��@��z@���@��q@��=@�l�@�>�@��@��2@��?@���@��@�7�@�#:@��@�J�@��@���@�Ta@�($@���@���@��M@�w2@�e�@�-w@��@���@���@��!@���@�y>@�Q�@��@�x@�5�@��@�͟@���@�Ov@��A@���@�1�@��R@���@�x@��@�P�@��@�� @�-�@��m@��@�Z�@��@��M@��]@��4@��@���@�c�@�Ta@�@��f@�2a@��@��@�u�@�V�@�-@��@��f@�Y@�ѷ@�\�@�:*@�1�@�e@�u�@��@��@��1@�w�@�a|@�1'@��#@��q@��@�)_@���@��B@���@���@�:*@���@���@��k@�Vm@�1�@��@��?@��1@��@��@���@�zx@�-w@���@���@�:*@���@���@��4@�Z�@�C@��R@��@�c�@�;�@�u@��@���@���@���@�t�@�<6@��P@��@���@��|@��U@��@��u@�~(@�C-@�M@��@��f@�`B@�q@�Ɇ@�v�@�D�@�,=@��@��@RT@~ں@~�h@~V@}�@}j@|�P@|�$@|�@|2�@{�K@{j�@{,�@z��@z�L@zn�@z�@y��@y�@xC-@w��@wMj@w i@v��@v��@u�T@uc@u:�@t��@t��@s�6@s1�@r��@rJ@q|@q\�@q?}@q�@p��@pQ�@p7@o˒@oW?@o
=@n�@n�!@nH�@mϫ@m��@m-w@l�f@l�@l,=@k��@k8@k�@j}V@i�@iw2@i�@hѷ@h��@h�@g�F@ge�@f�H@f��@f�@e�d@e8�@d�I@dH@c�P@b��@bu%@b-@a�@a�@`�@`,=@_��@_!-@^~�@^)�@]�@]��@]�=@]��@]4@\Ɇ@\`�@[�g@[$t@Z�!@Zh
@Z_@Y-w@X�@Xr�@W�+@W�@@W\)@W!-@V��@Vc @U��@U�S@UF@U \@T��@T��@T��@Tw�@TPH@T�@S˒@SU�@R�"@R�@R
�@Q�"@Q=�@PS�@O�@@O�@O�{@OZ�@O.I@OC@O�@Nߤ@Nȴ@N�R@N{�@Nh
@NL0@N=q@N#:@N@NJ@M�Z@MY�@M%@L֡@L�Y@LH@L%�@L"h@Lx@K��@K�@Jq�@J-@J�@I��@I}�@I:�@I%F@H�	@H��@H��@Hc�@HA�@G�]@G�V@Ge�@G4�@F��@FYK@E��@E�3@E��@E=�@D��@DQ�@C�@C1�@B��@BJ�@A�Z@A��@AO�@@�`@@bN@?�;@?�V@?��@?,�@>�X@>z@>!�@=�T@=A @=�@<֡@<PH@<%�@<  @;�}@;�f@;U�@;,�@:�@:��@:n�@:C�@:�@9�.@9��@9�h@9=�@8�[@8g8@7�@7�@7iD@79�@6\�@6�@5��@5��@5��@5x�@5�@4�@4K^@3�g@3��@3)_@3�@2��@2�L@2l�@2u@1�N@1��@1#�@0��@0��@0D�@/�Q@/�4@/E9@.�@.�m@.��@.GE@..�@.{@-��@-k�@-?}@-@,Ɇ@,�4@,:�@+�@+]�@+�@*{�@)��@)�'@)c@):�@(�@(��@(�u@(��@(_@(@'�@'��@'�P@'U�@'/�@'�@&�@&�6@&��@&�@&��@&}V@&c @&Z�@&)�@&�@&
�@%�N@%w2@%e,@%=�@%@$�P@$�@$�O@$_@$M@#��@#b�@#RT@#9�@#S@"��@"�1@"v�@"=q@" �@!�^@!Q�@ �[@ �z@ �D@ [�@��@iD@\)@J#@.I@�M@�!@}V@Ta@�@�@��@}�@^�@�@��@h�@7�@˒@X�@'�@(@�m@c @!�@J@�@�@hs@	l@�`@��@u�@Z@1'@��@��@��@��@y�@�@�y@��@}V@L0@�@��@k�@X@��@�`@�@��@֡@�j@�o@H@"h@�]@�m@��@��@=@ i@�@��@d�@
�@�@��@k�@Dg@�@��@��@j@%�@�@��@dZ@=@/�@.I@�@�c@ȴ@��@z@.�@$�@�@�@��@�3@�@u�@L�@#�@�@��@�[@�@�Y@j@ �@�@�@��@ƨ@�:@l�@_p11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��KA��;A��QA���A���A��PA���A���A��A��A�;A�_A��A��A��A��A�OA�kA��A�xA��A�	A�xA�.A��A��2A�ĜAչ�Aչ$Aշ�Aյ?AնzAխ�A�yrAԝ~A�h
A�8�A�WsA�K�A�o5A�{A��VA���A�4�A���A�x8A�q�A�)_A��A��A�#�A�~�A��A��aA���A�\�A�A���A���A���A�(A�YA�ǮA�(A��A�v`A�,qA�YA��<A��A�v+A�J�A�\A���A��A���A���A�7�A�.A��vA�!A�J�A��9A���A��A��?A��A��)A��SA�N<A���A}kQAx��Ar�]Ai�cAe��Aci�A^�)AZ��AVjAR�AP��AO�AAN�hAN�ALxAF�XAEMADW?AC33ABQAA��AA1'A>ߤA=dZA;� A91�A7�HA7�A6bNA5qA4	A3`�A0s�A.��A,ZA)��A(��A("hA'w�A'E�A'	A&~A$n�A$1'A"��A" �A!�[A ��A L�A��APHA�IA��A<6A��A��A��A�fA��AݘA"�A�AC�A��Al"AOvA��A��A�A�A��A.IA!-AiDAp�AYA�Ay�AMjA��AA��A<�A��A��A�AN<A�'A�A1'A$tA��A�ZA��A{JA
��A
�A	�AA	~A	_A��A��AԕA�A	YKA	�zA	��A	^5A	eA��AxlA-A�`AYKA֡A��Aj�A�6A��A��A��A�AR�A{JA4�A�UA6zA�/A�-AzxA�A�_AD�A ��A ��@���@�ȴ@�n�@��a@�a@�*0@��F@���@���@�C�@���@��9@��@�T�@��@�;@��@�h@�Mj@���@�	@@��@��@@�!�@�  @��@�5?@�p�@��@�*0@�!@�Ta@��]@�h�@�e@�1�@��/@�!@�{@��d@�@O@��v@�c�@�=@�!�@��Q@�4@މ�@�_@�Q@�Ft@ݞ�@ܭ�@��@�d�@��A@�a@�)_@�
�@��@�z@�1�@�($@ִ9@�p;@��@�&@��@��)@Բ�@�3�@���@�O@�8@���@�:*@ѽ�@�W?@���@��W@Ϭq@ϥ�@���@��@ͅ@̀4@�tT@�!�@˅@ʩ�@��)@�-w@�ں@ȑ�@�K^@��@�ݘ@Ǳ[@Ǔ@�.I@ƭ�@�:*@��@�x@���@�v`@Ē�@��g@��@�Ɇ@®}@�Xy@�@���@�K�@��@�{�@���@�ߤ@���@�YK@�خ@���@�O@��j@�:�@���@��f@��@���@���@���@�|�@�W?@�@O@��|@��e@���@�`�@�7@���@�E9@�#�@��@�Xy@��6@�x@�V@�ȴ@���@�7@���@�!-@��@���@��@�e�@�1�@��D@��0@���@�@�q�@��z@���@�J�@��}@�@���@�|@��@�bN@�)�@���@�<6@���@���@�� @�)�@��r@���@�#�@�	l@��@��o@�+k@���@��j@�!�@��@���@�j@��@�g�@��@��@��d@��=@�f�@�.I@��@��@��@���@�GE@��@��]@��r@��@��z@���@��q@��=@�l�@�>�@��@��2@��?@���@��@�7�@�#:@��@�J�@��@���@�Ta@�($@���@���@��M@�w2@�e�@�-w@��@���@���@��!@���@�y>@�Q�@��@�x@�5�@��@�͟@���@�Ov@��A@���@�1�@��R@���@�x@��@�P�@��@�� @�-�@��m@��@�Z�@��@��M@��]@��4@��@���@�c�@�Ta@�@��f@�2a@��@��@�u�@�V�@�-@��@��f@�Y@�ѷ@�\�@�:*@�1�@�e@�u�@��@��@��1@�w�@�a|@�1'@��#@��q@��@�)_@���@��B@���@���@�:*@���@���@��k@�Vm@�1�@��@��?@��1@��@��@���@�zx@�-w@���@���@�:*@���@���@��4@�Z�@�C@��R@��@�c�@�;�@�u@��@���@���@���@�t�@�<6@��P@��@���@��|@��U@��@��u@�~(@�C-@�M@��@��f@�`B@�q@�Ɇ@�v�@�D�@�,=@��@��@RT@~ں@~�h@~V@}�@}j@|�P@|�$@|�@|2�@{�K@{j�@{,�@z��@z�L@zn�@z�@y��@y�@xC-@w��@wMj@w i@v��@v��@u�T@uc@u:�@t��@t��@s�6@s1�@r��@rJ@q|@q\�@q?}@q�@p��@pQ�@p7@o˒@oW?@o
=@n�@n�!@nH�@mϫ@m��@m-w@l�f@l�@l,=@k��@k8@k�@j}V@i�@iw2@i�@hѷ@h��@h�@g�F@ge�@f�H@f��@f�@e�d@e8�@d�I@dH@c�P@b��@bu%@b-@a�@a�@`�@`,=@_��@_!-@^~�@^)�@]�@]��@]�=@]��@]4@\Ɇ@\`�@[�g@[$t@Z�!@Zh
@Z_@Y-w@X�@Xr�@W�+@W�@@W\)@W!-@V��@Vc @U��@U�S@UF@U \@T��@T��@T��@Tw�@TPH@T�@S˒@SU�@R�"@R�@R
�@Q�"@Q=�@PS�@O�@@O�@O�{@OZ�@O.I@OC@O�@Nߤ@Nȴ@N�R@N{�@Nh
@NL0@N=q@N#:@N@NJ@M�Z@MY�@M%@L֡@L�Y@LH@L%�@L"h@Lx@K��@K�@Jq�@J-@J�@I��@I}�@I:�@I%F@H�	@H��@H��@Hc�@HA�@G�]@G�V@Ge�@G4�@F��@FYK@E��@E�3@E��@E=�@D��@DQ�@C�@C1�@B��@BJ�@A�Z@A��@AO�@@�`@@bN@?�;@?�V@?��@?,�@>�X@>z@>!�@=�T@=A @=�@<֡@<PH@<%�@<  @;�}@;�f@;U�@;,�@:�@:��@:n�@:C�@:�@9�.@9��@9�h@9=�@8�[@8g8@7�@7�@7iD@79�@6\�@6�@5��@5��@5��@5x�@5�@4�@4K^@3�g@3��@3)_@3�@2��@2�L@2l�@2u@1�N@1��@1#�@0��@0��@0D�@/�Q@/�4@/E9@.�@.�m@.��@.GE@..�@.{@-��@-k�@-?}@-@,Ɇ@,�4@,:�@+�@+]�@+�@*{�@)��@)�'@)c@):�@(�@(��@(�u@(��@(_@(@'�@'��@'�P@'U�@'/�@'�@&�@&�6@&��@&�@&��@&}V@&c @&Z�@&)�@&�@&
�@%�N@%w2@%e,@%=�@%@$�P@$�@$�O@$_@$M@#��@#b�@#RT@#9�@#S@"��@"�1@"v�@"=q@" �@!�^@!Q�@ �[@ �z@ �D@ [�@��@iD@\)@J#@.I@�M@�!@}V@Ta@�@�@��@}�@^�@�@��@h�@7�@˒@X�@'�@(@�m@c @!�@J@�@�@hs@	l@�`@��@u�@Z@1'@��@��@��@��@y�@�@�y@��@}V@L0@�@��@k�@X@��@�`@�@��@֡@�j@�o@H@"h@�]@�m@��@��@=@ i@�@��@d�@
�@�@��@k�@Dg@�@��@��@j@%�@�@��@dZ@=@/�@.I@�@�c@ȴ@��@z@.�@$�@�@�@��@�3@�@u�@L�@#�@�@��@�[@�@�Y@j@ �@�@�@��@ƨ@�:@l�@_p11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�BB	�]B	�BB	��B	��B	��B	��B	��B	�VB	�6B	�B	�PB	��B	��B	�B	�BB	�B	��B	��B	��B	��B	�wB	��B	��B	�B	�>B	��B	��B	��B	��B	��B	��B	��B	�2B	��B	��B	�*B	��B	�OB	g�B	C�B	X_B	\xB	p;B	��B	�eB
0�B
7�B
;B
S[B
�B
՛B
��B�B)yB<�B=�BB�Br�B�bB��B�B��B�'B��B�BɺB�B�0BsMB\�BX�BP}BLBE9B7�B&LB.B
��B
�6B
��B
�B
��B
��B
v�B
dB
Q4B
G�B
5?B
{B
SB	�^B	�B	�#B	X�B	C-B	6B	$&B	�B	�B	�B	�B	 iB�.B�0B��B��B�B��B	B	�B	AB	�B	{B	$�B	# B	B	�B	/ B	9�B	F�B	JXB	G�B	;0B	.IB	dB		7B	�B	 �B	 �B	 �B	B		�B	B	B	)*B	-B	1�B	@ B	B'B	CB	C�B	E�B	L0B	J�B	O�B	XB	^B	\CB	\xB	YKB	W�B	X�B	W�B	O�B	V9B	_VB	poB	��B	��B	�LB	�B	��B	��B	�XB	āB	ȚB	�B	� B	͟B	οB	�B	��B	ϫB	�B	�-B	�RB	�DB	�fB	��B	�B	�B	��B	�3B	��B	��B	�4B	��B	�VB	��B	��B	�KB	��B	�3B	��B	�GB	�4B	՛B	�SB	�YB	�B	�7B	چB	�eB	��B	��B	רB	ևB	��B	�vB	��B	��B	�6B	��B	ٴB	�_B	׍B	��B	�B	�B	��B	�B	�_B	�sB	�mB	��B	�[B	ѝB	�NB	ҽB	�uB	�[B	��B	�EB	�7B	�eB	�B	�&B	�\B	��B	�.B	�}B	�:B	өB	ҽB	өB	��B	�aB	ԯB	��B	�{B	�2B	��B	� B	�(B	�jB	̘B	�~B	�)B	�xB	�"B	�2B	��B	��B	�MB	�B	��B	�kB	��B	�WB	�qB	��B	�qB	ܬB	ۦB	��B	�5B	��B	��B	�B	�pB	�IB	ݘB	�]B	ںB	�jB	�WB	�+B	�B	خB	�QB	�B	��B	��B	�B	�6B	��B	�B	�kB	��B	��B	��B	�6B	�yB	�B	�mB	��B	�B	��B	��B	�ZB	�HB	��B	�B	�4B	�B	�HB	�bB	�hB	�B	�B	�TB	�B	�B	�zB	��B	�B	�fB	�B	�B	�B	�B	�$B	�B	�B	��B	�*B	��B	�sB	��B	�B	��B	��B	�eB	�B	��B	�B	��B	�)B	��B	�B	�IB	��B	�5B	�!B	�B	��B	��B	�vB	�B	��B	��B	��B	�-B	�GB	�-B	�GB	�|B	�3B	�B	�B	��B	�B	�9B	�TB	�B	��B	�tB	��B	�zB	�+B	�+B	�FB	�zB	��B	��B	�2B	�LB	��B	��B	��B	�RB	��B	��B	��B	��B	��B	��B	��B	�6B	�jB	�<B	�B	�BB	��B	��B	�.B	�HB
  B
 OB
 OB
 OB
 �B
 B
;B
AB
�B
3B
MB
�B
B
�B
�B
�B
�B
%B
tB
�B
�B
�B
�B
B
�B
�B
EB
B
�B
�B
�B
�B
�B
�B
	B
	7B
	RB
	�B

#B
0B
B
�B
JB
JB
jB
�B
�B
vB
bB
NB
oB
�B
�B
�B
�B
�B
B
�B
�B
B
9B
9B
�B
MB
�B
�B
SB
sB
�B
�B
�B
�B
?B
�B
�B
9B
�B
B
�B
�B
$B
�B
�B
�B
�B
KB
B
B
B
�B
1B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
WB
�B
B
CB
CB
�B
�B
�B
/B
dB
dB
�B
B
5B
OB
�B
�B
!B
!B
VB
�B
 BB
 �B
!B
!�B
"B
"hB
"�B
#:B
# B
$@B
$�B
%B
%zB
%�B
&B
&�B
&�B
'mB
'�B
'�B
(�B
)*B
)DB
)_B
)yB
*B
*B
*B
*�B
*�B
+B
+�B
,WB
,�B
./B
.�B
/B
.�B
.}B
.cB
/iB
0oB
1�B
1�B
1�B
2GB
2�B
3MB
3hB
3hB
3hB
3�B
3�B
4B
4B
4TB
4�B
4�B
5%B
5%B
5?B
5?B
5%B
5ZB
5tB
5�B
5�B
6B
6`B
6`B
6�B
7fB
7�B
7�B
8B
8B
8B
8�B
8�B
8�B
8�B
8�B
9rB
9�B
9�B
:xB
:�B
:�B
:�B
:�B
;B
;0B
;0B
;B
;�B
;�B
;�B
<B
<PB
<�B
<�B
=B
=B
="B
=�B
=�B
>BB
>(B
>�B
?B
?cB
?�B
?�B
?�B
@�B
@�B
@�B
A;B
AoB
A�B
A�B
B'B
B�B
B�B
CaB
C�B
DB
DMB
D�B
ESB
E�B
FB
F?B
F�B
G_B
GzB
G�B
G�B
G�B
G�B
G�B
H1B
HfB
H�B
I7B
IlB
I�B
IlB
I�B
I�B
IlB
I�B
I�B
J	B
J	B
J=B
J�B
J�B
KB
K^B
KxB
K�B
K�B
K�B
K�B
K�B
L0B
L�B
L~B
L�B
L�B
MPB
M�B
M�B
N�B
N�B
O(B
OB
O(B
OBB
O\B
OvB
O\B
O\B
OvB
O�B
O�B
O�B
O�B
O�B
O�B
PB
P�B
P�B
P}B
P�B
P�B
Q4B
Q�B
Q�B
Q�B
Q�B
RTB
R�B
R�B
R�B
R�B
S&B
S&B
S&B
S&B
S@B
SuB
SuB
SuB
S�B
S�B
TB
TB
T{B
T�B
T�B
T�B
T�B
U2B
U�B
U�B
V9B
V9B
VmB
V�B
W
B
W$B
W$B
W�B
W�B
X+B
X+B
XEB
X�B
X�B
X�B
YKB
YKB
Y�B
Y�B
Y�B
Z7B
Z7B
Z7B
ZQB
Z�B
Z�B
Z�B
[	B
[#B
[=B
[WB
[qB
[�B
[�B
[�B
\B
\CB
\�B
\�B
]IB
]dB
]IB
^5B
^OB
^�B
^�B
^�B
^�B
^�B
_;B
_VB
_�B
`B
`vB
`vB
`�B
`�B
`�B
aB
aHB
a�B
a�B
bB
bB
b4B
b�B
b�B
b�B
cB
c:B
cTB
cnB
c�B
c�B
c�B
dtB
dtB
dtB
d�B
d�B
d�B
e`B
e`B
ezB
f2B
f�B
f�B
f�B
f�B
gB
gRB
g�B
g�B
g�B
h
B
h$B
h$B
h$B
h>B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
iB
i*B
i*B
i*B
i_B
i_B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j0B
jeB
j�B
kB
kB
kB
k6B
kkB
k�B
k�B
k�B
k�B
lqB
m)B
m)B
m)B
m]B
m�B
nB
n�B
n}B
n�B
n�B
n�B
n�B
oiB
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
q[B
q�B
q�B
q�B
rB
r-B
rGB
raB
raB
r�B
r�B
s3B
sB
shB
s�B
s�B
s�B
s�B
shB
s�B
t�B
t�B
tnB
tB
tB
tnB
t�B
t�B
uZB
utB
utB
u�B
u�B
u�B
u�B
u�B
u�B
vB
vFB
vzB
v�B
v�B
v�B
v�B
w�B
w�B
w�B
xB
w�B
xlB
xRB
x�B
x�B
x�B
y$B
yrB
y�B
zB
z*B
zDB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{JB
{�B
{�B
{�B
{�B
{�B
|6B
|6B
|jB
|�B
|�B
}B
}B
|�B
}<B
}VB
}qB
}�B
~B
~B
~BB
~BB
~wB
~�B
~�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�BB	�]B	�BB	��B	��B	��B	��B	��B	�VB	�6B	�B	�PB	��B	��B	�B	�BB	�B	��B	��B	��B	��B	�wB	��B	��B	�B	�>B	��B	��B	��B	��B	��B	��B	��B	�2B	��B	��B	�*B	��B	�OB	g�B	C�B	X_B	\xB	p;B	��B	�eB
0�B
7�B
;B
S[B
�B
՛B
��B�B)yB<�B=�BB�Br�B�bB��B�B��B�'B��B�BɺB�B�0BsMB\�BX�BP}BLBE9B7�B&LB.B
��B
�6B
��B
�B
��B
��B
v�B
dB
Q4B
G�B
5?B
{B
SB	�^B	�B	�#B	X�B	C-B	6B	$&B	�B	�B	�B	�B	 iB�.B�0B��B��B�B��B	B	�B	AB	�B	{B	$�B	# B	B	�B	/ B	9�B	F�B	JXB	G�B	;0B	.IB	dB		7B	�B	 �B	 �B	 �B	B		�B	B	B	)*B	-B	1�B	@ B	B'B	CB	C�B	E�B	L0B	J�B	O�B	XB	^B	\CB	\xB	YKB	W�B	X�B	W�B	O�B	V9B	_VB	poB	��B	��B	�LB	�B	��B	��B	�XB	āB	ȚB	�B	� B	͟B	οB	�B	��B	ϫB	�B	�-B	�RB	�DB	�fB	��B	�B	�B	��B	�3B	��B	��B	�4B	��B	�VB	��B	��B	�KB	��B	�3B	��B	�GB	�4B	՛B	�SB	�YB	�B	�7B	چB	�eB	��B	��B	רB	ևB	��B	�vB	��B	��B	�6B	��B	ٴB	�_B	׍B	��B	�B	�B	��B	�B	�_B	�sB	�mB	��B	�[B	ѝB	�NB	ҽB	�uB	�[B	��B	�EB	�7B	�eB	�B	�&B	�\B	��B	�.B	�}B	�:B	өB	ҽB	өB	��B	�aB	ԯB	��B	�{B	�2B	��B	� B	�(B	�jB	̘B	�~B	�)B	�xB	�"B	�2B	��B	��B	�MB	�B	��B	�kB	��B	�WB	�qB	��B	�qB	ܬB	ۦB	��B	�5B	��B	��B	�B	�pB	�IB	ݘB	�]B	ںB	�jB	�WB	�+B	�B	خB	�QB	�B	��B	��B	�B	�6B	��B	�B	�kB	��B	��B	��B	�6B	�yB	�B	�mB	��B	�B	��B	��B	�ZB	�HB	��B	�B	�4B	�B	�HB	�bB	�hB	�B	�B	�TB	�B	�B	�zB	��B	�B	�fB	�B	�B	�B	�B	�$B	�B	�B	��B	�*B	��B	�sB	��B	�B	��B	��B	�eB	�B	��B	�B	��B	�)B	��B	�B	�IB	��B	�5B	�!B	�B	��B	��B	�vB	�B	��B	��B	��B	�-B	�GB	�-B	�GB	�|B	�3B	�B	�B	��B	�B	�9B	�TB	�B	��B	�tB	��B	�zB	�+B	�+B	�FB	�zB	��B	��B	�2B	�LB	��B	��B	��B	�RB	��B	��B	��B	��B	��B	��B	��B	�6B	�jB	�<B	�B	�BB	��B	��B	�.B	�HB
  B
 OB
 OB
 OB
 �B
 B
;B
AB
�B
3B
MB
�B
B
�B
�B
�B
�B
%B
tB
�B
�B
�B
�B
B
�B
�B
EB
B
�B
�B
�B
�B
�B
�B
	B
	7B
	RB
	�B

#B
0B
B
�B
JB
JB
jB
�B
�B
vB
bB
NB
oB
�B
�B
�B
�B
�B
B
�B
�B
B
9B
9B
�B
MB
�B
�B
SB
sB
�B
�B
�B
�B
?B
�B
�B
9B
�B
B
�B
�B
$B
�B
�B
�B
�B
KB
B
B
B
�B
1B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
WB
�B
B
CB
CB
�B
�B
�B
/B
dB
dB
�B
B
5B
OB
�B
�B
!B
!B
VB
�B
 BB
 �B
!B
!�B
"B
"hB
"�B
#:B
# B
$@B
$�B
%B
%zB
%�B
&B
&�B
&�B
'mB
'�B
'�B
(�B
)*B
)DB
)_B
)yB
*B
*B
*B
*�B
*�B
+B
+�B
,WB
,�B
./B
.�B
/B
.�B
.}B
.cB
/iB
0oB
1�B
1�B
1�B
2GB
2�B
3MB
3hB
3hB
3hB
3�B
3�B
4B
4B
4TB
4�B
4�B
5%B
5%B
5?B
5?B
5%B
5ZB
5tB
5�B
5�B
6B
6`B
6`B
6�B
7fB
7�B
7�B
8B
8B
8B
8�B
8�B
8�B
8�B
8�B
9rB
9�B
9�B
:xB
:�B
:�B
:�B
:�B
;B
;0B
;0B
;B
;�B
;�B
;�B
<B
<PB
<�B
<�B
=B
=B
="B
=�B
=�B
>BB
>(B
>�B
?B
?cB
?�B
?�B
?�B
@�B
@�B
@�B
A;B
AoB
A�B
A�B
B'B
B�B
B�B
CaB
C�B
DB
DMB
D�B
ESB
E�B
FB
F?B
F�B
G_B
GzB
G�B
G�B
G�B
G�B
G�B
H1B
HfB
H�B
I7B
IlB
I�B
IlB
I�B
I�B
IlB
I�B
I�B
J	B
J	B
J=B
J�B
J�B
KB
K^B
KxB
K�B
K�B
K�B
K�B
K�B
L0B
L�B
L~B
L�B
L�B
MPB
M�B
M�B
N�B
N�B
O(B
OB
O(B
OBB
O\B
OvB
O\B
O\B
OvB
O�B
O�B
O�B
O�B
O�B
O�B
PB
P�B
P�B
P}B
P�B
P�B
Q4B
Q�B
Q�B
Q�B
Q�B
RTB
R�B
R�B
R�B
R�B
S&B
S&B
S&B
S&B
S@B
SuB
SuB
SuB
S�B
S�B
TB
TB
T{B
T�B
T�B
T�B
T�B
U2B
U�B
U�B
V9B
V9B
VmB
V�B
W
B
W$B
W$B
W�B
W�B
X+B
X+B
XEB
X�B
X�B
X�B
YKB
YKB
Y�B
Y�B
Y�B
Z7B
Z7B
Z7B
ZQB
Z�B
Z�B
Z�B
[	B
[#B
[=B
[WB
[qB
[�B
[�B
[�B
\B
\CB
\�B
\�B
]IB
]dB
]IB
^5B
^OB
^�B
^�B
^�B
^�B
^�B
_;B
_VB
_�B
`B
`vB
`vB
`�B
`�B
`�B
aB
aHB
a�B
a�B
bB
bB
b4B
b�B
b�B
b�B
cB
c:B
cTB
cnB
c�B
c�B
c�B
dtB
dtB
dtB
d�B
d�B
d�B
e`B
e`B
ezB
f2B
f�B
f�B
f�B
f�B
gB
gRB
g�B
g�B
g�B
h
B
h$B
h$B
h$B
h>B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
iB
i*B
i*B
i*B
i_B
i_B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j0B
jeB
j�B
kB
kB
kB
k6B
kkB
k�B
k�B
k�B
k�B
lqB
m)B
m)B
m)B
m]B
m�B
nB
n�B
n}B
n�B
n�B
n�B
n�B
oiB
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
q[B
q�B
q�B
q�B
rB
r-B
rGB
raB
raB
r�B
r�B
s3B
sB
shB
s�B
s�B
s�B
s�B
shB
s�B
t�B
t�B
tnB
tB
tB
tnB
t�B
t�B
uZB
utB
utB
u�B
u�B
u�B
u�B
u�B
u�B
vB
vFB
vzB
v�B
v�B
v�B
v�B
w�B
w�B
w�B
xB
w�B
xlB
xRB
x�B
x�B
x�B
y$B
yrB
y�B
zB
z*B
zDB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{JB
{�B
{�B
{�B
{�B
{�B
|6B
|6B
|jB
|�B
|�B
}B
}B
|�B
}<B
}VB
}qB
}�B
~B
~B
~BB
~BB
~wB
~�B
~�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105238  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191947  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191948  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191948                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041955  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041955  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                