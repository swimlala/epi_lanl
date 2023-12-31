CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-16T19:08:24Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ܐ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �L   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �lArgo profile    3.1 1.2 19500101000000  20230616190824  20230616190824  5904711 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  6217                            2B  A   NAVIS_A                         0487                            011514                          863 @��^T�MC1   @��_�l&f@)��x����d#�vȴ91   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @���@�  A   AffA>ffAa��A~ffA�  A���A�  A�  A�33A�33A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�C3Dۀ D�� D�  D�<�D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@��\@��\A�A;�A^�HA{�A���A�p�A���A���A��
A��
A��
A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B��*C�{C�{C�{C�{C	�{C�{C�{C�{C��C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<{�D<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�DwuDw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�=�D�z�Dۺ�D���D�7\D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�z�D⺏D���D�:�D�z�D㺏D���D�:�D�z�D亏D���D�:�D�z�D序D���D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D躏D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�w\D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D�ʏ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A��#A��/A��mA��mA��`A��;A��TA��`A��`A��HA��HA��#A�A�l�A�AζFA�VA��A�ZA��A�A�A�{A���A�r�A�9XA�33A�=qA��+A�I�A�bA�(�A��A��A�t�A�M�A���A��`A���A�`BA��hA��PA���A�A���A���A�A�7LA�?}A�z�A��A�x�A���A�\)A��Ay�;Au�wAp{Ak+Af�AdE�A_7LAXbAT�yAR�AP�`AJM�AE
=AC�#ACG�AB��AA�;A@��A?l�A?%A=��A<M�A:�A9dZA8JA5�A1S�A/��A.�!A.�A-�FA-�mA-�A-��A-�A-�#A-��A-l�A-K�A,��A,n�A,A+�^A+��A+oA*n�A*=qA* �A)�7A)%A(ZA'/A&ZA%O�A$ĜA$5?A$bA#��A"��A"=qA!�A ~�A JA7LA��A�`A��AO�A��AVA-A�mA�^At�A��AI�A�A�
A;dA�RA�!A�A�A9XA�mAS�A;dA7LA�A�!A�A�A`BAK�A?}A33A�A��A5?AƨA�Ar�AM�A�AhsA��A�A��Av�AVAE�AƨAA�DA��A�PA\)A?}A33A�yAA�A��Al�A/AA
��A
z�A
I�A	�;A	��A	?}A��Av�A$�AAO�A�AjA(�AA��A��A?}A��A��A^5A�PA"�A�Av�AbA�A&�A ��A ��A r�A ZA $�@�K�@�+@�"�@�
=@��\@�$�@��h@���@�ƨ@��@�K�@���@�M�@��@��7@�G�@���@�I�@��;@��P@�ȴ@�E�@�{@���@�bN@���@�ƨ@�;d@���@��@�ff@�V@�$�@��#@�@�9@�@�R@�n�@�=q@��#@��@��
@��y@�5?@�7@��`@�@�j@�1'@�S�@��H@�~�@��#@�?}@�dZ@�@�r�@�1@ߕ�@�"�@ޗ�@ݺ^@�O�@�O�@�G�@��/@�b@۝�@�+@��@ٲ-@�hs@�7L@��`@�Z@ם�@�5?@�hs@�?}@�&�@�%@��`@ԋD@Ӯ@�dZ@���@ѡ�@���@�(�@�\)@Η�@�E�@�@���@�p�@���@�Z@�1'@� �@��;@�33@�ȴ@�ff@�-@�&�@�1'@�33@�V@�J@�@�O�@Ĭ@�9X@þw@�;d@°!@�@�n�@�{@�`B@��@�b@��m@���@�o@���@�ff@���@���@�X@���@���@��m@���@��F@��P@�C�@�"�@��+@�{@�X@�Q�@���@��w@��@�l�@�
=@�v�@�{@��^@�hs@��@���@��u@��@�l�@��y@���@�~�@�$�@���@��-@��h@�p�@�/@���@�Z@�1@��w@�|�@�S�@�o@���@�V@��@���@��7@��@���@��D@�Z@��m@�|�@�S�@�33@��@�M�@�J@���@��@�&�@��@��@���@��/@��j@���@�9X@��@��@���@�t�@�@��+@�5?@��@�X@�&�@��j@�r�@�b@�ƨ@�l�@��!@���@��@���@��D@�bN@���@��F@�dZ@�33@�@���@�-@���@�&�@��@�9X@��@�ƨ@���@�|�@�\)@�33@�o@��H@�n�@���@�hs@�?}@��`@��j@�9X@�t�@��H@�E�@��@��-@���@��7@�`B@�?}@��@��u@��@��m@��@�S�@���@�ȴ@���@���@�V@��@��#@���@���@���@���@���@���@�@���@��@�/@���@�1@�t�@�
=@���@���@���@�~�@�=q@�J@��T@���@�O�@���@�A�@���@��@�K�@��@���@�~�@�$�@�@�x�@�`B@�7L@�%@��`@��j@���@��@�r�@�j@�bN@�Q�@�A�@��@��;@���@�33@�"�@�"�@�o@�
=@��@���@���@��+@�E�@��^@�p�@�O�@�/@���@���@�bN@�I�@�1'@�  @�|�@�33@�"�@�
=@���@�ȴ@�^5@�E�@�J@��T@��-@��@��@�Ĝ@�b@+@
=@~�y@~�y@~�@~��@~E�@}��@}`B@|�D@|1@{��@{S�@{33@{"�@z��@z^5@y��@y�@xr�@wl�@wK�@w�@v��@vE�@u@u`B@u�@t�@t�@t��@tz�@tI�@t�@s�m@s�@s33@r��@r~�@r^5@q��@q&�@p��@o��@n��@nv�@nV@n$�@m�@m��@mV@k��@j~�@i�#@h��@hQ�@h  @gK�@g
=@fȴ@f��@e�h@e`B@e?}@eV@d��@d�/@d�j@d�@dI�@c��@b�H@b��@b��@b�\@b�\@b�\@bn�@a�^@a��@ahs@`��@`A�@`b@_�@_K�@_;d@_+@^�R@^V@^{@]�T@]�-@]�h@]`B@\�@\I�@[dZ@[@Z��@Z~�@ZM�@Z-@Y��@YG�@X��@Xr�@X �@X �@W�@W��@W\)@W�@V�@Vv�@V5?@V@U�T@U�T@U��@U@U�@UV@T�D@TI�@T(�@S�@R��@Rn�@R-@Q��@Qhs@Q&�@P�`@P�9@Pr�@Pb@O��@O�P@O;d@N�y@NE�@M�T@M�@MV@L�/@L�j@Lj@L(�@K�F@KS�@K"�@J�H@J��@J�\@J^5@J-@I�@I�@I��@Ix�@IG�@I�@I�@I%@H�`@H�u@H1'@H  @G��@G\)@G+@G
=@F��@FV@F5?@E�h@EO�@E/@E�@D��@D�@D�/@D�@DI�@D9X@D(�@C�
@C@Bn�@B=q@A��@A&�@@ �@?�P@?l�@?\)@?+@>��@>��@>��@>V@>@=�h@=?}@<��@<��@<�D@<j@<Z@<I�@<9X@<1@;�m@;�
@;ƨ@;t�@;33@:��@:-@9�#@8�`@8  @7��@7l�@7;d@7�@6��@6�@6��@6ff@6E�@5�@5�@5/@4�@4z�@4j@4�@3dZ@3o@3o@2�H@2�!@2~�@2�@1�^@1��@1G�@1�@0�`@0��@0Ĝ@0�u@0 �@/��@/;d@/+@/
=@.�@.�+@.v�@.V@.E�@.$�@.$�@-��@,�@,j@+ƨ@+��@*�@*��@*��@*�!@*�\@)��@)&�@)%@(�@(  @'|�@'
=@&ȴ@&�R@&�R@&ȴ@&ȴ@&��@&$�@&@%@%�@%`B@%�@$�@#��@#��@#�@#dZ@#C�@#o@"�H@"~�@"-@"�@"J@!�^@!&�@ �9@ �u@ �@ 1'@��@�P@l�@\)@K�@K�@l�@\)@\)@
=@�@�R@�+@ff@@��@@�h@`B@O�@O�@O�@O�@O�@?}@?}@��@j@(�@1@1@ƨ@�@dZ@C�@"�@��@�H@��@��@�!@-@�#@�7@hs@&�@�@%@�`@��@Ĝ@��@�@bN@A�@b@�w@��@��@��@l�@;d@��@��@v�@ff@$�@@`B@?}@�/@I�@��@�
@�F@��@t�@"�@��@�\@n�@-@J@�^@��@x�@X@X@G�@7L@7L@7L@�@�`@Ĝ@r�@b@  @  @�;@�w@l�@K�@;d@�@ȴ@ȴ@ȴ@ȴ@�R@��@ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A��#A��/A��mA��mA��`A��;A��TA��`A��`A��HA��HA��#A�A�l�A�AζFA�VA��A�ZA��A�A�A�{A���A�r�A�9XA�33A�=qA��+A�I�A�bA�(�A��A��A�t�A�M�A���A��`A���A�`BA��hA��PA���A�A���A���A�A�7LA�?}A�z�A��A�x�A���A�\)A��Ay�;Au�wAp{Ak+Af�AdE�A_7LAXbAT�yAR�AP�`AJM�AE
=AC�#ACG�AB��AA�;A@��A?l�A?%A=��A<M�A:�A9dZA8JA5�A1S�A/��A.�!A.�A-�FA-�mA-�A-��A-�A-�#A-��A-l�A-K�A,��A,n�A,A+�^A+��A+oA*n�A*=qA* �A)�7A)%A(ZA'/A&ZA%O�A$ĜA$5?A$bA#��A"��A"=qA!�A ~�A JA7LA��A�`A��AO�A��AVA-A�mA�^At�A��AI�A�A�
A;dA�RA�!A�A�A9XA�mAS�A;dA7LA�A�!A�A�A`BAK�A?}A33A�A��A5?AƨA�Ar�AM�A�AhsA��A�A��Av�AVAE�AƨAA�DA��A�PA\)A?}A33A�yAA�A��Al�A/AA
��A
z�A
I�A	�;A	��A	?}A��Av�A$�AAO�A�AjA(�AA��A��A?}A��A��A^5A�PA"�A�Av�AbA�A&�A ��A ��A r�A ZA $�@�K�@�+@�"�@�
=@��\@�$�@��h@���@�ƨ@��@�K�@���@�M�@��@��7@�G�@���@�I�@��;@��P@�ȴ@�E�@�{@���@�bN@���@�ƨ@�;d@���@��@�ff@�V@�$�@��#@�@�9@�@�R@�n�@�=q@��#@��@��
@��y@�5?@�7@��`@�@�j@�1'@�S�@��H@�~�@��#@�?}@�dZ@�@�r�@�1@ߕ�@�"�@ޗ�@ݺ^@�O�@�O�@�G�@��/@�b@۝�@�+@��@ٲ-@�hs@�7L@��`@�Z@ם�@�5?@�hs@�?}@�&�@�%@��`@ԋD@Ӯ@�dZ@���@ѡ�@���@�(�@�\)@Η�@�E�@�@���@�p�@���@�Z@�1'@� �@��;@�33@�ȴ@�ff@�-@�&�@�1'@�33@�V@�J@�@�O�@Ĭ@�9X@þw@�;d@°!@�@�n�@�{@�`B@��@�b@��m@���@�o@���@�ff@���@���@�X@���@���@��m@���@��F@��P@�C�@�"�@��+@�{@�X@�Q�@���@��w@��@�l�@�
=@�v�@�{@��^@�hs@��@���@��u@��@�l�@��y@���@�~�@�$�@���@��-@��h@�p�@�/@���@�Z@�1@��w@�|�@�S�@�o@���@�V@��@���@��7@��@���@��D@�Z@��m@�|�@�S�@�33@��@�M�@�J@���@��@�&�@��@��@���@��/@��j@���@�9X@��@��@���@�t�@�@��+@�5?@��@�X@�&�@��j@�r�@�b@�ƨ@�l�@��!@���@��@���@��D@�bN@���@��F@�dZ@�33@�@���@�-@���@�&�@��@�9X@��@�ƨ@���@�|�@�\)@�33@�o@��H@�n�@���@�hs@�?}@��`@��j@�9X@�t�@��H@�E�@��@��-@���@��7@�`B@�?}@��@��u@��@��m@��@�S�@���@�ȴ@���@���@�V@��@��#@���@���@���@���@���@���@�@���@��@�/@���@�1@�t�@�
=@���@���@���@�~�@�=q@�J@��T@���@�O�@���@�A�@���@��@�K�@��@���@�~�@�$�@�@�x�@�`B@�7L@�%@��`@��j@���@��@�r�@�j@�bN@�Q�@�A�@��@��;@���@�33@�"�@�"�@�o@�
=@��@���@���@��+@�E�@��^@�p�@�O�@�/@���@���@�bN@�I�@�1'@�  @�|�@�33@�"�@�
=@���@�ȴ@�^5@�E�@�J@��T@��-@��@��@�Ĝ@�b@+@
=@~�y@~�y@~�@~��@~E�@}��@}`B@|�D@|1@{��@{S�@{33@{"�@z��@z^5@y��@y�@xr�@wl�@wK�@w�@v��@vE�@u@u`B@u�@t�@t�@t��@tz�@tI�@t�@s�m@s�@s33@r��@r~�@r^5@q��@q&�@p��@o��@n��@nv�@nV@n$�@m�@m��@mV@k��@j~�@i�#@h��@hQ�@h  @gK�@g
=@fȴ@f��@e�h@e`B@e?}@eV@d��@d�/@d�j@d�@dI�@c��@b�H@b��@b��@b�\@b�\@b�\@bn�@a�^@a��@ahs@`��@`A�@`b@_�@_K�@_;d@_+@^�R@^V@^{@]�T@]�-@]�h@]`B@\�@\I�@[dZ@[@Z��@Z~�@ZM�@Z-@Y��@YG�@X��@Xr�@X �@X �@W�@W��@W\)@W�@V�@Vv�@V5?@V@U�T@U�T@U��@U@U�@UV@T�D@TI�@T(�@S�@R��@Rn�@R-@Q��@Qhs@Q&�@P�`@P�9@Pr�@Pb@O��@O�P@O;d@N�y@NE�@M�T@M�@MV@L�/@L�j@Lj@L(�@K�F@KS�@K"�@J�H@J��@J�\@J^5@J-@I�@I�@I��@Ix�@IG�@I�@I�@I%@H�`@H�u@H1'@H  @G��@G\)@G+@G
=@F��@FV@F5?@E�h@EO�@E/@E�@D��@D�@D�/@D�@DI�@D9X@D(�@C�
@C@Bn�@B=q@A��@A&�@@ �@?�P@?l�@?\)@?+@>��@>��@>��@>V@>@=�h@=?}@<��@<��@<�D@<j@<Z@<I�@<9X@<1@;�m@;�
@;ƨ@;t�@;33@:��@:-@9�#@8�`@8  @7��@7l�@7;d@7�@6��@6�@6��@6ff@6E�@5�@5�@5/@4�@4z�@4j@4�@3dZ@3o@3o@2�H@2�!@2~�@2�@1�^@1��@1G�@1�@0�`@0��@0Ĝ@0�u@0 �@/��@/;d@/+@/
=@.�@.�+@.v�@.V@.E�@.$�@.$�@-��@,�@,j@+ƨ@+��@*�@*��@*��@*�!@*�\@)��@)&�@)%@(�@(  @'|�@'
=@&ȴ@&�R@&�R@&ȴ@&ȴ@&��@&$�@&@%@%�@%`B@%�@$�@#��@#��@#�@#dZ@#C�@#o@"�H@"~�@"-@"�@"J@!�^@!&�@ �9@ �u@ �@ 1'@��@�P@l�@\)@K�@K�@l�@\)@\)@
=@�@�R@�+@ff@@��@@�h@`B@O�@O�@O�@O�@O�@?}@?}@��@j@(�@1@1@ƨ@�@dZ@C�@"�@��@�H@��@��@�!@-@�#@�7@hs@&�@�@%@�`@��@Ĝ@��@�@bN@A�@b@�w@��@��@��@l�@;d@��@��@v�@ff@$�@@`B@?}@�/@I�@��@�
@�F@��@t�@"�@��@�\@n�@-@J@�^@��@x�@X@X@G�@7L@7L@7L@�@�`@Ĝ@r�@b@  @  @�;@�w@l�@K�@;d@�@ȴ@ȴ@ȴ@ȴ@�R@��@ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A�ƨA�ĜA�A���AܾwAܸRAܶFAܶFAܴ9Aܲ-Aܲ-AܾwA��/A�$�AݬAޅA޸RA��HA�t�Aߧ�A���A�oA�JA��A��/A�E�A��A��A���A�A�^A�ffA�wA�hsAᝲA�RA� �A� �A�VA�+A��A���A���A��A���A��A���A��A�
=A�1A���A��A�K�A�\)A㕁A�A��A��HA��A�I�A�r�A�z�A�`BA��A�%A��A� �A��A�(�A�?}A�^5A�^5A�r�A�PA噚A�A�9A���A�&�A�M�A�r�A�uA�A�FA�ƨA��yA��A���A�{A��A��A�$�A�+A�33A�7LA�A�A�ffA�z�A�|�A�z�A�+A�PA�hA��A�^A���A��
A��HA��/A��HA��mA��A���A��A��A�33A�7LA�1'A�/A�K�A�^5A�bNA�dZA�hsA�hsA�l�A�r�A�A�A�+A蕁A��A��A��A��A�!A�FA�ƨA�ȴA�ƨA�A�ȴA��#A��TA��`A��mA��mA��`A��HA��TA��A��A���A�
=A�
=A�bA��A�$�A�&�A�+A�-A�/A�(�A�-A�9XA�=qA�M�A�Q�A�VA�XA�VA�VA�\)A�bNA�hsA�n�A�p�A�t�A�x�A�|�A�+A�DA�PA�uA闍A陚A雦A��A��A�-A�RA�RA�RA�FA���A�A���A�ĜA���A��#A��#A��HA��yA��A���A�A�%A�1A�1A�
=A��A��A��A��A��A��A� �A�$�A�1'A�33A�5?A�7LA�=qA�?}A�C�A�E�A�I�A�K�A�O�A�O�A�XA�\)A�\)A�^5A�l�A�p�A�p�A�v�A�x�A�|�A�A�A�A�A�A�7A�uAꝲAꟾAꝲAꝲA��A�A�9A�^A���A�ȴA���A���A���A���A��
A���A���A���A��;A��A���A�A�A�1A�JA��A��A��A��A��A�&�A�-A�-A�7LA�;dA�=qA�?}A�A�A�C�A�E�A�XA�bNA�dZA�ffA�ffA�ffA�ffA�l�A�l�A�n�A�z�A�A�+A�PA뗍A뙚A뛦A띲A럾A��A�A�A�A�A�9A�FA�RA�9A�wA�ĜA���A��#A��/A��;A��HA��yA��A��A���A���A���A���A���A�A�VA��A��A��A��A� �A� �A�$�A�"�A�"�A�$�A�$�A�{A���A��B �B�B�5BD�B�oBǮB��B�B:^B]/B� B��B��B�5B��B	7B�B2-BA�BN�BQ�BG�B�
A�E�A�VA�M�A�VA�O�A�ZA�S�A�\)A�^5A�^5A�dZA�hsA�l�A�oB|�BcTA���A�B �-B�BaHB�VB�BÖB�B�`B��BB\B�B%�B5?BA�BJ�B\)Bn�Bw�B�B�7B��B��B�B�XBÖB��B�ZB�B��BBJB{B�B$�B-B33B;dBA�BI�BL�BO�BO�BN�BG�B=qBbB`BA� �A�&�BDB��B��BuBM�Bn�B�B�=B�hB��B��B��B�B�?B�XB�qBBǮB��B��B�B�;B�ZB�sB�B�B�B��B��B��BB+BDBbB�B�B �B#�B'�B-B/B2-B5?B9XB=qBA�BD�BI�BN�BR�BXB^5BffBm�Br�Bu�Bx�Bz�B|�B�B�B�+B�=B�PB�hB��B��B��B��B��B�B�B�-B�FB�^B�wB��BĜBƨBɺB��B��B��B��B�B�B�/B�BB�TB�fB�B�B�B�B�B��B��B��B��B��B	B		7B	JB	VB	hB	uB	�B	�B	�B	�B	 �B	#�B	$�B	&�B	'�B	)�B	-B	/B	1'B	49B	5?B	:^B	<jB	>wB	C�B	H�B	I�B	K�B	M�B	O�B	Q�B	T�B	YB	\)B	`BB	dZB	hsB	k�B	m�B	o�B	q�B	s�B	v�B	y�B	{�B	�B	�B	�B	�B	�+B	�=B	�DB	�PB	�VB	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�FB	�XB	�dB	�qB	�}B	��B	B	ÖB	ĜB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�#B	�)B	�/B	�;B	�BB	�NB	�ZB	�`B	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B

=B
DB
DB
JB
VB
\B
bB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
#�B
$�B
%�B
&�B
'�B
(�B
)�B
,B
,B
-B
.B
/B
0!B
0!B
1'B
2-B
49B
49B
49B
5?B
6FB
7LB
8RB
9XB
:^B
;dB
;dB
=qB
=qB
>wB
@�B
A�B
B�B
B�B
C�B
D�B
D�B
E�B
F�B
G�B
G�B
G�B
I�B
K�B
K�B
L�B
N�B
P�B
R�B
S�B
S�B
T�B
VB
VB
W
B
XB
YB
ZB
\)B
]/B
^5B
^5B
_;B
`BB
aHB
bNB
bNB
cTB
cTB
cTB
e`B
ffB
gmB
hsB
hsB
k�B
n�B
o�B
p�B
q�B
q�B
r�B
r�B
s�B
t�B
t�B
t�B
u�B
u�B
w�B
x�B
x�B
y�B
{�B
|�B
}�B
� B
� B
� B
�B
�B
�B
�B
�+B
�+B
�1B
�1B
�7B
�=B
�DB
�PB
�PB
�PB
�VB
�\B
�bB
�bB
�bB
�hB
�hB
�hB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�'B
�'B
�-B
�-B
�3B
�9B
�?B
�FB
�FB
�FB
�LB
�RB
�XB
�^B
�^B
�dB
�jB
�qB
�qB
�wB
�}B
�}B
��B
��B
B
ÖB
ĜB
ŢB
ƨB
ƨB
ǮB
ȴB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�
B
�B
�B
�B
�B
�#B
�)B
�/B
�5B
�5B
�;B
�BB
�BB
�HB
�HB
�NB
�NB
�TB
�ZB
�`B
�fB
�fB
�fB
�fB
�mB
�sB
�yB
�yB
�yB
�B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  BBBBBBBB%B+B+B+B+B1B1B	74444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  Aܛ�Aܝ�Aܝ�Aܛ�Aܙ�Aܗ�Aܕ�AܑiA܏]A܍PA܋DA܉8A܃A܁A܁A�~�A�|�A�|�A܉8Aܧ�A��A�v�A�O�AރAެ	A�?}A�r�Aߝ�A��0A��A߶FAߧ�A�cA�M�A�O�A�fgA��PA��A�1'A��8A�34A�hsA�A��A��A� �A�Q�A�p�A❳A⛦A�r�A♚A�RA�ĜA��TA���A���A�ȵA��HA��A�&�A�`BA�PA�_A�	A�wA�{A�=qA�E�A�+A�l�A���A��aA��A��mA��A�
>A�(�A�(�A�=qA�XA�d[A�v�A�~�A嗎A��A��A�=qA�^6A�v�A�A�iA�:A�wA�ƨA��<A��yA��aA��A���A���A�A�JA�1'A�E�A�G�A�E�A�Q�A�XA�\)A�t�A�A痎A��A�	A��A�	A�.A�kA�ȵA��HA��aA���A�A���A���A��A�(�A�-A�/A�34A�34A�7LA�=qA�O�A�O�A�Q�A�`BA�n�A�p�A�p�A�n�A�z�A�A�iA�uA�iA�PA�uA��A�A�!A�.A�.A�!A�	A�A�RA�kA�ƨA���A���A��#A��aA��A��A���A���A���A��A���A�A�2A��A��A� �A�"�A� �A� �A�&�A�-A�34A�9YA�;eA�?}A�C�A�G�A�Q�A�VA�XA�^6A�bNA�d[A�fgA�n�A�r�A�|�A�A�A�A�A�DA�PA�DA�]A靳A��A��A�	A�:A�_A�ĜA���A���A���A���A���A��TA��aA��aA��aA��mA��yA��A��A���A���A�  A�A�2A�
>A�WA�cA�{A��A��A��A�"�A�&�A�&�A�(�A�7LA�;eA�;eA�A�A�C�A�G�A�K�A�K�A�K�A�M�A�M�A�S�A�^6A�hsA�jA�hsA�hsA�l�A�v�A�~�A�A�DA�uAꕂAꗎAꕂAꝳA��AꟿAꟿAꝳA��A�wA�ȵA���A���A���A��A��HA��yA��mA��mA��yA��A���A���A�A�%A�2A�
>A�JA�WA�cA�"�A�-A�/A�1'A�1'A�1'A�1'A�7LA�7LA�9YA�E�A�M�A�Q�A�XA�bNA�d[A�fgA�hsA�jA�n�A�v�A�x�A�x�A�x�A�~�A�A�A�~�A�8A�]A뛦A��A��A��A�	A�:A�RA�kA�A�ȵA�ȵA�ȵA�ȵA���A��A��HA��HA��TA��mA��A��A��A��A��A��A��A��<A�ĜA��aB ��BjBÕB)�Bw�B�B�B��B�BB�Be`B�B��BÕB�B�BB�B&�B49B7LB-B�jA�cA� �A��A� �A��A�$�A��A�&�A�(�A�(�A�/A�34A�7LA��0BbNBH�A�fgA�M�B ��B��BF�Bs�B�hB��B�dB��B�B�lB��B��BCB�B&�B0!BA�BS�B]/BffBn�Bz�B�1B�tB��B��B�RBɺB�B�AB�B�B��BB
=BnB�B �B&�B/B2-B5?B5?B49B-B"�B��BE�A��A��B�B�B�3B��B33BS�BffBo�Bv�B{�B�B�7B�bB��B��B��B��B�B�-B�XB�}BěBɺB��B��B��B�
B�#B�5B�TB�yB�B�B��B��BB%B	7BPBnB{B�B�B�B"�B&�B)�B/B49B8RB=pBC�BK�BR�BXB[#B^5B`ABbNBffBiyBl�Bo�Br�Bv�B{�B�B�B�1B�IB�bB�tB��B��B��B��B��B��B�B�B�-B�9B�LB�^B�pB�}BBŢBȴB��B��B��B��B�B�B�B�/B�;B�GB�TB�yB�B�B�B��B��B��B��B	  B	B	%B		7B	
=B	IB	PB	\B	nB	{B	�B	�B	�B	�B	!�B	#�B	(�B	.B	/B	1'B	33B	5?B	7LB	:^B	>wB	A�B	E�B	I�B	M�B	P�B	R�B	T�B	W
B	YB	\)B	_;B	aGB	ffB	glB	hrB	jB	l�B	o�B	p�B	r�B	s�B	u�B	v�B	w�B	x�B	y�B	z�B	|�B	}�B	�B	�B	�B	�B	�+B	�1B	�CB	�VB	�bB	�hB	�nB	�tB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�-B	�3B	�3B	�9B	�EB	�XB	�^B	�dB	�jB	�jB	�pB	�pB	��B	��B	��B	B	ěB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�)B	�/B	�5B	�AB	�GB	�NB	�TB	�TB	�ZB	�`B	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
1B
	7B
	7B

=B
CB
IB
PB
VB
\B
hB
hB
nB
tB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
"�B
"�B
#�B
%�B
&�B
'�B
'�B
(�B
)�B
)�B
+B
,B
-B
-B
-B
/B
1'B
1'B
2-B
49B
6EB
8RB
9XB
9XB
:^B
;dB
;dB
<jB
=pB
>wB
?}B
A�B
B�B
C�B
C�B
D�B
E�B
F�B
G�B
G�B
H�B
H�B
H�B
J�B
K�B
L�B
M�B
M�B
P�B
S�B
T�B
VB
W
B
W
B
XB
XB
YB
ZB
ZB
ZB
[#B
[#B
]/B
^5B
^5B
_;B
aGB
bNB
cTB
e`B
e`B
e`B
ffB
glB
hrB
jB
l�B
l�B
m�B
m�B
n�B
o�B
p�B
r�B
r�B
r�B
s�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
x�B
y�B
{�B
{�B
~�B
~�B
� B
� B
~�B
�B
�B
�B
�B
�%B
�+B
�7B
�=B
�CB
�IB
�IB
�IB
�PB
�\B
�bB
�bB
�hB
�hB
�hB
�nB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�!B
�'B
�'B
�-B
�-B
�-B
�3B
�3B
�3B
�9B
�?B
�EB
�LB
�LB
�RB
�XB
�^B
�^B
�dB
�jB
�jB
�pB
�pB
�pB
�}B
��B
��B
B
ÕB
ÕB
ěB
ŢB
ŢB
ƨB
ƨB
ǮB
ǮB
ȴB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�
B
�
B
�
B
�B
�B
�B
�#B
�#B
�)B
�/B
�/B
�5B
�;B
�;B
�AB
�AB
�GB
�GB
�GB
�NB
�TB
�ZB
�`B
�fB
�fB
�fB
�lB
�rB
�yB
�yB
�B
�B
�B
�B
�B
�B
�B
�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0260000                                                                                                                                                                                                                                    Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                       PSAL ADJUST [dd mm yyyy N S_off stddev] 02 04 2020 160 -0.0260000 0.0000 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20230616190824              20230616190824  AO  ARCAADJP                                                                    20230616190824    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20230616190824    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230616190824  QCP$                G�O�G�O�G�O�1B83E           AO  ARGQQCPL                                                                    20230616190824  QCF$                G�O�G�O�G�O�8800            