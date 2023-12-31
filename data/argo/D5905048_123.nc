CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-05-28T00:35:35Z creation;2017-05-28T00:35:38Z conversion to V3.1;2019-12-19T08:06:27Z update;     
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
resolution        =���   axis      Z        h  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  sh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170528003535  20200116211516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               {A   JA  I2_0577_123                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�
�'qf 1   @�
��-� @2�u%F
��d^����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D.��D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D�ɚ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @{�@��\@��\AG�A=G�A^�HA}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bè�B��)B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9��C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��qC��qC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�DwuDw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�z�D⺏D���D�:�D�z�D㺏D���D�:�D�z�D亏D���D�:�D�z�D序D��\D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D躏D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�}�D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�G�A�5?A��A��A��A��A��A�{A�oA���A�ƨAմ9AծAՙ�AՍPA�x�A�dZA�XA�Q�A�I�A�E�A�?}A�7LA�5?A�33A�1'A�-A�VAԛ�A��AӓuA��/A��HAЁA���A�
=A��TA�jA��A�t�A��Aʟ�A�I�A���A���Aɛ�A�Q�A���Aȏ\A�~�A��AǸRAǁA��A�A�Ař�A�C�A��A�jA��/A�ZA���A�oA���A�n�A��-A���A�XA��A��hA���A�ĜA�n�A�n�A�^5A��yA��A�7LA�A�"�A��7A���A�r�A�`BA���A�ȴA���A��A�VA�bA��A�dZA�O�A�&�A��A� �A�oA�\)A���A�(�A�r�A���A���A��9A��9A���A�"�A�~�A��A���A���A���A��yA�-A��A���A���A�\)A��jA�-A�z�A���A�`BA��\A�&�A�C�A��A��FA�1A��A}��Az��Av1'As+ApĜAk��Ai��Ah��Ag��Af�Af�Af �Ad^5A`��A_�hA]��AXĜAT��AS"�ARn�AO�AM�AL�AJ=qAI|�AHI�AG;dAE+AB�+AAt�A?�
A<�/A;��A:�A8��A6^5A5|�A4��A4��A3dZA1��A1;dA0JA/&�A,��A*A(�A'�A%�A#O�A" �A z�A�A$�AE�A�FA1'A�\A��A��A�jA��AE�A�FAȴA%Av�A  A��AK�AjA�A7LA�yA�jAz�A�A�9AJA&�A
�DA	��A	�A	hsA	�A�A�HA�\AE�AbA  A�mA�AjA  A��A�A��AK�A?}A �/A ��A ȴA Ĝ@��@��@��@�V@�x�@�j@���@���@�~�@��@�G�@�dZ@���@�M�@��#@���@�V@�|�@�n�@�@�G�@��D@�;d@�
=@�9X@���@�@���@�b@�$�@ԓu@�V@ְ!@Ցh@��
@�dZ@�S�@�l�@�|�@���@ѡ�@�j@Ϯ@�dZ@�@�^5@���@�C�@���@͙�@�Z@���@ȃ@���@�?}@�I�@�I�@Ý�@Å@�33@��H@��^@��@���@�bN@��F@��H@���@�@��^@��/@��w@���@�7L@���@��@���@���@��u@�I�@���@��@�(�@��9@�7L@�/@�/@�?}@�X@�  @��@�I�@�dZ@��@�5?@�p�@�/@���@��@�1@���@�C�@�ȴ@���@��\@�{@���@��7@�/@�V@��@���@�bN@�Z@��
@��@�v�@��T@���@�?}@�7L@�7L@���@��@��`@���@���@�1@���@�l�@�dZ@�S�@�@���@�ff@�V@�G�@�Ĝ@���@�z�@�r�@�j@�Q�@�A�@�Z@�j@�A�@��
@� �@��@��@�"�@�o@�\)@�"�@��\@�~�@�5?@��@��^@��7@��h@��7@���@���@�x�@�?}@���@�j@�b@�;d@���@���@���@�ff@�=q@���@��-@�p�@�hs@�X@�/@�&�@���@��j@���@�z�@�A�@��@���@�dZ@�C�@�+@��@�
=@�ȴ@��\@�5?@��@���@�p�@�7L@���@���@�Ĝ@�z�@�1'@� �@��@�b@��@��;@��@��H@�n�@�-@��@��7@�G�@���@��D@�r�@�A�@���@�ƨ@���@���@��@�\)@��@�ff@�M�@�{@�@�hs@�?}@��j@�Z@�1@���@��@��@�ƨ@���@�|�@�33@�o@�
=@���@��y@��y@��H@��@��@���@���@���@��!@���@�~�@�E�@�@���@��@�&�@���@���@��`@���@��u@�z�@��@��;@���@�ƨ@�ƨ@��@�dZ@�33@�o@�@��@���@�=q@�{@���@�X@��@��@�%@�%@��@�z�@� �@��@�t�@�33@�o@��@���@�V@�@��T@�p�@�O�@�/@���@��/@��j@��u@�j@�9X@��@\)@;d@;d@~�R@}�T@}/@|��@|�D@|�@{�F@{��@{o@z��@z=q@y�7@xĜ@xb@w�@w|�@w
=@v�y@v��@vff@v5?@v@u��@up�@u?}@u�@t�/@t9X@t1@s�m@sƨ@s��@s@r��@r��@r��@rM�@qhs@p��@pA�@o�@o��@o;d@o
=@n�+@n5?@n@m@m�@m?}@m�@l�@lz�@l1@k�
@k��@kC�@j�H@j�\@i��@ix�@i7L@hĜ@hQ�@h �@h  @g�;@g�@gl�@g�@fv�@e�-@e?}@d�@d9X@c�
@ct�@c33@b�H@bM�@a�@a�^@aX@`bN@_l�@_
=@^�+@^{@]�@]��@]�-@]�@]O�@]�@]�@\��@\��@\Z@[�@Z�H@Z~�@ZM�@Y��@Y��@YG�@X�`@X�9@X�@X  @W�P@V�@Vff@V$�@V@U�-@U?}@S�
@St�@So@R��@R~�@Rn�@R�@Q��@PQ�@O��@O�w@O�w@O�@O�P@O|�@O�@Nȴ@N�R@N��@N��@N�+@Nff@N$�@N@M@M�@MO�@L�@LZ@K�@KdZ@KdZ@K33@J�!@JM�@I�@Ix�@H��@H��@H��@Hr�@G;d@Fff@FE�@F5?@E�@E�@DZ@C�
@CS�@C@B�H@B��@A�@AG�@A�@@�`@@�u@@bN@@A�@@ �@@  @?|�@?+@>��@>v�@>E�@>{@>@=��@=�@<�@<�D@<j@<I�@;��@;�m@;�F@:��@:~�@:n�@:n�@:^5@:�@9�7@97L@9%@8��@8Q�@8  @7��@7�@7|�@7+@6��@6��@6v�@6E�@6E�@65?@6@5�-@5`B@4�j@4Z@4(�@41@41@3��@3�m@3��@3S�@333@3o@2��@2n�@2M�@2J@1��@1�7@1hs@17L@1�@1%@0�9@0r�@0bN@0Q�@01'@0b@/��@/K�@.�@.V@.$�@-��@-p�@-`B@-O�@-?}@,�@,�@,z�@,9X@+ƨ@+�F@+��@+"�@+@*�\@*J@)�#@)��@)X@)%@(Ĝ@(r�@(bN@(A�@( �@'��@'�P@'\)@'�@&�y@&ȴ@&��@&��@&v�@&5?@%�@%�@%?}@$�@$��@$�j@$z�@$I�@$9X@#�m@#��@#�@#t�@#t�@#S�@#33@#@"�H@"��@"�!@"�!@"�\@"n�@"^5@"�@!�#@!��@!�^@!x�@!%@ ��@ �`@ ��@ �@ r�@ bN@ bN@ Q�@ A�@ 1'@  �@��@��@�P@�P@\)@\)@�@�@��@�+@V@@��@@�h@O�@V@��@�@�j@�j@z�@I�@(�@1@1@��@��@�F@"�@��@=q@J@��@��@�7@hs@&�@��@Ĝ@��@�u@�@�@�@A�@b@�@��@��@K�@
=@�y@�y@ȴ@�R@��@V@$�@�T@��@�@/@��@��@��@9X@1@�m@�
@ƨ@��@C�@"�@o@@�H@��@�!@��@~�@�#@x�@X@��@��@�9@�@ �@b@b@��@��@�P@|�@\)@��@�R@�+@V@@�T@�-@�h@�h@`B@/@�/@�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�G�A�5?A��A��A��A��A��A�{A�oA���A�ƨAմ9AծAՙ�AՍPA�x�A�dZA�XA�Q�A�I�A�E�A�?}A�7LA�5?A�33A�1'A�-A�VAԛ�A��AӓuA��/A��HAЁA���A�
=A��TA�jA��A�t�A��Aʟ�A�I�A���A���Aɛ�A�Q�A���Aȏ\A�~�A��AǸRAǁA��A�A�Ař�A�C�A��A�jA��/A�ZA���A�oA���A�n�A��-A���A�XA��A��hA���A�ĜA�n�A�n�A�^5A��yA��A�7LA�A�"�A��7A���A�r�A�`BA���A�ȴA���A��A�VA�bA��A�dZA�O�A�&�A��A� �A�oA�\)A���A�(�A�r�A���A���A��9A��9A���A�"�A�~�A��A���A���A���A��yA�-A��A���A���A�\)A��jA�-A�z�A���A�`BA��\A�&�A�C�A��A��FA�1A��A}��Az��Av1'As+ApĜAk��Ai��Ah��Ag��Af�Af�Af �Ad^5A`��A_�hA]��AXĜAT��AS"�ARn�AO�AM�AL�AJ=qAI|�AHI�AG;dAE+AB�+AAt�A?�
A<�/A;��A:�A8��A6^5A5|�A4��A4��A3dZA1��A1;dA0JA/&�A,��A*A(�A'�A%�A#O�A" �A z�A�A$�AE�A�FA1'A�\A��A��A�jA��AE�A�FAȴA%Av�A  A��AK�AjA�A7LA�yA�jAz�A�A�9AJA&�A
�DA	��A	�A	hsA	�A�A�HA�\AE�AbA  A�mA�AjA  A��A�A��AK�A?}A �/A ��A ȴA Ĝ@��@��@��@�V@�x�@�j@���@���@�~�@��@�G�@�dZ@���@�M�@��#@���@�V@�|�@�n�@�@�G�@��D@�;d@�
=@�9X@���@�@���@�b@�$�@ԓu@�V@ְ!@Ցh@��
@�dZ@�S�@�l�@�|�@���@ѡ�@�j@Ϯ@�dZ@�@�^5@���@�C�@���@͙�@�Z@���@ȃ@���@�?}@�I�@�I�@Ý�@Å@�33@��H@��^@��@���@�bN@��F@��H@���@�@��^@��/@��w@���@�7L@���@��@���@���@��u@�I�@���@��@�(�@��9@�7L@�/@�/@�?}@�X@�  @��@�I�@�dZ@��@�5?@�p�@�/@���@��@�1@���@�C�@�ȴ@���@��\@�{@���@��7@�/@�V@��@���@�bN@�Z@��
@��@�v�@��T@���@�?}@�7L@�7L@���@��@��`@���@���@�1@���@�l�@�dZ@�S�@�@���@�ff@�V@�G�@�Ĝ@���@�z�@�r�@�j@�Q�@�A�@�Z@�j@�A�@��
@� �@��@��@�"�@�o@�\)@�"�@��\@�~�@�5?@��@��^@��7@��h@��7@���@���@�x�@�?}@���@�j@�b@�;d@���@���@���@�ff@�=q@���@��-@�p�@�hs@�X@�/@�&�@���@��j@���@�z�@�A�@��@���@�dZ@�C�@�+@��@�
=@�ȴ@��\@�5?@��@���@�p�@�7L@���@���@�Ĝ@�z�@�1'@� �@��@�b@��@��;@��@��H@�n�@�-@��@��7@�G�@���@��D@�r�@�A�@���@�ƨ@���@���@��@�\)@��@�ff@�M�@�{@�@�hs@�?}@��j@�Z@�1@���@��@��@�ƨ@���@�|�@�33@�o@�
=@���@��y@��y@��H@��@��@���@���@���@��!@���@�~�@�E�@�@���@��@�&�@���@���@��`@���@��u@�z�@��@��;@���@�ƨ@�ƨ@��@�dZ@�33@�o@�@��@���@�=q@�{@���@�X@��@��@�%@�%@��@�z�@� �@��@�t�@�33@�o@��@���@�V@�@��T@�p�@�O�@�/@���@��/@��j@��u@�j@�9X@��@\)@;d@;d@~�R@}�T@}/@|��@|�D@|�@{�F@{��@{o@z��@z=q@y�7@xĜ@xb@w�@w|�@w
=@v�y@v��@vff@v5?@v@u��@up�@u?}@u�@t�/@t9X@t1@s�m@sƨ@s��@s@r��@r��@r��@rM�@qhs@p��@pA�@o�@o��@o;d@o
=@n�+@n5?@n@m@m�@m?}@m�@l�@lz�@l1@k�
@k��@kC�@j�H@j�\@i��@ix�@i7L@hĜ@hQ�@h �@h  @g�;@g�@gl�@g�@fv�@e�-@e?}@d�@d9X@c�
@ct�@c33@b�H@bM�@a�@a�^@aX@`bN@_l�@_
=@^�+@^{@]�@]��@]�-@]�@]O�@]�@]�@\��@\��@\Z@[�@Z�H@Z~�@ZM�@Y��@Y��@YG�@X�`@X�9@X�@X  @W�P@V�@Vff@V$�@V@U�-@U?}@S�
@St�@So@R��@R~�@Rn�@R�@Q��@PQ�@O��@O�w@O�w@O�@O�P@O|�@O�@Nȴ@N�R@N��@N��@N�+@Nff@N$�@N@M@M�@MO�@L�@LZ@K�@KdZ@KdZ@K33@J�!@JM�@I�@Ix�@H��@H��@H��@Hr�@G;d@Fff@FE�@F5?@E�@E�@DZ@C�
@CS�@C@B�H@B��@A�@AG�@A�@@�`@@�u@@bN@@A�@@ �@@  @?|�@?+@>��@>v�@>E�@>{@>@=��@=�@<�@<�D@<j@<I�@;��@;�m@;�F@:��@:~�@:n�@:n�@:^5@:�@9�7@97L@9%@8��@8Q�@8  @7��@7�@7|�@7+@6��@6��@6v�@6E�@6E�@65?@6@5�-@5`B@4�j@4Z@4(�@41@41@3��@3�m@3��@3S�@333@3o@2��@2n�@2M�@2J@1��@1�7@1hs@17L@1�@1%@0�9@0r�@0bN@0Q�@01'@0b@/��@/K�@.�@.V@.$�@-��@-p�@-`B@-O�@-?}@,�@,�@,z�@,9X@+ƨ@+�F@+��@+"�@+@*�\@*J@)�#@)��@)X@)%@(Ĝ@(r�@(bN@(A�@( �@'��@'�P@'\)@'�@&�y@&ȴ@&��@&��@&v�@&5?@%�@%�@%?}@$�@$��@$�j@$z�@$I�@$9X@#�m@#��@#�@#t�@#t�@#S�@#33@#@"�H@"��@"�!@"�!@"�\@"n�@"^5@"�@!�#@!��@!�^@!x�@!%@ ��@ �`@ ��@ �@ r�@ bN@ bN@ Q�@ A�@ 1'@  �@��@��@�P@�P@\)@\)@�@�@��@�+@V@@��@@�h@O�@V@��@�@�j@�j@z�@I�@(�@1@1@��@��@�F@"�@��@=q@J@��@��@�7@hs@&�@��@Ĝ@��@�u@�@�@�@A�@b@�@��@��@K�@
=@�y@�y@ȴ@�R@��@V@$�@�T@��@�@/@��@��@��@9X@1@�m@�
@ƨ@��@C�@"�@o@@�H@��@�!@��@~�@�#@x�@X@��@��@�9@�@ �@b@b@��@��@�P@|�@\)@��@�R@�+@V@@�T@�-@�h@�h@`B@/@�/@�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
�B
{B
{B
{B
{B
{B
{B
uB
{B
uB
uB
{B
�B
�B
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
#�B
/B
9XB
B�B
dZB
s�B
y�B
�DB
��B
�FB
ȴB
��B
�
B
�yB
�B  BVBuB�B$�B@�BL�BQ�B\)BdZBjB|�B�hBB��B�B��B �BA�BYBz�B}�B� B�1B�JB�=B�JB�DB�1B�+B�DB�JB�VB��B��B��B��B��B��B��B�+By�B]/BK�BH�BQ�BQ�BVBR�BR�BR�BQ�BP�BI�B;dB7LB.B%�B�BuBJB+BB��B�B�yB�`B�NB�;B�B�BB�ZB�/B��B�9B�DBp�BiyBVBA�B+B�BDB
�B
��B
�^B
�!B
��B
� B
k�B
H�B
2-B
"�B	��B	�B	�ZB	�5B	�B	��B	��B	ƨB	�B	��B	�{B	v�B	ZB	M�B	J�B	:^B	/B	(�B	�B	�B	uB	VB	B��B�B�sB�)B��BƨBB�?B�-B�-B�RB�RB�FB�LB�?B�'B�-B�B��B��B��B��B��B��B�{B�hB��B��B��B�B�B�B�B�B�B�!B�-B�3B�LB��BBÖBŢBƨBŢBĜBĜBĜBƨBĜBǮB��B��B��B��B��B�
B�B�B�B�
B�B�B�B�/B�#B�#B�B�B��B��BɺBƨBǮB��B��B��BȴBŢBB�}B�qB�XB�RB�?B�?B�B�B��B��B��B�3B�}BĜB��B��B��B��B�
B�HB�B�B�B�B�B��B��B	B	
=B	%B	B	DB	PB	uB	�B	�B	�B	�B	�B	�B	�B	�B	%�B	)�B	-B	,B	-B	+B	'�B	$�B	$�B	$�B	)�B	0!B	49B	6FB	6FB	7LB	7LB	8RB	<jB	B�B	F�B	H�B	J�B	K�B	J�B	G�B	G�B	D�B	F�B	N�B	VB	T�B	P�B	R�B	W
B	XB	T�B	M�B	@�B	@�B	@�B	@�B	B�B	B�B	D�B	K�B	O�B	N�B	N�B	O�B	R�B	T�B	W
B	ZB	[#B	[#B	\)B	\)B	\)B	[#B	ZB	ZB	ZB	ZB	ZB	[#B	\)B	]/B	dZB	iyB	l�B	m�B	p�B	r�B	s�B	t�B	w�B	w�B	w�B	w�B	x�B	{�B	|�B	}�B	}�B	|�B	~�B	�B	�B	�PB	�oB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�LB	�XB	�dB	�jB	�jB	�qB	�qB	��B	��B	��B	B	ŢB	ǮB	ȴB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�HB	�BB	�HB	�HB	�NB	�NB	�ZB	�fB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
+B
DB
JB
JB
PB
VB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
bB
hB
bB
hB
oB
oB
oB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
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
,B
,B
,B
,B
,B
,B
-B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
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
33B
33B
49B
5?B
6FB
6FB
7LB
7LB
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
9XB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
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
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
D�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
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
J�B
J�B
J�B
K�B
K�B
K�B
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
N�B
N�B
N�B
N�B
N�B
N�B
N�B
P�B
O�B
P�B
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
R�B
R�B
S�B
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
XB
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
YB
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
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
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
aHB
bNB
bNB
bNB
bNB
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
e`B
e`B
e`B
e`B
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
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
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
n�B
o�B
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
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
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
v�B
v�B
v�B
v�B
v�B
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
y�B
y�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!HB
$�B
0�B
;0B
E�B
fB
t�B
{�B
�PB
�TB
��B
��B
�B
�+B
�B
�nB �B�B,BqB%�BABMBR�B\�Be,Bk�B~]B��B�GBуB��B�>B!�BB�BZ�B{�B~�B��B��B�PB�)B��B��B�XB��B�xB��B��B�~B��B�$B�sB��B��B��B��BHB`�BM�BKDBS�BS�BW
BS[BSuBS�BT{BUgBLB=qB9�B1vB)_BBB�B	�B�B��B�B�B�B�B�BܬB�B�B�B�hB��B��Br�Bm]BZBD�B-�B �B�B
��B
��B
��B
��B
��B
�gB
p�B
L�B
6B
'�B
�B	��B	�B	�!B	��B	�MB	ңB	ʌB	�]B	�TB	�QB	{0B	\CB	O�B	M�B	<�B	1AB	+�B	!B	kB	gB	B	B��B�MB��B�B� B��B�B��B�3B�B�*B�DB��B�	B�LB��B�?B��B��B��B��B��B��B�sB�gB�B�B�6B�yB��B�B��B��B��B�5B��B�9B�B�B�;B�aB��BƨB�_B�?B�BňB�?B�zBżB�BˬBϑBуB�oBՁB�YB�mBٚBٚB�YB�mB֡B�WB�B��BۦBخBרB��B� B�XB��B��B�^B�6B�dB�#B�YB�aB��B��B��B��B��B��B��B�}B��B�`B��B��B��B�mB�PB�VBΥB��B�?B�B�QB�AB��B�aB��B�`B�9B	GB	B	EB	�B	^B	�B	�B	CB	 �B	 �B	;B	B	�B	B	�B	%�B	*�B	.B	-B	.cB	,�B	)yB	%�B	%�B	%,B	*eB	0oB	4�B	6�B	7B	7�B	7�B	8�B	=B	CB	F�B	IRB	KDB	L�B	K�B	H�B	H�B	D�B	FtB	N�B	VSB	U�B	Q4B	R�B	WsB	YB	WsB	O�B	@�B	@�B	@�B	@�B	CaB	B�B	D�B	L~B	PHB	O�B	OvB	PHB	S[B	UgB	W�B	Z�B	[�B	[�B	\xB	\xB	\�B	[qB	ZkB	ZkB	ZQB	ZkB	ZkB	[qB	\xB	]�B	eB	i�B	mB	m�B	p�B	r�B	s�B	uB	w�B	xB	xB	x8B	yXB	|PB	}"B	~B	~(B	}VB	cB	�;B	�{B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�&B	�&B	��B	�KB	�WB	�wB	�CB	�GB	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�7B	��B	�=B	�jB	�6B	�B	��B	�.B	�4B	�:B	�@B	�FB	�2B	�2B	�B	�9B	�SB	�YB	�?B	�?B	�YB	�yB	�eB	�QB	�WB	�]B	�IB	�dB	ބB	ߊB	�B	��B	�|B	�B	�vB	�|B	�|B	�hB	�B	�B	�B	�B	�B	�B	��B	��B	�B	� B	��B	��B	�B	��B	�B	�B	��B	�B	�B	��B	��B	��B	�B	�$B	�*B	�VB	�.B	�HB	�cB	�cB
 OB
 iB
uB
GB
MB
MB
MB
gB
mB
?B
zB
xB
JB
~B
�B
pB
VB
pB
pB
VB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
 B
!-B
!-B
 �B
 �B
!B
 �B
 �B
"B
# B
# B
$@B
$@B
$&B
%B
%B
%,B
&B
&B
&B
&B
&B
&B
'B
'B
'B
'8B
($B
(
B
(
B
($B
($B
)DB
)*B
)*B
)*B
)DB
)_B
)DB
*KB
*0B
*0B
*B
*0B
*KB
+6B
+B
+B
+B
,=B
,=B
,=B
,WB
,WB
,=B
,=B
,WB
-]B
.cB
.cB
/iB
0UB
0oB
0oB
0UB
0UB
0UB
0UB
0UB
1[B
1�B
2�B
2|B
2aB
2|B
2|B
2|B
2aB
3�B
3�B
3�B
3hB
3�B
4�B
5�B
6�B
6�B
7fB
7�B
7�B
7�B
7�B
7fB
8�B
8lB
8�B
8�B
8�B
8�B
8�B
9�B
8lB
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:�B
:�B
;B
;B
;�B
;�B
;�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
>�B
>wB
?}B
?�B
?�B
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
A�B
A�B
A�B
B�B
B�B
C�B
D�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F%B
G�B
G�B
G�B
G�B
HB
IB
IB
J	B
I�B
I�B
I�B
J#B
KB
J�B
J�B
K�B
K�B
K�B
K�B
K�B
LB
MB
MB
MB
MB
MB
MB
MB
N"B
N<B
OB
N�B
OB
OB
N�B
OB
OBB
QB
PB
Q B
PB
O�B
Q4B
Q B
QB
QB
R:B
RB
R B
S&B
S&B
S&B
S&B
S&B
T,B
T,B
TB
T,B
T,B
T,B
TFB
TFB
U2B
UB
UB
VB
VB
V9B
V9B
V9B
VB
V9B
W?B
W?B
W?B
W?B
W?B
XEB
XEB
X+B
X+B
XEB
XEB
Y1B
Y1B
Y1B
YKB
Y1B
YKB
YKB
ZkB
ZkB
[WB
[WB
[WB
[#B
[=B
[=B
[WB
\]B
\]B
\]B
\xB
]IB
]dB
]~B
]IB
]~B
^jB
^jB
^jB
_pB
_VB
_pB
_pB
_pB
`\B
`vB
`�B
abB
a|B
a|B
a|B
a|B
a|B
abB
a|B
a|B
b�B
b�B
b�B
bhB
c�B
c�B
cnB
c�B
c�B
d�B
d�B
d�B
dtB
dtB
dtB
d�B
d�B
d�B
ezB
e`B
ezB
e�B
e�B
ezB
e�B
e�B
ffB
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
gmB
g�B
g�B
gmB
g�B
g�B
h�B
h�B
hsB
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
jB
j�B
j�B
j�B
j�B
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
n�B
o�B
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
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
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
v�B
v�B
v�B
v�B
v�B
xB
xB
xB
xB
y	B
y	B
y	B
x�B
x�B
x�B
y�B
y�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.17(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201706010036372017060100363720170601003637201806221314042018062213140420180622131404201804050715392018040507153920180405071539  JA  ARFMdecpA19c                                                                20170528093510  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170528003535  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170528003536  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170528003537  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170528003537  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170528003537  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170528003537  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170528003537  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170528003538  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170528003538                      G�O�G�O�G�O�                JA  ARUP                                                                        20170528010906                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170528153348  CV  JULD            G�O�G�O�F�U�                JM  ARCAJMQC2.0                                                                 20170531153637  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170531153637  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221539  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041404  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211516                      G�O�G�O�G�O�                