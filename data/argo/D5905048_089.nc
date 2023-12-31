CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-02-15T00:35:39Z creation;2017-02-15T00:35:41Z conversion to V3.1;2019-12-19T08:15:00Z update;     
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
_FillValue                  �  ې   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20170215003539  20200116211517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               YA   JA  I2_0577_089                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��5��G�1   @��6l��@4�|����d��"��`1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�fC�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D��3D�3D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D���D�@ D�|�D��D���D�<�D� D�� D�  D�@ D� D�� D�  D�C3D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @u�@��\@��\AG�A=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C��C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;��C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}��C��C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�DwuDw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�Dҽ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD��\D�:�D�w\D�\D��\D�7\D�z�D⺏D���D�:�D�z�D㺏D���D�=�D�z�D�\D���D�:�D�z�D序D���D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D�\D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�^5A�O�A�G�A�G�A�G�A�E�A�A�A�?}A�9XA�9XA�7LA�/A�+A��A�%A���A���A���A���A���A���A���A�%A�VA��A��A�{A�"�A�C�A�?}A�9XA��A�1A�A�%A�1A�VA��A�+A�+A�E�AȃAǝ�AǓuAǋDA��Aơ�A�%A�|�A�1A�M�AüjA¼jA�(�A���A��A�%A�A�A��TA���A�K�A�bNA�A��mA���A��A�+A��PA��A���A�7LA���A�ĜA��A�jA�;dA���A��-A�?}A�1A�hsA�7LA��PA��A�S�A��DA���A���A�?}A�XA��A�p�A�n�A��wA��mA��A��A���A�I�A�l�A�\)A���A���A���A�E�A��A�JA�dZA��A�ffA�-A��#A��+A��A�\)A��mA�;dA� �A�1A�7A~{A|�Azv�Ax  Av1'At1'ArȴAp��Amt�Ak��Ak;dAhI�Ae`BAdffAb~�Aa33A`A^I�A[�AY�TAY;dAX�AXA�AX{AW��AW�-AWXAV�AU�PAS33ARJAP�yAOS�AM/AJ1'AHȴAF�AFbAD�/AC��AB�uA?�A=��A;�A:=qA9+A8=qA6E�A3�
A2v�A0$�A/��A//A.n�A-��A-��A-/A* �A(��A'`BA&�A%XA#�;A#�A"�A!�A!��A!��A!l�A ��A�A(�AƨAS�AbA%A��A�wAA^5A�#A�+Ar�A/AbNA�7A�RA~�A�wA+A�A
��An�AG�A��A{A�7A��AbNA�!A�A ��A 1'@���@��H@��#@�x�@��9@���@�b@�bN@�A�@�bN@��w@��@�^5@�J@���@��7@��7@���@��@�~�@�E�@��@���@�"�@�`B@�O�@�ƨ@�+@�p�@��@�F@�%@�j@�bN@�33@⟾@�X@�/@��`@�z�@��
@�o@݁@�@��/@�ƨ@�"�@��y@֟�@�~�@Չ7@� �@�K�@Ұ!@���@Л�@θR@�&�@�G�@̼j@�1@�A�@̋D@�ƨ@�V@�J@�j@�33@��T@Õ�@�~�@��@��@��@�l�@��@��\@���@���@�&�@�Ĝ@�(�@�t�@�@���@���@��@�~�@�{@��h@��P@�o@���@�=q@�=q@�=q@�$�@�J@��@�@���@�x�@�`B@�%@�  @��@��@��
@���@�"�@�{@�p�@��-@�{@��@��^@�9X@�\)@�E�@�X@���@���@�Q�@�V@��@��^@�G�@��@��!@�5?@���@�Q�@�~�@��^@�O�@�&�@��`@��`@��@�dZ@�S�@���@�b@��@��@��
@��@��@��\@�@��-@�/@�Ĝ@�9X@��@�l�@�ȴ@���@�~�@�V@�5?@��@��h@��`@���@�z�@�1'@�  @���@���@�  @��m@�ƨ@��
@��m@���@��H@�n�@�V@�E�@�$�@��@�$�@���@�7L@�&�@�Ĝ@� �@���@�|�@�dZ@�dZ@�"�@��@���@�$�@���@���@�@��@�5?@�v�@�ff@�M�@�$�@��T@���@���@�G�@�/@��@�V@���@��@��@�r�@�bN@�9X@� �@��@��m@��@��@�|�@�;d@���@�~�@�^5@�{@���@���@�hs@�X@��@���@��D@�A�@���@�o@�ȴ@���@�^5@�5?@��@���@�?}@�%@��/@���@�Ĝ@��@���@��u@�Q�@�(�@��@�ƨ@��@�;d@��@�ȴ@���@��+@�n�@�=q@�{@�@��@���@�&�@�%@��@��j@��@���@���@��@�Q�@�I�@�1'@�|�@�@��@�ȴ@��+@�5?@�x�@��@�%@�V@���@�(�@���@�S�@�;d@�"�@�@���@��\@�E�@�{@��@���@���@��h@�X@�?}@��@���@�z�@�@�@~ȴ@~5?@}�T@}V@|j@|(�@{��@{ƨ@{33@z�!@z=q@y�^@y��@yhs@y�@x�9@x�u@x�u@x�u@x�u@x�u@x�@w��@vȴ@vff@v{@v@v@u�@u��@uO�@u�@t�D@t9X@s�
@s33@r�@r��@rJ@qhs@q7L@p��@pĜ@p�@p�@pQ�@pb@o�w@o|�@o
=@nȴ@n�+@nv�@nV@nV@nE�@n{@m��@m�-@m�h@m�@m�@m�@mp�@mp�@mO�@lz�@l(�@k�F@k@j�!@jJ@i�#@i��@ix�@i&�@h�u@h1'@f��@e�@d�@d�j@d��@d(�@c�m@c��@b�\@b=q@b�@a�#@a��@a&�@`��@` �@_�w@_+@^��@^{@]��@]/@\�@\�@\�@[��@[�@[dZ@[S�@[C�@[C�@[33@[@Zn�@Y�^@Y�@XĜ@Xb@W�P@W�@V��@V5?@U@U?}@T�/@T�D@S�m@S��@St�@SC�@S33@So@R��@Rn�@RM�@R�@Q��@Q��@Qx�@Q7L@Q�@P�9@P1'@O�@O
=@N��@NE�@M�T@M`B@L��@L�@L�D@LI�@K��@K��@K33@J~�@JM�@JM�@J-@I�@I��@IX@IG�@I7L@I7L@I�@HĜ@HbN@Hb@G|�@GK�@F�@F�R@F�+@E@E?}@D�/@D�j@D��@D�@C��@C�F@C�@Ct�@CS�@C"�@B��@BM�@A�^@Ax�@A&�@@��@@�@?�w@?�P@?|�@?K�@>��@>��@>��@>��@>��@>��@>�+@>�+@>v�@>v�@>ff@>E�@>$�@>@=��@=`B@<�/@<�@;�
@;�F@;"�@:�!@:�\@:^5@9�#@9X@9&�@9%@8��@8��@8��@8��@8�u@8bN@8A�@8  @7��@7�P@7l�@7�@6��@6{@5��@5��@5O�@5�@4�@4�/@4��@4�D@4I�@3�
@3�F@3��@3�@3dZ@3"�@3@2�@2��@2��@2n�@2-@2�@2�@2J@1��@1&�@0�@0b@/�;@/�@/\)@/;d@.�@.E�@-��@-��@-�-@-�@-/@,��@,�@,�D@,z�@,(�@+�
@+ƨ@+"�@*��@*�!@*�!@*��@*�\@*=q@*�@*�@)��@)G�@(��@(��@(A�@(  @'�;@'��@'�@'��@'|�@';d@'
=@&�@&�R@&ff@%��@%O�@%/@%�@%�@%�@%V@$��@$��@$�/@$j@$�@#��@#�F@#C�@#C�@#"�@#@"�H@"n�@"J@!�^@!��@!x�@!%@ �`@ �u@ 1'@   @�@�@�;@�@�P@|�@l�@\)@K�@+@�@
=@
=@�y@�@��@�+@v�@�+@ff@E�@@�h@�@Z@(�@�m@��@C�@��@=q@X@&�@%@��@��@��@��@�@1'@�@�R@�+@E�@{@@�@��@O�@/@�@��@�/@��@�j@��@j@9X@ƨ@C�@�@�!@^5@M�@=q@-@J@�#@�^@hs@G�@G�@G�@G�@G�@&�@��@�`@��@��@��@Ĝ@Ĝ@��@�u@bN@ �@�;@|�@;d@+@
=@�y@�R@��@��@��@E�@@�@�T@�T@�-@��@��@�h@p�@`B@?}@��@��@�j@�j@�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�^5A�O�A�G�A�G�A�G�A�E�A�A�A�?}A�9XA�9XA�7LA�/A�+A��A�%A���A���A���A���A���A���A���A�%A�VA��A��A�{A�"�A�C�A�?}A�9XA��A�1A�A�%A�1A�VA��A�+A�+A�E�AȃAǝ�AǓuAǋDA��Aơ�A�%A�|�A�1A�M�AüjA¼jA�(�A���A��A�%A�A�A��TA���A�K�A�bNA�A��mA���A��A�+A��PA��A���A�7LA���A�ĜA��A�jA�;dA���A��-A�?}A�1A�hsA�7LA��PA��A�S�A��DA���A���A�?}A�XA��A�p�A�n�A��wA��mA��A��A���A�I�A�l�A�\)A���A���A���A�E�A��A�JA�dZA��A�ffA�-A��#A��+A��A�\)A��mA�;dA� �A�1A�7A~{A|�Azv�Ax  Av1'At1'ArȴAp��Amt�Ak��Ak;dAhI�Ae`BAdffAb~�Aa33A`A^I�A[�AY�TAY;dAX�AXA�AX{AW��AW�-AWXAV�AU�PAS33ARJAP�yAOS�AM/AJ1'AHȴAF�AFbAD�/AC��AB�uA?�A=��A;�A:=qA9+A8=qA6E�A3�
A2v�A0$�A/��A//A.n�A-��A-��A-/A* �A(��A'`BA&�A%XA#�;A#�A"�A!�A!��A!��A!l�A ��A�A(�AƨAS�AbA%A��A�wAA^5A�#A�+Ar�A/AbNA�7A�RA~�A�wA+A�A
��An�AG�A��A{A�7A��AbNA�!A�A ��A 1'@���@��H@��#@�x�@��9@���@�b@�bN@�A�@�bN@��w@��@�^5@�J@���@��7@��7@���@��@�~�@�E�@��@���@�"�@�`B@�O�@�ƨ@�+@�p�@��@�F@�%@�j@�bN@�33@⟾@�X@�/@��`@�z�@��
@�o@݁@�@��/@�ƨ@�"�@��y@֟�@�~�@Չ7@� �@�K�@Ұ!@���@Л�@θR@�&�@�G�@̼j@�1@�A�@̋D@�ƨ@�V@�J@�j@�33@��T@Õ�@�~�@��@��@��@�l�@��@��\@���@���@�&�@�Ĝ@�(�@�t�@�@���@���@��@�~�@�{@��h@��P@�o@���@�=q@�=q@�=q@�$�@�J@��@�@���@�x�@�`B@�%@�  @��@��@��
@���@�"�@�{@�p�@��-@�{@��@��^@�9X@�\)@�E�@�X@���@���@�Q�@�V@��@��^@�G�@��@��!@�5?@���@�Q�@�~�@��^@�O�@�&�@��`@��`@��@�dZ@�S�@���@�b@��@��@��
@��@��@��\@�@��-@�/@�Ĝ@�9X@��@�l�@�ȴ@���@�~�@�V@�5?@��@��h@��`@���@�z�@�1'@�  @���@���@�  @��m@�ƨ@��
@��m@���@��H@�n�@�V@�E�@�$�@��@�$�@���@�7L@�&�@�Ĝ@� �@���@�|�@�dZ@�dZ@�"�@��@���@�$�@���@���@�@��@�5?@�v�@�ff@�M�@�$�@��T@���@���@�G�@�/@��@�V@���@��@��@�r�@�bN@�9X@� �@��@��m@��@��@�|�@�;d@���@�~�@�^5@�{@���@���@�hs@�X@��@���@��D@�A�@���@�o@�ȴ@���@�^5@�5?@��@���@�?}@�%@��/@���@�Ĝ@��@���@��u@�Q�@�(�@��@�ƨ@��@�;d@��@�ȴ@���@��+@�n�@�=q@�{@�@��@���@�&�@�%@��@��j@��@���@���@��@�Q�@�I�@�1'@�|�@�@��@�ȴ@��+@�5?@�x�@��@�%@�V@���@�(�@���@�S�@�;d@�"�@�@���@��\@�E�@�{@��@���@���@��h@�X@�?}@��@���@�z�@�@�@~ȴ@~5?@}�T@}V@|j@|(�@{��@{ƨ@{33@z�!@z=q@y�^@y��@yhs@y�@x�9@x�u@x�u@x�u@x�u@x�u@x�@w��@vȴ@vff@v{@v@v@u�@u��@uO�@u�@t�D@t9X@s�
@s33@r�@r��@rJ@qhs@q7L@p��@pĜ@p�@p�@pQ�@pb@o�w@o|�@o
=@nȴ@n�+@nv�@nV@nV@nE�@n{@m��@m�-@m�h@m�@m�@m�@mp�@mp�@mO�@lz�@l(�@k�F@k@j�!@jJ@i�#@i��@ix�@i&�@h�u@h1'@f��@e�@d�@d�j@d��@d(�@c�m@c��@b�\@b=q@b�@a�#@a��@a&�@`��@` �@_�w@_+@^��@^{@]��@]/@\�@\�@\�@[��@[�@[dZ@[S�@[C�@[C�@[33@[@Zn�@Y�^@Y�@XĜ@Xb@W�P@W�@V��@V5?@U@U?}@T�/@T�D@S�m@S��@St�@SC�@S33@So@R��@Rn�@RM�@R�@Q��@Q��@Qx�@Q7L@Q�@P�9@P1'@O�@O
=@N��@NE�@M�T@M`B@L��@L�@L�D@LI�@K��@K��@K33@J~�@JM�@JM�@J-@I�@I��@IX@IG�@I7L@I7L@I�@HĜ@HbN@Hb@G|�@GK�@F�@F�R@F�+@E@E?}@D�/@D�j@D��@D�@C��@C�F@C�@Ct�@CS�@C"�@B��@BM�@A�^@Ax�@A&�@@��@@�@?�w@?�P@?|�@?K�@>��@>��@>��@>��@>��@>��@>�+@>�+@>v�@>v�@>ff@>E�@>$�@>@=��@=`B@<�/@<�@;�
@;�F@;"�@:�!@:�\@:^5@9�#@9X@9&�@9%@8��@8��@8��@8��@8�u@8bN@8A�@8  @7��@7�P@7l�@7�@6��@6{@5��@5��@5O�@5�@4�@4�/@4��@4�D@4I�@3�
@3�F@3��@3�@3dZ@3"�@3@2�@2��@2��@2n�@2-@2�@2�@2J@1��@1&�@0�@0b@/�;@/�@/\)@/;d@.�@.E�@-��@-��@-�-@-�@-/@,��@,�@,�D@,z�@,(�@+�
@+ƨ@+"�@*��@*�!@*�!@*��@*�\@*=q@*�@*�@)��@)G�@(��@(��@(A�@(  @'�;@'��@'�@'��@'|�@';d@'
=@&�@&�R@&ff@%��@%O�@%/@%�@%�@%�@%V@$��@$��@$�/@$j@$�@#��@#�F@#C�@#C�@#"�@#@"�H@"n�@"J@!�^@!��@!x�@!%@ �`@ �u@ 1'@   @�@�@�;@�@�P@|�@l�@\)@K�@+@�@
=@
=@�y@�@��@�+@v�@�+@ff@E�@@�h@�@Z@(�@�m@��@C�@��@=q@X@&�@%@��@��@��@��@�@1'@�@�R@�+@E�@{@@�@��@O�@/@�@��@�/@��@�j@��@j@9X@ƨ@C�@�@�!@^5@M�@=q@-@J@�#@�^@hs@G�@G�@G�@G�@G�@&�@��@�`@��@��@��@Ĝ@Ĝ@��@�u@bN@ �@�;@|�@;d@+@
=@�y@�R@��@��@��@E�@@�@�T@�T@�-@��@��@�h@p�@`B@?}@��@��@�j@�j@�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B8RB8RB8RB8RB8RB8RB8RB9XB:^B;dB;dB>wB?}BE�BI�BN�BP�BQ�BR�BS�BS�BW
B^5Bk�Bz�B�=B��B��B��B�B�B�-B�'B�'B�-B�9B�qB��B�5B�B��B�B<jBC�BI�B\)B_;BdZBk�Bn�Bs�By�B�JB��B�B�RB�FB�3B�3B�3B�FB�'B�B�B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B�\B�%B�B|�BiyBXBQ�BI�B>wB.B�B
=BB��B�B�TB�5B��BƨB�'B��B�{B}�Bn�BS�BF�BA�B33BVB
��B
�B
�yB
�ZB
�;B
��B
�?B
��B
�hB
�\B
�JB
�7B
|�B
s�B
cTB
N�B
B�B
2-B
%�B
�B
B	��B	�B	�BB	ɺB	B	�LB	�B	��B	��B	�JB	�B	}�B	x�B	v�B	t�B	t�B	r�B	o�B	l�B	dZB	S�B	N�B	L�B	D�B	:^B	,B	!�B	�B	oB	PB	%B	B��B�B�B�fB�HB�/B�B��BȴBĜB��B��B�}B�jB�^B�RB�?B�!B�B�B�3B�-B�!B�!B�!B�B�B�B��B��B��B��B��B��B��B��B�'B�-B�-B�'B�B��B��B��B�uB�oB��B��B�{B��B��B��B��B�B�'B�-B�B�B��B��B��B��B��B��B��B��B��B��B�FBƨBɺB��B��B��B��B��B��B��B��B��B��BȴB�jB��BƨB��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�#B�#B�/B�HB�HB�NB�ZB�ZB�`B�`B�B�B�B��B��B��B�B�B��B	B	\B	�B	�B	"�B	!�B	!�B	"�B	!�B	!�B	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	%�B	(�B	+B	.B	33B	5?B	6FB	7LB	9XB	<jB	<jB	>wB	?}B	?}B	A�B	C�B	C�B	E�B	G�B	I�B	J�B	K�B	L�B	M�B	M�B	O�B	R�B	Q�B	P�B	ZB	\)B	]/B	\)B	ZB	]/B	cTB	bNB	gmB	bNB	`BB	^5B	]/B	^5B	aHB	ffB	iyB	r�B	q�B	q�B	m�B	iyB	hsB	gmB	k�B	l�B	n�B	n�B	o�B	q�B	q�B	x�B	x�B	x�B	}�B	�B	�B	�B	�B	�B	�B	�JB	�VB	�\B	�hB	�uB	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�XB	�XB	�XB	�XB	�XB	�XB	�RB	�RB	�RB	�XB	�XB	�^B	�jB	�qB	�}B	�}B	��B	��B	B	ÖB	ŢB	��B	��B	��B	��B	��B	�B	�5B	�5B	�BB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
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
JB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
bB
bB
hB
bB
bB
hB
uB
uB
uB
uB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
&�B
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
,B
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
33B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
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
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
>wB
>wB
?}B
?}B
?}B
?}B
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
C�B
C�B
C�B
C�B
D�B
D�B
E�B
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
L�B
M�B
N�B
N�B
N�B
O�B
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
P�B
P�B
P�B
P�B
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
S�B
S�B
T�B
T�B
T�B
T�B
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
VB
W
B
W
B
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
ZB
ZB
ZB
ZB
ZB
ZB
ZB
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
^5B
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
`BB
aHB
bNB
bNB
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
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
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
o�B
p�B
o�B
o�B
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
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
u�B
u�B
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
w�B
w�B
w�B
x�B
x�B
x�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B8�B8lB8RB8lB8RB8RB8RB9rB:xB;B;�B>�B?�BE�BI�BN�BP�BQ�BR�BTBS�BV�B^5Bk�Bz�B�XB�mB�~B��B�=B�OB�GB�'B�AB�-B�9B�qB��B�jB��B��B BB<jBC�BJ�B]IB`�Be�Bl�Bp;Bu�B|�B�B��B�'B��B�rB��B�+B�`B��B�B��B��B��B�6B�B�LB��B��B��B��B�)B�/B��B�TB��B�CB��B��B�EB��B��BmBZBTBL�BA�B2BCBBgB��B�iB�B�'B�BɠB��B��B��B��BraBU�BH1BD�B8�B4B
��B
�}B
�B
��B
�4B
خB
�RB
�4B
��B
��B
�jB
�)B
B
v�B
fLB
QhB
EB
4TB
(�B
B
%B	�zB	�-B	�TB	�xB	�B	�	B	��B	�XB	��B	�B	��B	~�B	yXB	w2B	u%B	u?B	shB	p�B	n�B	gB	U�B	P�B	O(B	G�B	=�B	./B	$B	B	B	B	1B	mB��B��B�B�
B� B��B��B� B�)B�mB�[B��B�OB�"B�B��B�LB��B��B��B��B��B�[B�B��B��B�B��B�B��B��B��B�KB�B��B�ZB�-B�3B�hB�MB��B��B�B��B��B�&B��B��B��B��B�IB�EB��B��B�-B�3B�;B�CB��B��B��B�CB�WB�]B�;B�VB�nB��B�`B��B�	B�\BуB�HB�BB�<B�B�"B�BB�BϫB�)B��B�[B�zB��B�VB�B��B��B��B��BՁB�SB֡B��BخB��B�kBچBڠB��B�BܬB�pB��B��B�B�B�B��B�LB�}B�[B�hB��B��B��B��B��B�lB	�B	\B	�B	jB	#�B	"NB	#B	#�B	#B	#TB	�B	CB	dB	IB	�B	 BB	"NB	$ZB	%FB	&fB	)yB	+�B	.�B	3�B	5tB	6`B	7�B	9�B	<�B	=<B	?�B	@ B	?�B	A�B	C�B	C�B	E�B	G�B	I�B	J�B	LB	MB	N"B	NVB	P�B	S�B	Q�B	P�B	ZkB	\�B	]�B	\�B	ZB	]B	c�B	b�B	hsB	c B	a-B	^�B	]�B	^jB	a|B	fB	iDB	r�B	raB	r�B	ncB	j0B	i_B	h>B	l�B	m)B	o B	n�B	o�B	q�B	raB	y>B	x�B	x�B	~B	�-B	�MB	�gB	�gB	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�B	��B	�B	�TB	�`B	�8B	�$B	�>B	�6B	�)B	�/B	�/B	�OB	�UB	�GB	�TB	��B	��B	��B	�rB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�EB	�jB	ބB	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	��B	��B	�B	��B	�B	�B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�8B	�rB	�*B	�B	�0B	�0B	�0B	�6B	�VB
 iB
;B
AB
GB
3B
MB
MB
MB
gB
SB
SB
tB
_B
zB
�B
	RB
	lB
	lB
	lB

rB

XB

rB

XB

�B
�B
dB
�B
�B
jB
jB
�B
jB
�B
�B
�B
�B
�B
�B
vB
�B
�B
 B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
	B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
 B
�B
 B
!B
!B
!�B
!�B
!�B
#B
#B
$B
$B
$B
%B
%,B
&B
&B
'B
'B
'B
'B
'B
'B
'B
'B
'B
&�B
'�B
(
B
'B
($B
(XB
($B
(>B
)DB
)DB
)*B
)B
*0B
+6B
+QB
,WB
,WB
,qB
-�B
.cB
.IB
.IB
.IB
/5B
/iB
/�B
0UB
0UB
0UB
0UB
0oB
0oB
0oB
1vB
1vB
2|B
2|B
3hB
3�B
3hB
3hB
3�B
4�B
5ZB
5tB
5ZB
5ZB
5ZB
5tB
5tB
5�B
5�B
6�B
6�B
6�B
6�B
7�B
7�B
8�B
8�B
8�B
9�B
9�B
9�B
:�B
:�B
:�B
:�B
:�B
:xB
;�B
;�B
;B
;B
;�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
>�B
>�B
?�B
?�B
?�B
?�B
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
C�B
C�B
C�B
C�B
D�B
D�B
E�B
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
IB
J	B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
KB
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
MB
MB
MB
L�B
MB
MB
N"B
OB
OB
O(B
PB
PB
PB
P.B
PB
QB
QB
Q B
QB
QB
Q B
QB
QB
QB
Q B
R B
R B
R B
R B
R B
S@B
S&B
S&B
S&B
T,B
T,B
TB
TB
T,B
T,B
T,B
UB
UB
U2B
UB
U2B
UB
UB
U2B
V9B
V9B
V9B
VB
VB
V9B
VSB
WYB
WYB
X_B
Y1B
Y1B
Y1B
YKB
YeB
YeB
YeB
ZQB
Z7B
ZQB
ZQB
Z7B
ZQB
ZQB
Z7B
ZQB
[WB
[WB
\xB
\]B
\)B
\CB
\CB
\CB
\]B
\CB
\]B
]dB
]dB
]IB
^jB
^jB
^jB
^OB
_;B
_pB
_pB
_pB
_pB
`vB
`vB
`vB
`�B
a�B
b�B
b�B
bNB
bhB
bhB
bhB
bhB
bhB
b�B
b�B
c�B
c�B
cnB
c�B
dtB
d�B
d�B
dtB
d�B
d�B
e�B
ezB
e�B
e�B
f�B
f�B
f�B
f�B
f�B
ffB
f�B
f�B
g�B
g�B
gmB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
j�B
j�B
j�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
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
o�B
p�B
o�B
o�B
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
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
u�B
u�B
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
xB
xB
xB
x�B
x�B
x�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.17(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201702190037432017021900374320170219003743201806221309192018062213091920180622130919201804050710082018040507100820180405071008  JA  ARFMdecpA19c                                                                20170215093511  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170215003539  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170215003540  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170215003540  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170215003541  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170215003541  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170215003541  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170215003541  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170215003541  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170215003541                      G�O�G�O�G�O�                JA  ARUP                                                                        20170215010313                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170215153430  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20170218153743  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170218153743  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221008  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040919  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211517                      G�O�G�O�G�O�                