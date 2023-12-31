CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-08-17T09:00:46Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �4   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �4   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �4   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �4   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210817090046  20210817090046  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               sA   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @ٌSgY)�1   @ٌT  	�@:��O�;d�c��$�/1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         sA   A   F   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DRy�DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @{�@��\@��\AG�A=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��)B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1��C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�Cc�{Ce�{Cg�{Ci�Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRn�DR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�DwuDw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�7\D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�w\D���D���D�:�D�z�D���D���D�:�D�z�D���D��\D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�=�D�z�D���D���D�=�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�w\D��\D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�z�D⺏D���D�:�D�z�D㺏D���D�:�D�z�D亏D���D�:�D�z�D序D���D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D躏D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�\)A�\)A�n�A�r�A�p�A�p�A�n�A�ZA�A�A�(�A��;A̮A̧�A̟�A̍PA�K�A��;A�VA�ƨA�dZA��HAȁA��A��A��#A�A��uA��uA��mA��HA�7LA�VA��A��A�=qA��A�7LA���A�t�A�A�ĜA�dZA��PA�;dA��A��`A���A�ȴA� �A�K�A��9A�{A��A���A���A�z�A��/A��hA�`BA�G�A��mA�l�A�n�A���A��HA�~�A��+A��RA�VA��wA��FA��PA��A���A�dZA��A��A���A�^5A�$�A���A��!A��/A�S�A��`A�33A��A��uA�A�A�\)A���A�"�A�XA��wA�XA��A~ffA{�PAy�Ay;dAy
=Axz�Aw�FAv^5At�HAs+Aq��Aql�Ap~�An1'Ak`BAi�TAg/Ae�7Ad�DAcƨAbffA_��A^AZ�AX �AV�/AU�;AS�hAR�RAQ�^AP�+AO?}AN��ANffAM�-ALȴALn�AKhsAJbAH��AH{AF�DAE\)AD��ADz�ADAC��ACS�AA�TA@I�A@JA?�^A?O�A>��A>bNA=��A=��A=��A<�A;�-A;�A9�;A9��A9�7A8��A8VA7�-A7l�A7A5&�A4M�A4$�A3��A3K�A2��A0�A/�-A.�uA-�A-ƨA,n�A+O�A*�A)�^A(JA'%A&I�A%�PA#��A#G�A"~�A!XA ȴA (�A�A�AA�A�DAI�A�;A|�A(�A�`A  A\)A%A�+A�TAS�AĜAJA��AM�A{A�7A�At�A��A\)A�A��A	�^AĜA$�A��A`BA�A��A�AM�A�AA��A�AƨAS�A�`A  A1@��@�?}@�z�@�A�@��@��F@�J@��m@���@���@�(�@�@��T@�z�@��y@�X@��y@���@�bN@�t�@��
@�7L@�t�@�&�@�M�@׾w@�
=@���@Ӯ@җ�@щ7@�Ĝ@Ѓ@� �@���@�7L@�Z@�+@�&�@���@�b@őh@å�@���@��m@���@�|�@�
=@�$�@��h@��@�j@�  @��
@��F@���@��!@���@���@���@�bN@�\)@�J@�p�@�?}@�A�@��@�v�@�@�X@�Ĝ@�Q�@��
@�l�@�ȴ@��^@�hs@�X@�Ĝ@��F@��@��@���@��R@�M�@�Ĝ@��@�l�@��@�J@��T@�x�@���@��j@��u@��@��
@��@�33@���@�=q@�x�@��@��/@�j@�  @�+@��@�7L@�I�@�dZ@���@��R@���@���@���@�~�@�{@�X@���@��
@�K�@�"�@��!@���@�/@���@��/@���@���@�I�@��@�C�@��@���@���@��#@��@��9@�9X@��w@���@��P@�K�@���@�V@��@�@�@���@�x�@�G�@���@�9X@��F@�t�@�C�@��y@���@�V@���@�/@�V@���@��`@���@�Ĝ@��9@��@�A�@�(�@��;@��w@�@���@�ff@���@��h@�X@���@��`@���@�j@��j@���@��`@��9@��m@�S�@�K�@�33@�\)@�dZ@�l�@�l�@�t�@�t�@�\)@�;d@�o@��R@��\@�ff@��^@��7@�G�@���@��/@��j@���@�r�@�Z@��@�;@��@l�@K�@+@~�y@~ȴ@~��@}�@}�h@}?}@|��@|�@|��@|�@|�D@|(�@|(�@|�@{��@{��@{��@z�!@z=q@z�@zJ@y�#@y��@y�7@yX@x�`@x�9@x�@x �@w��@vȴ@vV@v$�@v{@v@v@v{@v{@v{@v{@v@u�T@t�/@t��@t��@tI�@sƨ@r�\@r��@r�H@r��@r��@r�!@r��@q��@q7L@p�@pbN@pA�@pb@o�;@o�;@o�P@o\)@n�R@m�@mO�@lz�@l(�@l�@k��@k��@kt�@kt�@kdZ@j�H@j��@jn�@j^5@jM�@i��@i�@hĜ@h�u@g�;@g;d@f��@f@e��@d��@dz�@cƨ@c"�@b��@b-@a�@a��@`��@`��@`�@`A�@_�;@_�@_\)@^�y@^v�@^@]�@]��@]p�@]V@\��@\�j@\��@\z�@[��@[��@[dZ@[33@[o@Z�\@Z^5@ZM�@Z�@Y�#@Y��@Y�7@YX@Y%@X�u@W�@W|�@W;d@V�+@V{@U�T@U�h@UV@Tj@TI�@T1@SdZ@R�H@R~�@R�@Q��@QX@Qhs@Qx�@PĜ@PQ�@Pb@O�;@O�P@O�@N��@N�R@N��@N��@NV@N@M�-@MO�@L�@L�j@L�@L��@LZ@L1@K�m@Kƨ@K��@Kt�@Kt�@KdZ@KC�@K33@K@J��@J�\@J~�@JM�@J�@I�#@I��@IX@I�@H�`@H�9@HbN@HA�@H1'@H �@Hb@Hb@H  @H  @H  @G�@G�@G�;@G�;@G�;@G��@G��@G�w@G�@G|�@F�@F��@Fff@F$�@E�@Ep�@D�@Dj@DI�@D(�@D�@D�@C�
@Ct�@B�@BM�@A��@Ahs@AG�@A7L@A&�@A&�@A%@@�9@@r�@?�;@?�@>ȴ@>�R@>�R@>��@>�+@>@=?}@<�/@<�D@<j@;��@;�
@;ƨ@;�F@;��@;"�@:�!@:�@9�7@9�7@9�@8Ĝ@8A�@8 �@7|�@7�@6�+@65?@6{@5�@5�@5�@5�-@5p�@5�@4�/@4��@4z�@4I�@4�@3��@3ƨ@3S�@2�\@2�\@2~�@2n�@2J@1x�@17L@1&�@1%@0�`@0�9@0bN@01'@/�@/|�@/;d@/+@.��@.�@.��@.��@.V@-�T@-�h@-`B@-V@,�/@,��@,z�@,�@+��@+t�@+dZ@*�@*�\@*=q@)�@)�^@)x�@)�@)%@(��@(��@(�9@( �@'�w@'��@'\)@'�@&�y@&�y@&�y@&�y@&�@&�@&ȴ@&�+@%�@%�-@%��@%�@%?}@%?}@%/@%V@$��@$z�@$(�@#��@#��@#33@"�H@"�\@"M�@"�@"J@!��@!�@!�#@!�^@!�7@!G�@ Ĝ@ r�@�;@�@�@�@�;@�P@
=@
=@�y@�@�@�R@ff@$�@{@�@��@�-@p�@?}@�@�@��@�/@�@�D@z�@Z@�@ƨ@��@t�@C�@o@�@��@��@�!@��@n�@M�@=q@J@��@��@x�@X@�@��@��@�@Q�@ �@�;@\)@ȴ@�+@E�@{@@�T@�T@@�@p�@p�@`B@`B@`B@?}@�@��@�/@�j@Z@�m@ƨ@t�@t�@dZ@S�@"�@"�@@��@�!@�!@��@^5@-@-@�@J@��@��@G�@�@��@��@�u@�@Q�@b@�P@
=@�@ȴ@ȴ@ȴ@ȴ@��@V@5?@�@�@�h@O�@V@�/@�j@��@9X@�@1@�
@�F@��@t�@S�@C�@33@
��@
��@
~�@
^5@
=q@	��@	�^@	�7@	X@	7L@	�@�`@�9@�u@�u@�u@�@r�@bN@A�@�@��@�@|�@;d@�@��@v�@V@V@E�@$�@@�T@��@�-@�h@`B@�@��@�D@�@�
@ƨ@��@t�@C�@"�@@��@�\@~�@M�@�@��@�^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�\)A�\)A�n�A�r�A�p�A�p�A�n�A�ZA�A�A�(�A��;A̮A̧�A̟�A̍PA�K�A��;A�VA�ƨA�dZA��HAȁA��A��A��#A�A��uA��uA��mA��HA�7LA�VA��A��A�=qA��A�7LA���A�t�A�A�ĜA�dZA��PA�;dA��A��`A���A�ȴA� �A�K�A��9A�{A��A���A���A�z�A��/A��hA�`BA�G�A��mA�l�A�n�A���A��HA�~�A��+A��RA�VA��wA��FA��PA��A���A�dZA��A��A���A�^5A�$�A���A��!A��/A�S�A��`A�33A��A��uA�A�A�\)A���A�"�A�XA��wA�XA��A~ffA{�PAy�Ay;dAy
=Axz�Aw�FAv^5At�HAs+Aq��Aql�Ap~�An1'Ak`BAi�TAg/Ae�7Ad�DAcƨAbffA_��A^AZ�AX �AV�/AU�;AS�hAR�RAQ�^AP�+AO?}AN��ANffAM�-ALȴALn�AKhsAJbAH��AH{AF�DAE\)AD��ADz�ADAC��ACS�AA�TA@I�A@JA?�^A?O�A>��A>bNA=��A=��A=��A<�A;�-A;�A9�;A9��A9�7A8��A8VA7�-A7l�A7A5&�A4M�A4$�A3��A3K�A2��A0�A/�-A.�uA-�A-ƨA,n�A+O�A*�A)�^A(JA'%A&I�A%�PA#��A#G�A"~�A!XA ȴA (�A�A�AA�A�DAI�A�;A|�A(�A�`A  A\)A%A�+A�TAS�AĜAJA��AM�A{A�7A�At�A��A\)A�A��A	�^AĜA$�A��A`BA�A��A�AM�A�AA��A�AƨAS�A�`A  A1@��@�?}@�z�@�A�@��@��F@�J@��m@���@���@�(�@�@��T@�z�@��y@�X@��y@���@�bN@�t�@��
@�7L@�t�@�&�@�M�@׾w@�
=@���@Ӯ@җ�@щ7@�Ĝ@Ѓ@� �@���@�7L@�Z@�+@�&�@���@�b@őh@å�@���@��m@���@�|�@�
=@�$�@��h@��@�j@�  @��
@��F@���@��!@���@���@���@�bN@�\)@�J@�p�@�?}@�A�@��@�v�@�@�X@�Ĝ@�Q�@��
@�l�@�ȴ@��^@�hs@�X@�Ĝ@��F@��@��@���@��R@�M�@�Ĝ@��@�l�@��@�J@��T@�x�@���@��j@��u@��@��
@��@�33@���@�=q@�x�@��@��/@�j@�  @�+@��@�7L@�I�@�dZ@���@��R@���@���@���@�~�@�{@�X@���@��
@�K�@�"�@��!@���@�/@���@��/@���@���@�I�@��@�C�@��@���@���@��#@��@��9@�9X@��w@���@��P@�K�@���@�V@��@�@�@���@�x�@�G�@���@�9X@��F@�t�@�C�@��y@���@�V@���@�/@�V@���@��`@���@�Ĝ@��9@��@�A�@�(�@��;@��w@�@���@�ff@���@��h@�X@���@��`@���@�j@��j@���@��`@��9@��m@�S�@�K�@�33@�\)@�dZ@�l�@�l�@�t�@�t�@�\)@�;d@�o@��R@��\@�ff@��^@��7@�G�@���@��/@��j@���@�r�@�Z@��@�;@��@l�@K�@+@~�y@~ȴ@~��@}�@}�h@}?}@|��@|�@|��@|�@|�D@|(�@|(�@|�@{��@{��@{��@z�!@z=q@z�@zJ@y�#@y��@y�7@yX@x�`@x�9@x�@x �@w��@vȴ@vV@v$�@v{@v@v@v{@v{@v{@v{@v@u�T@t�/@t��@t��@tI�@sƨ@r�\@r��@r�H@r��@r��@r�!@r��@q��@q7L@p�@pbN@pA�@pb@o�;@o�;@o�P@o\)@n�R@m�@mO�@lz�@l(�@l�@k��@k��@kt�@kt�@kdZ@j�H@j��@jn�@j^5@jM�@i��@i�@hĜ@h�u@g�;@g;d@f��@f@e��@d��@dz�@cƨ@c"�@b��@b-@a�@a��@`��@`��@`�@`A�@_�;@_�@_\)@^�y@^v�@^@]�@]��@]p�@]V@\��@\�j@\��@\z�@[��@[��@[dZ@[33@[o@Z�\@Z^5@ZM�@Z�@Y�#@Y��@Y�7@YX@Y%@X�u@W�@W|�@W;d@V�+@V{@U�T@U�h@UV@Tj@TI�@T1@SdZ@R�H@R~�@R�@Q��@QX@Qhs@Qx�@PĜ@PQ�@Pb@O�;@O�P@O�@N��@N�R@N��@N��@NV@N@M�-@MO�@L�@L�j@L�@L��@LZ@L1@K�m@Kƨ@K��@Kt�@Kt�@KdZ@KC�@K33@K@J��@J�\@J~�@JM�@J�@I�#@I��@IX@I�@H�`@H�9@HbN@HA�@H1'@H �@Hb@Hb@H  @H  @H  @G�@G�@G�;@G�;@G�;@G��@G��@G�w@G�@G|�@F�@F��@Fff@F$�@E�@Ep�@D�@Dj@DI�@D(�@D�@D�@C�
@Ct�@B�@BM�@A��@Ahs@AG�@A7L@A&�@A&�@A%@@�9@@r�@?�;@?�@>ȴ@>�R@>�R@>��@>�+@>@=?}@<�/@<�D@<j@;��@;�
@;ƨ@;�F@;��@;"�@:�!@:�@9�7@9�7@9�@8Ĝ@8A�@8 �@7|�@7�@6�+@65?@6{@5�@5�@5�@5�-@5p�@5�@4�/@4��@4z�@4I�@4�@3��@3ƨ@3S�@2�\@2�\@2~�@2n�@2J@1x�@17L@1&�@1%@0�`@0�9@0bN@01'@/�@/|�@/;d@/+@.��@.�@.��@.��@.V@-�T@-�h@-`B@-V@,�/@,��@,z�@,�@+��@+t�@+dZ@*�@*�\@*=q@)�@)�^@)x�@)�@)%@(��@(��@(�9@( �@'�w@'��@'\)@'�@&�y@&�y@&�y@&�y@&�@&�@&ȴ@&�+@%�@%�-@%��@%�@%?}@%?}@%/@%V@$��@$z�@$(�@#��@#��@#33@"�H@"�\@"M�@"�@"J@!��@!�@!�#@!�^@!�7@!G�@ Ĝ@ r�@�;@�@�@�@�;@�P@
=@
=@�y@�@�@�R@ff@$�@{@�@��@�-@p�@?}@�@�@��@�/@�@�D@z�@Z@�@ƨ@��@t�@C�@o@�@��@��@�!@��@n�@M�@=q@J@��@��@x�@X@�@��@��@�@Q�@ �@�;@\)@ȴ@�+@E�@{@@�T@�T@@�@p�@p�@`B@`B@`B@?}@�@��@�/@�j@Z@�m@ƨ@t�@t�@dZ@S�@"�@"�@@��@�!@�!@��@^5@-@-@�@J@��@��@G�@�@��@��@�u@�@Q�@b@�P@
=@�@ȴ@ȴ@ȴ@ȴ@��@V@5?@�@�@�h@O�@V@�/@�j@��@9X@�@1@�
@�F@��@t�@S�@C�@33@
��@
��@
~�@
^5@
=q@	��@	�^@	�7@	X@	7L@	�@�`@�9@�u@�u@�u@�@r�@bN@A�@�@��@�@|�@;d@�@��@v�@V@V@E�@$�@@�T@��@�-@�h@`B@�@��@�D@�@�
@ƨ@��@t�@C�@"�@@��@�\@~�@M�@�@��@�^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B6FB6FB5?B5?B5?B49B49B33B33B33B33B49B49B33B2-B/B(�B#�B�B�B�B%B��B�7BaHBXBS�BN�BK�BG�BC�B=qB;dB6FB33B2-B-B)�B'�B"�B!�B�B�B�B�B�B�BPB	7B  B��B�B�fB�;B�/B�)B�B��B��B��B��B��B�
B��B�^B�-B��B�VB� Bv�Bw�B�B�B�By�Bn�BO�B@�B:^B7LB/B\B��B�B�fB�BɺB�-B�BffBI�B;dB-B�BB�B��B�}B�B�B�B��B��B��B�oB�DB� B{�By�Bk�B]/BT�BF�B:^B5?B1'B+B �B�BPBB
��B
��B
�B
��B
�B
�B
�B
�B
�B
�sB
�HB
�`B
�TB
�)B
��B
��B
ƨB
ÖB
��B
�}B
�qB
�jB
�wB
��B
�wB
�dB
�dB
�dB
�XB
�RB
�FB
�?B
�3B
�-B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�hB
�JB
�DB
�7B
�+B
�B
{�B
t�B
l�B
gmB
ffB
`BB
]/B
W
B
R�B
M�B
G�B
E�B
E�B
@�B
>wB
=qB
9XB
7LB
6FB
49B
1'B
1'B
/B
-B
-B
,B
)�B
(�B
$�B
#�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
hB
VB
JB
1B
%B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�mB	�mB	�fB	�`B	�ZB	�TB	�NB	�HB	�HB	�BB	�BB	�BB	�;B	�;B	�;B	�HB	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�BB	�BB	�NB	�NB	�NB	�ZB	�fB	�`B	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
+B

=B
JB
VB
\B
hB
oB
{B
�B
�B
�B
�B
�B
�B
"�B
$�B
%�B
%�B
&�B
'�B
/B
1'B
49B
6FB
;dB
;dB
>wB
@�B
A�B
B�B
D�B
E�B
G�B
I�B
L�B
N�B
R�B
S�B
T�B
VB
XB
]/B
bNB
ffB
hsB
iyB
l�B
m�B
n�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
z�B
~�B
� B
�B
�B
�B
�B
�7B
�DB
�PB
�PB
�VB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�!B
�-B
�3B
�9B
�RB
�qB
��B
B
ÖB
ƨB
ǮB
��B
��B
��B
�B
�B
�
B
�B
�B
�B
�#B
�)B
�5B
�BB
�NB
�sB
�sB
�B
�B
�B
�B
��B
��B
��B  BB+BJBhBhBuBuB{B�B�B�B�B�B�B!�B"�B%�B'�B(�B+B.B/B0!B2-B33B49B6FB7LB8RB:^B;dB<jB<jB=qB=qB>wB?}B?}BC�BD�BF�BG�BH�BH�BH�BI�BK�BN�BQ�BQ�BR�BT�BYB[#B]/B_;B`BBbNBcTBdZBffBhsBk�Bl�Bm�Bn�Bo�Bq�Br�Bs�Bs�Bu�Bv�Bv�Bw�By�B{�B}�B}�B�B�B�%B�DB�JB�VB�VB�VB�\B�\B�\B�hB�oB�oB�uB�uB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�3B�?B�RB�XB�^B�^B�jB�jB�qB�}B��B��B��BÖBĜBƨBǮBȴBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�B�B�#B�#B�5B�;B�BB�BB�NB�TB�TB�TB�`B�`B�fB�mB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B  B  B  B  B  B  BBBBBBBB%B1B1B1B1B1B	7B	7BDBJBVB\B\B\B\B\B\B\BbBhBoBoBoBoBoBoBuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B!�B!�B!�B"�B#�B$�B$�B$�B%�B%�B%�B&�B&�B'�B(�B(�B(�B(�B)�B+B+B+B+B,B,B,B,B-B-B.B.B.B.B/B/B/B0!B0!B0!B1'B1'B1'B1'B2-B33B33B33B49B49B49B5?B5?B5?B6FB6FB6FB6FB6FB7LB8RB8RB8RB8RB9XB9XB9XB9XB9XB9XB9XB9XB:^B:^B;dB;dB;dB;dB;dB;dB;dB<jB<jB<jB=qB=qB>wB>wB?}B?}B?}B?}B?}B@�B@�B@�B@�BA�BB�BB�BB�BB�BB�BB�BC�BD�BD�BD�BD�BD�BD�BE�BE�BE�BE�BE�BF�BF�BF�BG�BG�BG�BG�BG�BG�BH�BH�BH�BI�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BK�BL�BL�BL�BL�BM�BM�BN�BN�BN�BN�BN�BO�BP�BQ�BQ�BQ�BQ�BQ�BQ�BR�BR�BR�BR�BR�BR�BR�BS�BS�BS�BS�BS�BT�BT�BVBVBVBVBW
BW
BW
BW
BW
BW
BXBXBXBXBXBXBXBYBYBZBZBZBZBZB[#B[#B[#B\)B]/B]/B]/B]/B]/B]/B]/B^5B^5B^5B^5B_;B_;B`BB`BB`BB`BBaHBaHBaHBbNBbNBbNBbNBbNBbNBbNBcTBcTBdZBdZBdZBdZBe`Be`Be`Be`BffBffBffBffBffBffBgmBgmBgmBgmBgmBhsBhsBhsBiyBiyBiyBjBjBjBjBjBjBjBk�Bk�Bk�Bk�Bk�Bl�Bl�Bl�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bo�Bo�Bo�Bp�Bp�Bp�Bp�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 B6FB6FB5?B5?B5?B49B49B33B33B33B33B49B49B33B2-B/B(�B#�B�B�B�B%B��B�7BaHBXBS�BN�BK�BG�BC�B=qB;dB6FB33B2-B-B)�B'�B"�B!�B�B�B�B�B�B�BPB	7B  B��B�B�fB�;B�/B�)B�B��B��B��B��B��B�
B��B�^B�-B��B�VB� Bv�Bw�B�B�B�By�Bn�BO�B@�B:^B7LB/B\B��B�B�fB�BɺB�-B�BffBI�B;dB-B�BB�B��B�}B�B�B�B��B��B��B�oB�DB� B{�By�Bk�B]/BT�BF�B:^B5?B1'B+B �B�BPBB
��B
��B
�B
��B
�B
�B
�B
�B
�B
�sB
�HB
�`B
�TB
�)B
��B
��B
ƨB
ÖB
��B
�}B
�qB
�jB
�wB
��B
�wB
�dB
�dB
�dB
�XB
�RB
�FB
�?B
�3B
�-B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�hB
�JB
�DB
�7B
�+B
�B
{�B
t�B
l�B
gmB
ffB
`BB
]/B
W
B
R�B
M�B
G�B
E�B
E�B
@�B
>wB
=qB
9XB
7LB
6FB
49B
1'B
1'B
/B
-B
-B
,B
)�B
(�B
$�B
#�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
hB
VB
JB
1B
%B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�mB	�mB	�fB	�`B	�ZB	�TB	�NB	�HB	�HB	�BB	�BB	�BB	�;B	�;B	�;B	�HB	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�BB	�BB	�NB	�NB	�NB	�ZB	�fB	�`B	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
+B

=B
JB
VB
\B
hB
oB
{B
�B
�B
�B
�B
�B
�B
"�B
$�B
%�B
%�B
&�B
'�B
/B
1'B
49B
6FB
;dB
;dB
>wB
@�B
A�B
B�B
D�B
E�B
G�B
I�B
L�B
N�B
R�B
S�B
T�B
VB
XB
]/B
bNB
ffB
hsB
iyB
l�B
m�B
n�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
z�B
~�B
� B
�B
�B
�B
�B
�7B
�DB
�PB
�PB
�VB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�!B
�-B
�3B
�9B
�RB
�qB
��B
B
ÖB
ƨB
ǮB
��B
��B
��B
�B
�B
�
B
�B
�B
�B
�#B
�)B
�5B
�BB
�NB
�sB
�sB
�B
�B
�B
�B
��B
��B
��B  BB+BJBhBhBuBuB{B�B�B�B�B�B�B!�B"�B%�B'�B(�B+B.B/B0!B2-B33B49B6FB7LB8RB:^B;dB<jB<jB=qB=qB>wB?}B?}BC�BD�BF�BG�BH�BH�BH�BI�BK�BN�BQ�BQ�BR�BT�BYB[#B]/B_;B`BBbNBcTBdZBffBhsBk�Bl�Bm�Bn�Bo�Bq�Br�Bs�Bs�Bu�Bv�Bv�Bw�By�B{�B}�B}�B�B�B�%B�DB�JB�VB�VB�VB�\B�\B�\B�hB�oB�oB�uB�uB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�3B�?B�RB�XB�^B�^B�jB�jB�qB�}B��B��B��BÖBĜBƨBǮBȴBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�B�B�#B�#B�5B�;B�BB�BB�NB�TB�TB�TB�`B�`B�fB�mB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B  B  B  B  B  B  BBBBBBBB%B1B1B1B1B1B	7B	7BDBJBVB\B\B\B\B\B\B\BbBhBoBoBoBoBoBoBuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B!�B!�B!�B"�B#�B$�B$�B$�B%�B%�B%�B&�B&�B'�B(�B(�B(�B(�B)�B+B+B+B+B,B,B,B,B-B-B.B.B.B.B/B/B/B0!B0!B0!B1'B1'B1'B1'B2-B33B33B33B49B49B49B5?B5?B5?B6FB6FB6FB6FB6FB7LB8RB8RB8RB8RB9XB9XB9XB9XB9XB9XB9XB9XB:^B:^B;dB;dB;dB;dB;dB;dB;dB<jB<jB<jB=qB=qB>wB>wB?}B?}B?}B?}B?}B@�B@�B@�B@�BA�BB�BB�BB�BB�BB�BB�BC�BD�BD�BD�BD�BD�BD�BE�BE�BE�BE�BE�BF�BF�BF�BG�BG�BG�BG�BG�BG�BH�BH�BH�BI�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BK�BL�BL�BL�BL�BM�BM�BN�BN�BN�BN�BN�BO�BP�BQ�BQ�BQ�BQ�BQ�BQ�BR�BR�BR�BR�BR�BR�BR�BS�BS�BS�BS�BS�BT�BT�BVBVBVBVBW
BW
BW
BW
BW
BW
BXBXBXBXBXBXBXBYBYBZBZBZBZBZB[#B[#B[#B\)B]/B]/B]/B]/B]/B]/B]/B^5B^5B^5B^5B_;B_;B`BB`BB`BB`BBaHBaHBaHBbNBbNBbNBbNBbNBbNBbNBcTBcTBdZBdZBdZBdZBe`Be`Be`Be`BffBffBffBffBffBffBgmBgmBgmBgmBgmBhsBhsBhsBiyBiyBiyBjBjBjBjBjBjBjBk�Bk�Bk�Bk�Bk�Bl�Bl�Bl�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bo�Bo�Bo�Bp�Bp�Bp�Bp�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.17 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210817090046                              AO  ARCAADJP                                                                    20210817090046    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210817090046  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210817090046  QCF$                G�O�G�O�G�O�8000            