CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-12-13T00:36:44Z creation;2018-12-13T00:36:50Z conversion to V3.1;2019-12-19T07:25:51Z update;     
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
resolution        =���   axis      Z        d  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181213003644  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              6A   JA  I2_0576_310                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @ؗ��Ӡ�1   @ؗ�www�@9>5?|��d4C,�zx1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dx��Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D���D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ Dм�D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�<�Dր D�� D�  D�@ D׀ D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�3D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @{�@��\@�\)AG�A=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BG�RBOQ�BWQ�B_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�Da{�Da�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�DwuDw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�=�D�z�D���D��\D�:�D�z�D��\D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dз\D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�7\D�z�Dպ�D���D�7\D�z�Dֺ�D���D�:�D�z�D׺�D��\D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�=�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�w\D⺏D���D�:�D�z�D㺏D���D�:�D�z�D亏D���D�:�D�z�D序D���D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D躏D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�=�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�z�D���D���D�=�D�w\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AA+A7A+A7APADAPA7A7A7A7A7ADADADAPADAPAPAPAPAPAPAPA\AhAhAhAhAuAuAhAhAhAhAuA+A��/A��#A��PA�p�A���A���A�ȴA�n�A�I�A��`A�ȴA�?}A���A�{A��DA���A���A�G�A���A�`BA�?}A�dZA��A�^5A�-A���A�A��A�  A��/A���A�O�A�bNA��HA��A�A�A���A���A�S�A��A�dZA�-A�"�A���A�r�A�5?A�hsA��FA��FA���A��/A�%A��A�~�A��yA�p�A�1A���A���A���A��+A��9A�Q�A�oA���A��;A��\A�7LA���A�^A}��A{\)Ay�Awx�Au��At��Ar�Ao&�Aj1'Ag��Af��AfZAe\)Ad��Acl�Ab��Aa|�A`=qA_|�A^��A\��AY�AX��AX��AX(�AW�
AW"�AUƨAS��AR  AQ�AO`BAO"�AL�+AJ�DAIO�AH$�AG��AG�7AG7LAE��ACAB�\A@A�A?/A>��A>M�A<�A;��A;/A:�/A:��A8^5A6�A5��A5�A5VA4�uA3�
A3�A2�`A2E�A1O�A0��A0�A/�A.-A-O�A,bNA+�A+7LA*��A)`BA(��A(VA'�;A'G�A&�yA&��A& �A%�A%K�A$�A#p�A"��A!�A ��A33A-A�^A7LA{A��AVA��A33AffA��A�\A�A�7A%A�#Al�A�`A�AE�A�
A%AQ�A-AAA�#A�hA
�9A	�hA~�Ap�A��A�AAƨA�PAx�AK�A��Al�A�A��A �A7L@��;@��@���@�n�@��@�/@�=q@��/@���@�^5@��@���@�@�A�@�"�@��@���@�@��@��m@�P@�@��m@�1@�=q@�J@�^@�dZ@�=q@�x�@�z�@��@�ƨ@�+@�$�@ܴ9@�S�@�^5@ו�@�hs@�z�@�;d@�ȴ@��#@д9@���@϶F@�|�@�K�@��@Ο�@�~�@�{@͡�@��@ʧ�@�Q�@��H@�z�@�E�@���@��@��@�Z@�33@��D@��R@�X@��@�Q�@���@��y@���@��@��@�33@���@�E�@�@��7@�bN@�+@�=q@��@�bN@�ƨ@�E�@���@��7@�O�@��9@�  @�C�@�-@��7@�x�@�x�@��@��7@�p�@�Q�@��@���@��@�G�@��@��u@���@�o@���@��!@�ȴ@��!@�@��7@�?}@�&�@���@���@�Z@�t�@�$�@��@��#@��^@�O�@�&�@�V@���@���@��@��`@�Ĝ@�  @���@�l�@�l�@�l�@�t�@�t�@�|�@�|�@�|�@�|�@�dZ@�o@��H@��R@�n�@���@�7L@���@��w@�C�@�33@�@��@��H@��@��R@�ff@�J@���@�%@�I�@�  @��w@�dZ@�
=@��y@���@�n�@�-@���@�O�@��/@��D@�1'@��m@���@�t�@�-@��h@�%@���@���@�9X@��m@���@�l�@�\)@�C�@�
=@��!@�M�@��#@��7@�/@���@���@��@�Z@�A�@�1@��@��m@��
@���@�l�@�"�@��H@��R@�v�@�$�@�@��T@���@�X@�/@���@���@�Z@�b@�@~ȴ@~�+@~5?@~5?@~v�@~�+@~�+@~V@}�@}p�@}�@}�@}�@|�@|�/@|�@|9X@{ƨ@{dZ@{S�@{o@z�H@zn�@z=q@y��@yX@x��@x�9@x�@xbN@x �@w|�@vV@u@u`B@u?}@t�@s�
@s"�@r�!@rn�@rJ@q��@q��@q��@qX@qG�@q7L@q�@pĜ@o�;@o\)@o�@n��@n��@n�@nȴ@n�R@n��@nv�@m�T@l�@l�j@l��@l9X@k�
@k�F@k��@k�@kt�@kdZ@k33@j�@j�\@j^5@j=q@j-@i��@i�#@i��@ix�@ihs@iG�@h��@h�@hQ�@h1'@h  @gl�@g
=@f�y@e�h@eV@d��@cƨ@c"�@b�!@bn�@a��@a%@`Q�@_K�@^��@^V@^$�@^{@]�@]�T@]�T@]��@]�-@]�@]p�@]�@]p�@]`B@]?}@\�j@\�D@\Z@\1@Z�!@Z-@Y�@Y��@Yhs@YG�@YG�@YX@Y�@X�9@X��@XA�@V��@VE�@U@U��@UV@T�/@T�j@SS�@R��@R��@R-@Q7L@Pr�@Pr�@Pr�@PQ�@PA�@Pb@O+@N�R@N��@Nff@N5?@N{@M�@M@M��@Mp�@M/@L�/@L�@L�@L�j@L��@L9X@K�m@K��@Ko@J�\@JM�@JJ@I&�@G�@F�R@Fv�@Fff@Fff@Fff@Fff@FV@FE�@F$�@F@E�T@E@E�@E/@D�D@C�
@C@B^5@B�@A�#@Ahs@A&�@A�@A%@@��@@bN@@b@?�;@?�@?��@?�;@?��@?l�@?;d@?
=@>�y@>��@>�+@>v�@>$�@=��@<��@<�D@<I�@<(�@;�m@;��@;t�@;o@:�@:��@:�!@:-@9��@9x�@9hs@9hs@97L@8�9@81'@8  @7�;@7��@7l�@7�@6�y@6��@65?@5�@5�-@5�@5O�@5�@4��@4j@4I�@41@3��@3�
@3ƨ@3��@3t�@3S�@3C�@2�@2�\@0�u@/K�@/
=@.ff@,��@,��@,��@,I�@+t�@+o@+S�@*��@*��@*��@*�\@)��@)x�@(��@(�u@(�u@(r�@(A�@(  @'��@'�@'��@'��@'�P@'l�@'K�@'+@'�@'�@&��@&�@&ȴ@&��@&�+@&ff@&ff@&ff@&ff@&V@&E�@&E�@&E�@&E�@&5?@&5?@&$�@&{@&{@&{@&5?@&E�@&E�@&E�@&5?@&$�@&@%�@%�T@%@%@%@%@%�-@%�-@%�-@%��@%�h@%�h@%�@%�@%�@%�@%�@%�@%p�@%O�@%O�@%/@%�@$�@#��@!��@ ��@ 1'@�P@;d@
=@�y@
=@�y@�y@�y@��@E�@5?@{@@��@�-@�h@p�@`B@/@��@1@�m@�F@��@t�@"�@~�@^5@M�@M�@M�@=q@-@�@��@�#@��@G�@Ĝ@�u@bN@A�@A�@b@�@  @�@�;@�;@�;@��@�w@�P@K�@;d@�@
=@��@�y@ȴ@�+@V@5?@$�@@@@�@�T@��@�@�/@I�@�@t�@��@~�@^5@^5@M�@-@J@��@��@�7@�@Ĝ@�9@��@�@Q�@ �@  @�;@�@�P@l�@\)@;d@
=@�@ȴ@�R@��@E�@E�@5?@$�@�T@��@��@��@��@�h@�h@�h@�h@��@�h@�h@�@`B@O�@O�@?}@�@��@�@�@�/@�/@��@��@��@�D@Z@Z@I�@9X@(�@1@�
@t�@"�@
�H@
�!@
^5@
J@	�^@	��@	�7@	hs@	G�@	7L@	7L@��@�@b@�@|�@l�@K�@��@v�@v�@v�@v�@v�@v�@v�@�+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AA+A7A+A7APADAPA7A7A7A7A7ADADADAPADAPAPAPAPAPAPAPA\AhAhAhAhAuAuAhAhAhAhAuA+A��/A��#A��PA�p�A���A���A�ȴA�n�A�I�A��`A�ȴA�?}A���A�{A��DA���A���A�G�A���A�`BA�?}A�dZA��A�^5A�-A���A�A��A�  A��/A���A�O�A�bNA��HA��A�A�A���A���A�S�A��A�dZA�-A�"�A���A�r�A�5?A�hsA��FA��FA���A��/A�%A��A�~�A��yA�p�A�1A���A���A���A��+A��9A�Q�A�oA���A��;A��\A�7LA���A�^A}��A{\)Ay�Awx�Au��At��Ar�Ao&�Aj1'Ag��Af��AfZAe\)Ad��Acl�Ab��Aa|�A`=qA_|�A^��A\��AY�AX��AX��AX(�AW�
AW"�AUƨAS��AR  AQ�AO`BAO"�AL�+AJ�DAIO�AH$�AG��AG�7AG7LAE��ACAB�\A@A�A?/A>��A>M�A<�A;��A;/A:�/A:��A8^5A6�A5��A5�A5VA4�uA3�
A3�A2�`A2E�A1O�A0��A0�A/�A.-A-O�A,bNA+�A+7LA*��A)`BA(��A(VA'�;A'G�A&�yA&��A& �A%�A%K�A$�A#p�A"��A!�A ��A33A-A�^A7LA{A��AVA��A33AffA��A�\A�A�7A%A�#Al�A�`A�AE�A�
A%AQ�A-AAA�#A�hA
�9A	�hA~�Ap�A��A�AAƨA�PAx�AK�A��Al�A�A��A �A7L@��;@��@���@�n�@��@�/@�=q@��/@���@�^5@��@���@�@�A�@�"�@��@���@�@��@��m@�P@�@��m@�1@�=q@�J@�^@�dZ@�=q@�x�@�z�@��@�ƨ@�+@�$�@ܴ9@�S�@�^5@ו�@�hs@�z�@�;d@�ȴ@��#@д9@���@϶F@�|�@�K�@��@Ο�@�~�@�{@͡�@��@ʧ�@�Q�@��H@�z�@�E�@���@��@��@�Z@�33@��D@��R@�X@��@�Q�@���@��y@���@��@��@�33@���@�E�@�@��7@�bN@�+@�=q@��@�bN@�ƨ@�E�@���@��7@�O�@��9@�  @�C�@�-@��7@�x�@�x�@��@��7@�p�@�Q�@��@���@��@�G�@��@��u@���@�o@���@��!@�ȴ@��!@�@��7@�?}@�&�@���@���@�Z@�t�@�$�@��@��#@��^@�O�@�&�@�V@���@���@��@��`@�Ĝ@�  @���@�l�@�l�@�l�@�t�@�t�@�|�@�|�@�|�@�|�@�dZ@�o@��H@��R@�n�@���@�7L@���@��w@�C�@�33@�@��@��H@��@��R@�ff@�J@���@�%@�I�@�  @��w@�dZ@�
=@��y@���@�n�@�-@���@�O�@��/@��D@�1'@��m@���@�t�@�-@��h@�%@���@���@�9X@��m@���@�l�@�\)@�C�@�
=@��!@�M�@��#@��7@�/@���@���@��@�Z@�A�@�1@��@��m@��
@���@�l�@�"�@��H@��R@�v�@�$�@�@��T@���@�X@�/@���@���@�Z@�b@�@~ȴ@~�+@~5?@~5?@~v�@~�+@~�+@~V@}�@}p�@}�@}�@}�@|�@|�/@|�@|9X@{ƨ@{dZ@{S�@{o@z�H@zn�@z=q@y��@yX@x��@x�9@x�@xbN@x �@w|�@vV@u@u`B@u?}@t�@s�
@s"�@r�!@rn�@rJ@q��@q��@q��@qX@qG�@q7L@q�@pĜ@o�;@o\)@o�@n��@n��@n�@nȴ@n�R@n��@nv�@m�T@l�@l�j@l��@l9X@k�
@k�F@k��@k�@kt�@kdZ@k33@j�@j�\@j^5@j=q@j-@i��@i�#@i��@ix�@ihs@iG�@h��@h�@hQ�@h1'@h  @gl�@g
=@f�y@e�h@eV@d��@cƨ@c"�@b�!@bn�@a��@a%@`Q�@_K�@^��@^V@^$�@^{@]�@]�T@]�T@]��@]�-@]�@]p�@]�@]p�@]`B@]?}@\�j@\�D@\Z@\1@Z�!@Z-@Y�@Y��@Yhs@YG�@YG�@YX@Y�@X�9@X��@XA�@V��@VE�@U@U��@UV@T�/@T�j@SS�@R��@R��@R-@Q7L@Pr�@Pr�@Pr�@PQ�@PA�@Pb@O+@N�R@N��@Nff@N5?@N{@M�@M@M��@Mp�@M/@L�/@L�@L�@L�j@L��@L9X@K�m@K��@Ko@J�\@JM�@JJ@I&�@G�@F�R@Fv�@Fff@Fff@Fff@Fff@FV@FE�@F$�@F@E�T@E@E�@E/@D�D@C�
@C@B^5@B�@A�#@Ahs@A&�@A�@A%@@��@@bN@@b@?�;@?�@?��@?�;@?��@?l�@?;d@?
=@>�y@>��@>�+@>v�@>$�@=��@<��@<�D@<I�@<(�@;�m@;��@;t�@;o@:�@:��@:�!@:-@9��@9x�@9hs@9hs@97L@8�9@81'@8  @7�;@7��@7l�@7�@6�y@6��@65?@5�@5�-@5�@5O�@5�@4��@4j@4I�@41@3��@3�
@3ƨ@3��@3t�@3S�@3C�@2�@2�\@0�u@/K�@/
=@.ff@,��@,��@,��@,I�@+t�@+o@+S�@*��@*��@*��@*�\@)��@)x�@(��@(�u@(�u@(r�@(A�@(  @'��@'�@'��@'��@'�P@'l�@'K�@'+@'�@'�@&��@&�@&ȴ@&��@&�+@&ff@&ff@&ff@&ff@&V@&E�@&E�@&E�@&E�@&5?@&5?@&$�@&{@&{@&{@&5?@&E�@&E�@&E�@&5?@&$�@&@%�@%�T@%@%@%@%@%�-@%�-@%�-@%��@%�h@%�h@%�@%�@%�@%�@%�@%�@%p�@%O�@%O�@%/@%�@$�@#��@!��@ ��@ 1'@�P@;d@
=@�y@
=@�y@�y@�y@��@E�@5?@{@@��@�-@�h@p�@`B@/@��@1@�m@�F@��@t�@"�@~�@^5@M�@M�@M�@=q@-@�@��@�#@��@G�@Ĝ@�u@bN@A�@A�@b@�@  @�@�;@�;@�;@��@�w@�P@K�@;d@�@
=@��@�y@ȴ@�+@V@5?@$�@@@@�@�T@��@�@�/@I�@�@t�@��@~�@^5@^5@M�@-@J@��@��@�7@�@Ĝ@�9@��@�@Q�@ �@  @�;@�@�P@l�@\)@;d@
=@�@ȴ@�R@��@E�@E�@5?@$�@�T@��@��@��@��@�h@�h@�h@�h@��@�h@�h@�@`B@O�@O�@?}@�@��@�@�@�/@�/@��@��@��@�D@Z@Z@I�@9X@(�@1@�
@t�@"�@
�H@
�!@
^5@
J@	�^@	��@	�7@	hs@	G�@	7L@	7L@��@�@b@�@|�@l�@K�@��@v�@v�@v�@v�@v�@v�@v�@�+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BW
BW
BVBW
BW
BW
BW
BVBW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BVBVBVBS�BQ�BI�B2-BDBbB�B�B�/BcTBP�B�B,B9XB/B��B�BuB2-B=qB5?B,B�B	7BJB�B�B�BB�
B��BJBB�B�/B�3Bt�B��B��B��B�B��B�7B}�Bz�BdZB_;BiyBVB,B
�B
��B
��B
��B
ÖB
��B
ŢB
�?B
�B
��B
��B
�=B
x�B
�B
z�B
�%B
�%B
x�B
cTB
\)B
J�B
�B
�B
B	�B	�ZB	�B	��B	�XB	��B	p�B	P�B	W
B	~�B	�B	v�B	w�B	m�B	o�B	l�B	_;B	`BB	VB	7LB	bB	8RB	O�B	P�B	N�B	?}B	+B	DB	uB	PB	%B	hB�B�fB��B��B	1B	%B��B�fB��B�
B�}B��B�B��B�jB�LBȴBŢB�XB��B��B��B�3B�!B�B��B��B�B��B��B��B��B��By�B�7B�%B�=B�uB�JBz�B�B�JB�=B�B�+B�B}�Bw�B{�Br�B[#BiyB\)BJ�BP�BQ�B_;BZBI�BK�BXBW
BS�BO�BL�BG�BO�BR�BP�BD�BL�BJ�B=qB1'BC�BA�BB�BK�BF�B5?B.B=qB2-B$�B(�B+B0!B;dBD�BA�B?}B@�B9XB,B�B5?B1'B(�B �B�B49B9XB6FB1'B'�BhB%�B�B7LB8RB6FB33B%�B'�B49B1'B'�B�B.B%�B�B
=B  B�B.B&�B�B�B(�B&�B.B.B'�B �B�B�B�BJB{B(�B'�B2-B.B.B5?B<jB=qB<jB;dB:^B:^B5?B0!B(�B�B�B#�B �B,B?}BB�BB�B;dB0!B!�B0!B:^BG�BE�BG�BH�BL�BG�B=qBP�BT�BVBW
BR�BL�BN�BT�BT�B]/B_;BZBiyBq�Bn�Bk�BjBl�Bn�Bv�B�B�B� B}�Bw�Bn�Bq�Bv�B�7B�=B�DB�+B�+B�JB�{B��B��B��B�{B��B��B��B��B��B��B�{B��B��B�B�B��B�B�!B�'B�-B�'B�'B�B��B�-B�XB�wB�}B��B��BBB��BÖBBBŢBŢBÖB��BȴBɺB��B�
B�5B�5B�;B�NB�HB�;B�;B�HB�ZB�TB�sB�B��B��B��B��B��B��B��B	  B	%B	+B	JB	PB	\B	hB	VB		7B	�B	�B	&�B	)�B	)�B	,B	0!B	49B	6FB	6FB	5?B	49B	8RB	;dB	@�B	C�B	F�B	L�B	N�B	O�B	R�B	R�B	XB	YB	ZB	YB	\)B	]/B	_;B	bNB	cTB	dZB	hsB	iyB	iyB	jB	o�B	p�B	q�B	s�B	u�B	w�B	}�B	�B	�B	�=B	�VB	�\B	�\B	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	�B	�!B	�3B	�?B	�?B	�LB	�RB	�LB	�LB	�^B	�dB	�^B	�XB	�RB	�jB	��B	��B	��B	��B	��B	��B	��B	�}B	�jB	�qB	ĜB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�#B	�)B	�#B	�B	�/B	�/B	�ZB	�ZB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
%B
%B
%B
B
B
B
%B
	7B
1B
	7B

=B

=B

=B

=B

=B

=B
DB
JB
PB
PB
JB

=B
DB
JB

=B
JB
JB
DB
1B
+B
PB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
oB
bB
bB
hB
{B
�B
�B
�B
�B
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
!�B
!�B
 �B
"�B
!�B
�B
�B
�B
#�B
$�B
%�B
$�B
$�B
%�B
%�B
'�B
(�B
'�B
&�B
'�B
,B
-B
-B
,B
)�B
,B
/B
/B
/B
/B
/B
/B
/B
.B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
5?B
5?B
5?B
5?B
6FB
5?B
5?B
5?B
5?B
2-B
1'B
,B
0!B
9XB
7LB
33B
<jB
<jB
;dB
:^B
=qB
@�B
=qB
@�B
?}B
>wB
<jB
>wB
<jB
B�B
B�B
B�B
A�B
A�B
B�B
B�B
C�B
C�B
B�B
B�B
B�B
C�B
D�B
C�B
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
F�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
F�B
F�B
F�B
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
F�B
F�B
F�B
F�B
G�B
G�B
F�B
G�B
G�B
G�B
F�B
F�B
E�B
F�B
D�B
C�B
@�B
;dB
9XB
B�B
E�B
H�B
J�B
L�B
M�B
N�B
M�B
N�B
M�B
L�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
N�B
M�B
L�B
N�B
Q�B
Q�B
R�B
R�B
Q�B
Q�B
T�B
VB
VB
VB
VB
VB
VB
T�B
S�B
S�B
R�B
S�B
W
B
W
B
XB
YB
XB
YB
ZB
YB
YB
ZB
YB
YB
YB
XB
XB
ZB
ZB
ZB
[#B
ZB
ZB
YB
ZB
[#B
[#B
\)B
\)B
\)B
[#B
[#B
ZB
XB
W
B
XB
[#B
YB
ZB
_;B
aHB
aHB
`BB
`BB
`BB
`BB
`BB
_;B
_;B
aHB
cTB
cTB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
dZB
dZB
e`B
ffB
ffB
e`B
e`B
gmB
gmB
ffB
ffB
gmB
hsB
hsB
gmB
hsB
hsB
hsB
iyB
iyB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
hsB
hsB
iyB
jB
iyB
iyB
jB
jB
iyB
hsB
iyB
jB
jB
jB
jB
iyB
iyB
hsB
iyB
jB
jB
jB
jB
k�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
k�B
k�B
m�B
n�B
o�B
o�B
n�B
n�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BW$BW
BVBW$BW$BW
BW
BVBW
BW$BW
BW$BW$BW$BW
BW$BW$BW$BW$BW$BW
BW$BW
BW
BW$BW$BW$BW
BW$BW
BW
BVBVBVBTBR BJrB4�B�B�B��B�tB��BdZBR�B$�B/B;�B1�B B�8BSB3�B>(B6FB-CB!HB�B<B�B�ByB�BۦB��B�B{B�nBބB�+By�B��B�]B��B�GB�EB�XBHB{�Bf�B`vBi�BW
B.�B
�B
��B
��B
��B
�SB
�hB
ƨB
��B
�;B
�*B
��B
�B
{JB
�gB
|�B
��B
��B
z*B
e,B
]/B
L0B
qB
�B
�B	�B	��B	�)B	��B	��B	��B	utB	VB	Y�B	�B	��B	x8B	x�B	o5B	p�B	nB	`�B	abB	W?B	:B	�B	9rB	O�B	Q�B	O�B	@�B	-B	VB	gB	�B	KB	TB��B��B�lB�JB	�B	�B��B�B��BخB�[B�(BخB��B�]B��B�RB�YB�DB��B�kB�B��B��B�B�
B��B�WB��B�B��B�B��B|6B�rB��B�xB��B�6B|�B��B��B�B��B��B��B~�Bx�B|jBs�B]IBjeB]�BL�BR�BS[B`B[	BK�BM�BX�BW�BUBQ4BN"BIBP�BS�BQ�BF?BM�BK�B>�B3�BD�BB�BC�BL0BGEB6�B/�B>B3�B&�B*B,�B1[B<BD�BA�B?�B@�B:B-)B!|B5�B1�B*0B"BjB4�B9�B6�B1�B(�BuB'B!bB7�B8�B6�B3�B'B(�B4�B1�B(�B�B.IB&�B�BB�B�B.cB'�BKB�B)�B'�B.}B.}B(�B!�B�B�B�B"B�B)�B(�B2�B/ B/ B5�B<�B=�B<�B;�B:�B:�B5�B0�B)�B�BWB%B"�B-�B@ BCBB�B<B1[B#�B1vB;dBHBFYBHfBIlBM6BH�B>�BQNBU�BVmBWsBS�BM�BO�BU�BU�B]�B_�B[WBi�Bq�Bn�Bl=Bk6BmCBo�BwLB�'B�;B�4B~(Bx8Bo�Br�Bw�B�lB��B��B��B��B��B��B��B��B��B�B��B��B��B��B�B�$B�gB��B�0B�CB�WB�B�OB�UB�[B�aB�[B�[B�}B��B��B��B��B��B��B��BªBªB��BðB��B��B��B��B�B�;B�7B�=BϑB�sB�OB�jBߊB�hB�|BߊBߊB�B��B�B�B�B�B�2B�$B�<B�PB�<B�HB	 �B	�B	�B	�B	�B	�B	�B	�B	
XB	+B	 BB	'B	*KB	*KB	,qB	0oB	4nB	6zB	6�B	5�B	4�B	8�B	;�B	@�B	C�B	GB	MB	OB	PB	SB	S@B	XEB	YKB	ZQB	YKB	\xB	]~B	_pB	b�B	c�B	d�B	h�B	i�B	i�B	j�B	o�B	p�B	rB	tB	vB	x8B	~(B	�'B	�SB	�XB	�VB	�vB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�$B	�8B	�,B	�zB	�=B	�OB	�;B	�cB	�}B	�oB	�hB	�tB	�tB	�fB	�lB	��B	��B	�^B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�.B	�.B	� B	� B	� B	�.B	�:B	�&B	�\B	�FB	�SB	�MB	�eB	�WB	�xB	یB	چB	ݘB	ݘB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�;B	�B	�B	�B	�*B	�B	�0B	�LB	�*B	�.B	�<B	�jB
 OB
?B
?B
YB
SB
3B
oB
YB
	RB
fB
	lB

rB

rB

rB

rB

rB

rB
xB
dB
jB
jB
~B

�B
^B
~B

�B
�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
 �B
"�B
!�B
 B
 B
!B
$B
%B
&B
$�B
%B
&B
%�B
($B
)*B
(
B
'8B
($B
,"B
-)B
-)B
,=B
*0B
,WB
/5B
/5B
/OB
/OB
/OB
/OB
/5B
.IB
1[B
1[B
2aB
2aB
2aB
2GB
2aB
5tB
5ZB
5ZB
5tB
6`B
5tB
5ZB
5tB
5ZB
2|B
1vB
,�B
0�B
9�B
7�B
3�B
<jB
<�B
;�B
:�B
=�B
@�B
=�B
@�B
?�B
>�B
<�B
>�B
<�B
B�B
B�B
B�B
A�B
A�B
B�B
B�B
C�B
C�B
B�B
B�B
B�B
C�B
D�B
C�B
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
F�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
F�B
F�B
F�B
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
F�B
F�B
F�B
F�B
G�B
G�B
F�B
G�B
G�B
G�B
F�B
F�B
E�B
F�B
D�B
C�B
@�B
<B
:*B
B�B
FB
H�B
J�B
MB
M�B
N�B
M�B
N�B
M�B
L�B
NB
N�B
N�B
OB
OB
OB
N�B
O�B
OB
NB
M6B
O(B
RB
R B
SB
S&B
R:B
R:B
UB
VB
VB
VB
VB
VB
VB
U2B
TB
T,B
S@B
TFB
W?B
W?B
XEB
Y1B
XEB
YB
ZB
Y1B
YB
Z7B
Y1B
Y1B
Y1B
XEB
XEB
Z7B
ZQB
ZQB
[=B
ZQB
ZQB
YKB
ZQB
[=B
[=B
\CB
\)B
\CB
[=B
[=B
ZQB
XEB
WYB
X_B
[WB
YB
Z�B
_pB
abB
aHB
`\B
`vB
`vB
`vB
`vB
_VB
_�B
a|B
cnB
cnB
b�B
b�B
cnB
c�B
cnB
dtB
dtB
d�B
ezB
d�B
d�B
e�B
f�B
f�B
e�B
ezB
gmB
gmB
f�B
f�B
g�B
h�B
h�B
g�B
hsB
h�B
h�B
i�B
i�B
h�B
h�B
hsB
h�B
hsB
i�B
i�B
h�B
h�B
i�B
jB
i�B
i�B
j�B
j�B
i�B
h�B
i�B
j�B
j�B
j�B
jB
i�B
i�B
h�B
i�B
j�B
j�B
j�B
j�B
k�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
k�B
k�B
m�B
n�B
o�B
o�B
n�B
n�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.17(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201812170038082018121700380820181217003808201812170200142018121702001420181217020014201812180033012018121800330120181218003301  JA  ARFMdecpA19c                                                                20181213093642  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181213003644  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181213003648  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181213003648  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181213003649  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181213003649  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181213003649  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181213003649  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181213003649  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181213003650                      G�O�G�O�G�O�                JA  ARUP                                                                        20181213005604                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181213153550  CV  JULD            G�O�G�O�FĽ�                JM  ARCAJMQC2.0                                                                 20181216153808  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181216153808  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181216170014  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181217153301  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                