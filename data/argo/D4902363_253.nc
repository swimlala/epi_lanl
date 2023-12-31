CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-24T21:35:29Z creation;2018-06-24T21:35:33Z conversion to V3.1;2019-12-19T07:39:04Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180624213529  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_253                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�l�ҭ��1   @�l󙙙�@9��{���dDJ�L�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.fD.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DF��DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�C3Dȃ3D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @{�@��\@��\AG�A=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��qC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D{�D��D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-��D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�DwuDw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�=�D�}�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�=�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�z�D⺏D���D�:�D�z�D㺏D���D�:�D�z�D亏D���D�:�D�z�D序D���D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D躏D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D� �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�  A���A���A���A��A��#A��/Aȏ\A��AǴ9A���A�bNA�p�A��9A�hsA��;A���A�XA�A��7A�p�A�S�A�5?A���A�9XA��A��A�v�A���A��jA�;dA��yA��;A���A���A��A�{A��A�I�A�v�A���A���A���A���A�p�A���A��yA�E�A�ƨA�\)A��A��A�l�A��A��^A��A���A�  A��HA���A�A���A��FA�hsA�=qA���A�l�A��+A��A��A��A��mA�ĜA�z�A�/A���A�oA�bNA�S�A�VA��A�`BA��`A�\)A�9XA��A~Q�A}
=A{��AzbAx�Av  Aul�Au�AsAr�\Ao�Am�-Ak��Aj�AjI�Ai�7Ahn�AgG�AfZAe�hAd��AdJAb�HAbȴAbbAa`BA`=qA_33A]��A\�jA[��AZ�!AXQ�AVJAU�ATE�ARAPn�AOdZAMG�AL��AK�hAGƨAF�!AFz�AE�AE�AC��AC�AB��AA�FAAt�A@ĜA?�;A?`BA>�A=A:�!A8��A8ĜA8bA7S�A7
=A6ĜA6��A6��A6v�A5��A4�yA4�A4Q�A4Q�A4I�A3��A2(�A1�-A1dZA0�A0A/x�A/�A.�yA.��A.JA-ƨA-G�A,1'A+�A*$�A(�A'�A&��A%��A$ĜA$(�A#�^A#
=A"^5A!��A!7LA ��A ��A ZAx�A��A�
Ax�A(�A�`A�DA�AS�A�TA�PAS�A+A��A�+AI�A$�A�A��At�A�AA�Ar�A��A��A�+A7LA-A
��A
I�A
(�A
 �A
bA
  A	�A	�wA�HA�TA��A��AffA�A�-A��AhsAr�A=qA�A|�A �u@��@�S�@���@�&�@�bN@��F@�+@�J@���@�b@�ff@��@�E�@�ƨ@���@��@�9@�Z@��m@�ƨ@��@�S�@���@�~�@�J@�Ĝ@�@��@���@ݙ�@݉7@݉7@݁@݁@�p�@�&�@۝�@�?}@�n�@���@�O�@�/@��@�%@���@ԛ�@�1'@ӕ�@ҟ�@���@��@�7L@̬@�Q�@�b@�ƨ@ʰ!@�O�@ǶF@�@��@Ĵ9@ÍP@�@°!@�^5@���@���@�?}@� �@�@�J@�G�@�V@��`@���@�(�@��F@�33@�E�@��@��F@���@�&�@�dZ@���@��@�r�@�j@�I�@��@��@���@�-@��j@��
@���@���@���@�z�@�b@�b@��w@���@�"�@��@�%@��/@�Ĝ@���@� �@���@��#@�/@���@��9@���@�z�@�I�@�9X@��
@���@���@�\)@��@��\@�-@�hs@��@�ƨ@�^5@��#@�V@�I�@�  @���@�
=@��+@�$�@�p�@��@�K�@���@��\@�J@��7@�X@�?}@�%@��j@��9@��@���@��u@��@�Q�@���@��;@�C�@���@���@��\@��\@�~�@�ff@�{@�G�@�Q�@�\)@��@�M�@�J@���@���@��@��@�Z@�(�@�ƨ@�|�@�+@�o@�@��@��@���@�n�@�V@�E�@�5?@�J@���@���@��D@��D@��@��@��D@���@���@��D@�r�@�bN@�I�@�9X@�(�@�@
=@~��@~{@}@}?}@}�@|��@|��@|Z@{t�@z�@z��@z~�@y��@y�^@yx�@y&�@y&�@x��@x�`@x��@x�9@x�u@xA�@x  @w
=@v�+@v{@u�-@u?}@u?}@u?}@u/@t��@t�/@t��@t�j@t�@t�D@tj@tI�@s�F@rn�@qG�@p��@pbN@pb@o�P@o
=@nff@m`B@l��@l�/@l�j@l�@l��@l(�@k��@kC�@k"�@j��@j^5@i��@i��@i��@ix�@iX@iX@iG�@i7L@i7L@i�@hĜ@hA�@g�@g;d@f��@f�@f�R@fV@e�@e�h@ep�@e`B@eO�@eO�@eO�@e�@d�@dj@c�@b^5@a�@aX@`��@`��@`Q�@` �@` �@`  @_�w@_�@_l�@^�y@^��@^5?@]@]�@]�@\�@\�D@\I�@\1@[ƨ@[��@[��@[�@[t�@[S�@Z�\@Y��@Y%@XĜ@X��@X�@XQ�@X �@X  @W�;@W��@W��@W;d@W
=@V��@U�@UO�@U/@U/@U/@U�@Tz�@S�@R��@R~�@R~�@R-@Q��@Qhs@P�9@Pr�@PQ�@P1'@Pb@O�@O|�@O+@O
=@N��@N�y@N��@N�+@Nv�@NE�@N{@M�-@M�@L��@Lj@LZ@L(�@K�F@K"�@J�!@Jn�@I��@I�7@Ix�@Ihs@I%@H��@HbN@H1'@H �@Hb@G�w@F�R@F@E�@D9X@D�@C�
@C"�@B�!@Bn�@B=q@B�@A��@AX@A7L@A�@@Ĝ@@bN@@  @?�w@?�@?l�@?
=@>ȴ@>��@>v�@>5?@>$�@>$�@=�T@=�-@=�@=V@<�D@<Z@;��@;��@;S�@;o@:��@:~�@:M�@9��@9�@9�^@97L@8�u@8Q�@81'@8b@8  @8  @7��@7�@7K�@6��@6v�@6E�@6{@5@5O�@4�j@4Z@4(�@4(�@4�@41@3��@3C�@333@3o@2�@2��@2��@2��@2�!@2�!@2��@2n�@2-@1��@1��@1��@1hs@0��@0A�@/��@/��@/\)@/;d@.��@.ff@.V@.V@.V@.5?@-�T@-�@,�@,��@,�D@,9X@+��@+dZ@*�H@*~�@*J@)X@(�@(b@(b@(  @(  @'�w@';d@&��@&��@&$�@%�T@%�-@%O�@$�j@$Z@$�@#�m@#��@#t�@#33@#o@#@#@#@"�H@"�H@"�H@"�H@"��@"��@"��@"��@"��@"~�@"^5@"=q@!��@!��@!X@!%@ ��@ �`@ �9@ A�@�@�@|�@l�@\)@\)@\)@\)@\)@\)@�@
=@�R@�R@��@ff@5?@$�@�@@��@�@V@�@��@�@��@�m@�
@��@t�@33@o@�@��@~�@=q@�@�#@�^@��@��@�7@G�@7L@7L@�@�9@r�@A�@b@b@  @�@;d@��@v�@V@E�@5?@�T@�h@�@p�@`B@O�@?}@�@�@�/@��@��@�j@�@�@z�@j@(�@ƨ@@�@�@�\@�\@~�@^5@-@��@�7@G�@�@�@��@�`@��@Ĝ@��@�u@�@r�@r�@bN@bN@Q�@Q�@Q�@ �@  @��@��@�P@�P@|�@;d@ȴ@��@ff@5?@$�@�@��@��@�@?}@��@�@�/@j@�@�m@�@S�@o@
��@
��@
��@
n�@
^5@
^5@
^5@
^5@
^5@
-@
�@	�@	��@	��@	�^@	��@	��@	�7@	hs@	G�@��@Ĝ@Q�@  @�@��@�w@�@|�@l�@K�@K�@;d@+@�y@ȴ@�R@��@��@ff@5?@{@�T@�-@O�@/@�@�@�@�@V@V@V@��@��@�/@�j@��@j@Z@9X@(�@1@�m@ƨ@��@�@dZ@dZ@S�@S�@S�@S�@S�@C�@C�@33@@��@�!@��@��@��@�!111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�  A���A���A���A��A��#A��/Aȏ\A��AǴ9A���A�bNA�p�A��9A�hsA��;A���A�XA�A��7A�p�A�S�A�5?A���A�9XA��A��A�v�A���A��jA�;dA��yA��;A���A���A��A�{A��A�I�A�v�A���A���A���A���A�p�A���A��yA�E�A�ƨA�\)A��A��A�l�A��A��^A��A���A�  A��HA���A�A���A��FA�hsA�=qA���A�l�A��+A��A��A��A��mA�ĜA�z�A�/A���A�oA�bNA�S�A�VA��A�`BA��`A�\)A�9XA��A~Q�A}
=A{��AzbAx�Av  Aul�Au�AsAr�\Ao�Am�-Ak��Aj�AjI�Ai�7Ahn�AgG�AfZAe�hAd��AdJAb�HAbȴAbbAa`BA`=qA_33A]��A\�jA[��AZ�!AXQ�AVJAU�ATE�ARAPn�AOdZAMG�AL��AK�hAGƨAF�!AFz�AE�AE�AC��AC�AB��AA�FAAt�A@ĜA?�;A?`BA>�A=A:�!A8��A8ĜA8bA7S�A7
=A6ĜA6��A6��A6v�A5��A4�yA4�A4Q�A4Q�A4I�A3��A2(�A1�-A1dZA0�A0A/x�A/�A.�yA.��A.JA-ƨA-G�A,1'A+�A*$�A(�A'�A&��A%��A$ĜA$(�A#�^A#
=A"^5A!��A!7LA ��A ��A ZAx�A��A�
Ax�A(�A�`A�DA�AS�A�TA�PAS�A+A��A�+AI�A$�A�A��At�A�AA�Ar�A��A��A�+A7LA-A
��A
I�A
(�A
 �A
bA
  A	�A	�wA�HA�TA��A��AffA�A�-A��AhsAr�A=qA�A|�A �u@��@�S�@���@�&�@�bN@��F@�+@�J@���@�b@�ff@��@�E�@�ƨ@���@��@�9@�Z@��m@�ƨ@��@�S�@���@�~�@�J@�Ĝ@�@��@���@ݙ�@݉7@݉7@݁@݁@�p�@�&�@۝�@�?}@�n�@���@�O�@�/@��@�%@���@ԛ�@�1'@ӕ�@ҟ�@���@��@�7L@̬@�Q�@�b@�ƨ@ʰ!@�O�@ǶF@�@��@Ĵ9@ÍP@�@°!@�^5@���@���@�?}@� �@�@�J@�G�@�V@��`@���@�(�@��F@�33@�E�@��@��F@���@�&�@�dZ@���@��@�r�@�j@�I�@��@��@���@�-@��j@��
@���@���@���@�z�@�b@�b@��w@���@�"�@��@�%@��/@�Ĝ@���@� �@���@��#@�/@���@��9@���@�z�@�I�@�9X@��
@���@���@�\)@��@��\@�-@�hs@��@�ƨ@�^5@��#@�V@�I�@�  @���@�
=@��+@�$�@�p�@��@�K�@���@��\@�J@��7@�X@�?}@�%@��j@��9@��@���@��u@��@�Q�@���@��;@�C�@���@���@��\@��\@�~�@�ff@�{@�G�@�Q�@�\)@��@�M�@�J@���@���@��@��@�Z@�(�@�ƨ@�|�@�+@�o@�@��@��@���@�n�@�V@�E�@�5?@�J@���@���@��D@��D@��@��@��D@���@���@��D@�r�@�bN@�I�@�9X@�(�@�@
=@~��@~{@}@}?}@}�@|��@|��@|Z@{t�@z�@z��@z~�@y��@y�^@yx�@y&�@y&�@x��@x�`@x��@x�9@x�u@xA�@x  @w
=@v�+@v{@u�-@u?}@u?}@u?}@u/@t��@t�/@t��@t�j@t�@t�D@tj@tI�@s�F@rn�@qG�@p��@pbN@pb@o�P@o
=@nff@m`B@l��@l�/@l�j@l�@l��@l(�@k��@kC�@k"�@j��@j^5@i��@i��@i��@ix�@iX@iX@iG�@i7L@i7L@i�@hĜ@hA�@g�@g;d@f��@f�@f�R@fV@e�@e�h@ep�@e`B@eO�@eO�@eO�@e�@d�@dj@c�@b^5@a�@aX@`��@`��@`Q�@` �@` �@`  @_�w@_�@_l�@^�y@^��@^5?@]@]�@]�@\�@\�D@\I�@\1@[ƨ@[��@[��@[�@[t�@[S�@Z�\@Y��@Y%@XĜ@X��@X�@XQ�@X �@X  @W�;@W��@W��@W;d@W
=@V��@U�@UO�@U/@U/@U/@U�@Tz�@S�@R��@R~�@R~�@R-@Q��@Qhs@P�9@Pr�@PQ�@P1'@Pb@O�@O|�@O+@O
=@N��@N�y@N��@N�+@Nv�@NE�@N{@M�-@M�@L��@Lj@LZ@L(�@K�F@K"�@J�!@Jn�@I��@I�7@Ix�@Ihs@I%@H��@HbN@H1'@H �@Hb@G�w@F�R@F@E�@D9X@D�@C�
@C"�@B�!@Bn�@B=q@B�@A��@AX@A7L@A�@@Ĝ@@bN@@  @?�w@?�@?l�@?
=@>ȴ@>��@>v�@>5?@>$�@>$�@=�T@=�-@=�@=V@<�D@<Z@;��@;��@;S�@;o@:��@:~�@:M�@9��@9�@9�^@97L@8�u@8Q�@81'@8b@8  @8  @7��@7�@7K�@6��@6v�@6E�@6{@5@5O�@4�j@4Z@4(�@4(�@4�@41@3��@3C�@333@3o@2�@2��@2��@2��@2�!@2�!@2��@2n�@2-@1��@1��@1��@1hs@0��@0A�@/��@/��@/\)@/;d@.��@.ff@.V@.V@.V@.5?@-�T@-�@,�@,��@,�D@,9X@+��@+dZ@*�H@*~�@*J@)X@(�@(b@(b@(  @(  @'�w@';d@&��@&��@&$�@%�T@%�-@%O�@$�j@$Z@$�@#�m@#��@#t�@#33@#o@#@#@#@"�H@"�H@"�H@"�H@"��@"��@"��@"��@"��@"~�@"^5@"=q@!��@!��@!X@!%@ ��@ �`@ �9@ A�@�@�@|�@l�@\)@\)@\)@\)@\)@\)@�@
=@�R@�R@��@ff@5?@$�@�@@��@�@V@�@��@�@��@�m@�
@��@t�@33@o@�@��@~�@=q@�@�#@�^@��@��@�7@G�@7L@7L@�@�9@r�@A�@b@b@  @�@;d@��@v�@V@E�@5?@�T@�h@�@p�@`B@O�@?}@�@�@�/@��@��@�j@�@�@z�@j@(�@ƨ@@�@�@�\@�\@~�@^5@-@��@�7@G�@�@�@��@�`@��@Ĝ@��@�u@�@r�@r�@bN@bN@Q�@Q�@Q�@ �@  @��@��@�P@�P@|�@;d@ȴ@��@ff@5?@$�@�@��@��@�@?}@��@�@�/@j@�@�m@�@S�@o@
��@
��@
��@
n�@
^5@
^5@
^5@
^5@
^5@
-@
�@	�@	��@	��@	�^@	��@	��@	�7@	hs@	G�@��@Ĝ@Q�@  @�@��@�w@�@|�@l�@K�@K�@;d@+@�y@ȴ@�R@��@��@ff@5?@{@�T@�-@O�@/@�@�@�@�@V@V@V@��@��@�/@�j@��@j@Z@9X@(�@1@�m@ƨ@��@�@dZ@dZ@S�@S�@S�@S�@S�@C�@C�@33@@��@�!@��@��@��@�!111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��BɺBȴBƨBĜB�qB�?B�9B��Bu�B%�B�DBs�BjB�XB�jB�dB�XBŢB��B�XB��B�VBaHB�B1'Bp�B}�B��B�oBz�BgmBk�BK�B(�BT�B=qBF�BM�BXBO�B;dB'�BbB{B�BoB�B��B�fB��B�'B�wB�B�{B�{B��B�oBhsBYBt�BiyBhsBZBI�B'�B(�BD�BA�B:^B.B�B
�mB
�!B
ĜB
��B
��B
�\B
~�B
|�B
�+B
w�B
�B
p�B
`BB
XB
B�B
:^B
+B
 �B
+B
)�B
oB
B	�BB	�#B	�B	�NB	�mB	�5B	��B	ǮB	ŢB	ŢB	�wB	�jB	�B	�RB	��B	��B	�hB	�=B	|�B	~�B	p�B	jB	H�B	>wB	M�B	G�B	&�B	&�B	,B	bB	�B	PB�sB	B	�B	\B	B��B��B	B�B��B�B�NB�mB�BƨB��B�B��BȴBĜB��B��B��B��BǮB�jB�XB��BÖBĜB�}B�3B��B�'B�-B��B��B��B�B��B��B��B��B��B�+B�B� Bx�Bx�B|�B�By�B� B�B|�Bz�B|�By�B}�B{�Bu�BiyBjB_;BffBS�BR�B`BB]/BT�BF�B\)B_;B`BB]/BYB\)B]/BYBT�BQ�BE�B49B�B�B.B$�B$�B%�B(�B5?BB�BD�BC�BB�B>wB8RB+B �B&�B6FB33B0!B&�B�BoB�B,B&�B�B\B��B\B�B"�B#�B �B!�B�B�B�BhB
=BVBDB�B�B �B(�B)�B,B+B'�B#�B"�B�B\B��BbB�B(�B)�B+B)�B'�B$�B�BbBBDB%�B)�B.B/B-B+B(�B$�B�B�B\B{B�B)�B,B+B&�B�B�B�B$�B"�B!�B'�B/B33B1'B/B-B#�B�BPBB5?B?}B?}B<jB9XB8RB49B0!B)�B5?B5?B-B,B2-BR�BYBXBT�BO�BN�BVBO�BG�BO�BT�BXB]/BiyBk�Bn�Bl�Bl�BffBcTBiyBv�Bw�Bt�Bo�BhsBs�B~�B�B�7B�7B�1B�1B�7B�%B�7B�=B�+B�B�1B�%B�B�+B� B�B��B��B��B��B��B��B��B�B��B�B�'B��BƨBŢBȴB��B��B��B��B�B�B�B�B�B�B��B�B�B�#B�fB�fB�fB�ZB�TB�;B�)B�BB�sB��B��B��B	  B	%B	B	%B	bB	hB	hB	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	"�B	"�B	�B	#�B	33B	:^B	;dB	=qB	>wB	?}B	?}B	?}B	A�B	B�B	B�B	B�B	B�B	@�B	B�B	G�B	H�B	M�B	N�B	R�B	R�B	Q�B	S�B	S�B	\)B	`BB	`BB	aHB	e`B	gmB	hsB	m�B	m�B	n�B	n�B	n�B	n�B	m�B	n�B	m�B	t�B	v�B	y�B	{�B	�B	�B	�B	� B	�B	�B	�B	�B	�B	�B	� B	|�B	|�B	�B	�PB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�3B	�?B	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�XB	�dB	�wB	�}B	�}B	�wB	�}B	��B	ÖB	ĜB	ĜB	ĜB	ĜB	ÖB	ÖB	��B	�}B	B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�5B	�5B	�;B	�BB	�NB	�NB	�NB	�NB	�BB	�5B	�;B	�ZB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
  B
  B
  B
  B	��B
  B	��B
B
B
B
B
B
B
%B
B
1B
	7B
	7B
1B
1B

=B
DB
DB

=B
1B
B
1B
	7B
DB
hB
hB
VB
hB
{B
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
�B
 �B
$�B
%�B
&�B
&�B
&�B
%�B
%�B
$�B
%�B
%�B
'�B
'�B
&�B
&�B
&�B
)�B
,B
-B
-B
,B
+B
,B
.B
.B
.B
/B
0!B
0!B
/B
/B
/B
.B
.B
.B
/B
/B
.B
-B
,B
/B
1'B
1'B
1'B
2-B
1'B
49B
5?B
49B
33B
2-B
0!B
33B
6FB
6FB
49B
33B
6FB
49B
5?B
5?B
49B
6FB
9XB
=qB
<jB
<jB
:^B
9XB
;dB
;dB
:^B
<jB
=qB
<jB
;dB
>wB
?}B
@�B
@�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
C�B
C�B
C�B
C�B
B�B
C�B
C�B
D�B
E�B
E�B
D�B
D�B
E�B
F�B
G�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
H�B
H�B
I�B
I�B
H�B
I�B
J�B
I�B
I�B
I�B
I�B
H�B
I�B
K�B
J�B
L�B
M�B
M�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
O�B
N�B
O�B
P�B
P�B
O�B
O�B
Q�B
P�B
O�B
N�B
O�B
P�B
Q�B
Q�B
Q�B
O�B
N�B
O�B
S�B
S�B
T�B
S�B
R�B
S�B
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
VB
XB
XB
XB
XB
W
B
W
B
W
B
VB
T�B
T�B
S�B
ZB
ZB
YB
[#B
[#B
ZB
YB
YB
ZB
[#B
\)B
]/B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
^5B
^5B
]/B
^5B
^5B
_;B
_;B
`BB
_;B
]/B
]/B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
cTB
bNB
`BB
aHB
bNB
bNB
dZB
dZB
dZB
ffB
ffB
ffB
gmB
hsB
hsB
gmB
gmB
ffB
gmB
gmB
hsB
iyB
iyB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
k�B
k�B
l�B
l�B
k�B
l�B
l�B
m�B
m�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
n�B
m�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
q�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
r�B
r�B
r�B
r�B
q�B
s�B
s�B
t�B
t�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B�]B��B��B�LB{�B0UB��Bx�BoB�>B�<B�PB�DB��B��B��B�_B��BfLB%�B7�Bs�B��B��B��B}qBjeBm�BO�B-wBV�B@4BHfBN�BX�BQ B=�B+6B�B�BB�B��B��B��B�vB�TB�}B�)B�?B��B�HB��Bk�B[�BuBjeBiB[qBK^B+�B*0BD�BA�B:�B.�B+B
�B
��B
�YB
��B
�EB
��B
��B
~�B
�fB
y>B
�oB
q�B
bB
Y�B
D�B
<PB
-wB
"�B
+�B
*�B
aB
�B	�B	��B	�]B	�B	�>B	�VB	�bB	�7B	��B	��B	��B	�qB	��B	��B	�B	��B	�B	��B	~�B	�4B	raB	l"B	K�B	A B	N�B	IB	)�B	)B	-]B	&B	 �B	�B��B	MB	�B	HB	uB��B��B	�B��B�rB�B�B�>B�WBȴB��B� BуBɺBŢB�PB�VB�(B�B�1B��B�DB�'B��B��B� B�nB��B��B��B�B��B��B��B�B��B��B�VB��B��B��B��Bz�Bz�B~BB�B{B��B��B~B|B}�Bz�B~]B|jBvzBj�Bk�B`�BgBU�BT�B`�B^BVSBH�B\�B_�B`�B]�BY�B\�B]�BY�BU�BR�BF�B5�B"NB�B/�B&�B&�B'mB*B6+BB�BD�BC�BB�B>�B8�B,qB"4B(>B6�B3�B0�B'�BCB,B�B,�B'�B�B�B�cB�B�B#nB$�B!|B"hB�BqBSB�B�B�B�B�B�B!�B)_B*eB,WB+6B(>B$tB#TB~B�B��BhB \B)B*B+6B*0B($B%B]B�B�BB&LB*B.cB/OB-CB+kB)_B%`BpB�B�B�B�B*KB,qB+kB'�B�B�B�B%�B#�B"�B(�B/�B3�B1vB/�B-]B$�B�BB�B5�B?�B?�B<�B9�B8�B4�B0�B+QB6B6+B.�B-�B3�BS[BYKBXEBUgBP}BOvBVSBP�BH�BP�BU�BX�B^Bi�Bk�Bn�Bl�Bl�BgBdZBj0Bv�BxBu%BpUBi�BtnB}B�{B�RB�lB��B��B�lB��B��B��B��B��B��B��B��B��B�;B�9B�+B�YB�dB�2B�fB�zB�yB��B��B��B�B��B��B�%B�7B�B�&B�@B�,B�EB�1B�EB�+B�+B�SB�MB�eBևBۦB�B�B�B�B�B߾B��B�B�*B�%B�fB�HB	 OB	?B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	#B	$B	#B	# B	 \B	$�B	3hB	:xB	;�B	=qB	>wB	?}B	?�B	?�B	A�B	B�B	B�B	B�B	B�B	@�B	B�B	G�B	IB	NB	O(B	SB	S&B	R:B	T,B	T{B	\xB	`vB	`�B	a�B	e�B	g�B	h�B	m�B	m�B	n�B	n�B	n�B	n�B	m�B	n�B	nB	uB	wB	z*B	|6B	� B	� B	�;B	�4B	�AB	�AB	�AB	�AB	�'B	�;B	�B	}VB	}qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�&B	�2B	�*B	�6B	�QB	�]B	�cB	�[B	�hB	�hB	�tB	�`B	�zB	�`B	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	ĶB	ĶB	ĶB	��B	��B	��B	��B	�B	��B	�B	�"B	�B	�B	�B	��B	�B	�B	�2B	�B	�&B	�SB	�YB	�_B	�7B	�kB	�qB	�jB	�jB	�pB	�vB	�B	�B	�B	�B	�vB	ޞB	߾B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�+B	�B	�B	�B	�"B	�B	�6B	�"B	�.B
 B
 4B	�.B
 4B
 4B
 4B
 4B	�.B
 B	�cB
AB
3B
GB
[B
[B
gB
?B
mB
fB
	lB
	RB
�B
fB

rB
xB
xB

rB
�B
�B
�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 B
!B
%B
%�B
'B
&�B
'B
&B
&B
%,B
&2B
&B
(
B
(
B
'8B
'8B
'8B
*B
,=B
-)B
-B
,=B
+QB
,"B
./B
.IB
.IB
/OB
0;B
0;B
/OB
/B
/OB
.IB
.IB
.IB
/OB
/OB
.IB
-]B
,qB
/OB
1[B
1[B
1AB
2aB
1vB
4TB
5ZB
4TB
3hB
2|B
0�B
3hB
6`B
6zB
4�B
3�B
6zB
4�B
5tB
5tB
4�B
6�B
9�B
=�B
<�B
<�B
:�B
9�B
;�B
;�B
:�B
<�B
=�B
<�B
;�B
>�B
?�B
@�B
@�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
C�B
C�B
C�B
C�B
B�B
C�B
C�B
D�B
E�B
E�B
D�B
D�B
E�B
F�B
G�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
H�B
H�B
I�B
I�B
H�B
I�B
J�B
I�B
I�B
I�B
I�B
H�B
I�B
K�B
J�B
L�B
M�B
M�B
MB
MB
MB
NB
M�B
NB
OB
OB
PB
OB
O�B
P�B
Q B
PB
PB
Q�B
Q B
PB
OB
O�B
QB
R B
RB
RB
PB
O(B
P.B
T,B
T,B
UB
T,B
SB
T,B
VB
W$B
W$B
W$B
W$B
W?B
V9B
X+B
X+B
X+B
X+B
W$B
W$B
W?B
V9B
U2B
UMB
TaB
Z7B
Z7B
YKB
[=B
[#B
ZQB
YKB
YKB
Z7B
[WB
\]B
]IB
\]B
]/B
]IB
]/B
]dB
^OB
^OB
_VB
_VB
_;B
_VB
_VB
^OB
^5B
]IB
^jB
^jB
_VB
_;B
`\B
_pB
]dB
]dB
_pB
`vB
`vB
abB
a|B
a|B
abB
abB
a|B
abB
cnB
bhB
`vB
a|B
bhB
b�B
d�B
d�B
d�B
f�B
f�B
f�B
g�B
hsB
h�B
g�B
g�B
f�B
g�B
g�B
h�B
i�B
i�B
h�B
h�B
hsB
h�B
h�B
h�B
h�B
h�B
i�B
k�B
k�B
l�B
l�B
k�B
l�B
l�B
m�B
m�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
n�B
m�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
p�B
q�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
r�B
r�B
r�B
r�B
q�B
s�B
s�B
t�B
t�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.17(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806290038312018062900383120180629003831201806290200152018062902001520180629020015201806300025592018063000255920180630002559  JA  ARFMdecpA19c                                                                20180625063528  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180624213529  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180624213532  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180624213532  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180624213533  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180624213533  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180624213533  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180624213533  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180624213533  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180624213533                      G�O�G�O�G�O�                JA  ARUP                                                                        20180624215600                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180625153256  CV  JULD            G�O�G�O�F�g�                JM  ARCAJMQC2.0                                                                 20180628153831  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180628153831  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180628170015  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180629152559  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                