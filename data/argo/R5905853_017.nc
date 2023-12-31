CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:25:33Z creation;2022-06-04T17:25:33Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604172533  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @������?1   @�����@-BI�^5�c����l�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B���B���B�ffB�  B�  B�33B�  B���B�  B�33B�  B�  B�  B���B���B�  B�33B���B�  B�  B�  B�  B�  B���B�33B���B�  B�  B�  B�  C   C�fC  C  C  C
  C�C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.�C0  C1�fC4  C5�fC7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!fD!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dyy�Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�R@u�@��\@��\AG�A>�HA]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�BoQ�BwQ�BQ�B�u�B�B�B�B�B�\B���B���B��)B���B�u�B���B��)B���B���B���B�u�B�u�Bè�B��)B�u�BϨ�BӨ�Bר�Bۨ�Bߨ�B�u�B��)B�u�B��B��B���B���B���C��C�{C�{C�{C	�{C�C�C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�C-�C/�{C1��C3�{C5��C7��C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs��Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD ��D!{�D!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�DwuDw�DxuDx�Dyn�Dy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D��\D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�z�D⺏D���D�:�D�z�D㺏D���D�:�D�z�D亏D���D�:�D�z�D序D���D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D躏D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�m�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aݑ AݗYAݗYAݗYAݑ�Aݏ\Aݍ�Aݐ.AݍPA݌~A݌JA݇�A݂uA݁A�~�A݁A݁oA݂uA݁;A݁�A݂AA݂uA݃{A݁A݁�A݂A݁�A݀�A݂AA݃A݂uA݂�A�VmA�]dAٲ�A��)AؽA׿A�p�A�"hAԎ"A�d�A�A�~]A��AƔ�AĄMA� \A�|PA�*0A��A��HA�Q�A���A��mA���A�NA�'�A�t�A���A��A�ٴA�fA�`vA�1[A��A�[�A�+kA�\]A��A��A��fA�q�A���A�{A�d�A��TA��A�*0A��FA���A���Az�sAu�+Ar�_AnjAl��Aj�Ai1'Aey�Ad�A_�RAZ��AW��AV1�AT�AR�APD�AMC�AJ�AGc AG$AF�|AG+�AE�AB�mA@MjA=��A7��A3��A0�>A.g�A,;�A,�A+�hA)��A(ɆA(+kA'��A&�+A&�A%<�A#0UA"ZA!�.A w�A�A��A�]A��A)�A{JA4�AaA�HA��A�AZ�A��A��AqvA��A��A \A��A��A$�A`�A�+A��Aa|A�A�XA_pAA7�A��A�A~�A�A-wA�KAZ�A�KA�AY�AbA\�Aq�AhsA�|AѷA��Ae,A iA
�*A
�hA
��A
��A
G�A	͟Ap�A�"A/�A�AxAa�A�AZ�A��A��AhsA/�A�A�FAu%A֡A��AVA	A ��A �VA ZA �@��*@�c�@��@��@��@@��@�ߤ@�E�@���@���@�s@���@�6@��)@��*@�Y@��U@���@��m@��S@��@�@O@���@�@�z�@��@�@�$@�~@�n/@��@�	@�U�@���@�D@�@��Q@���@�@��@�}@��@�Q�@��@�p;@��&@��@�J@�rG@���@�$�@�g�@��@�K^@�ԕ@�(�@�L0@۝�@���@�M�@�3�@�N<@��B@ڌ@�=q@��@���@ف�@�Z�@��@�U�@ֲ�@�q�@�خ@ԧ@�G�@��@���@��U@ҳh@�}V@�V�@��@�rG@�<6@��@�N�@��@�H�@���@�9X@��@�Y@�h�@�7@˓@ʱ�@���@�'�@�Dg@��@��,@ȗ�@�J�@�5?@��@��a@�G�@�{@���@şV@�S@���@ĩ�@� �@�+@�~�@�u@�{J@��K@���@�>B@��W@���@�G@���@�F@�Ɇ@�;�@�+k@�($@�"h@��@�{@��A@��@�u%@�3�@���@�@O@�ی@���@��@�خ@�c�@���@��<@�y>@�;�@�>B@�4@��	@�1�@���@��@��Q@���@���@��@���@�4n@�J@���@�4�@�@���@��@�:�@���@�S&@�^�@�v`@�͟@�҉@���@�u�@�4@��d@��[@�p�@��@���@�Z�@��)@�l�@�8@��@���@�_p@��@��h@�GE@���@���@�p�@���@�"h@�\�@�@��8@��/@�K^@��C@�x�@�N<@�J�@�A @���@�S�@��r@�ԕ@���@�T�@�_@��@��@��@�@��@���@��{@�|�@�x�@�G�@���@���@�GE@�1'@�e@��o@��~@�(�@��B@�{�@�B[@�M@��-@�e�@���@��@�}V@�[�@�H�@�1'@��r@��@���@�v�@�Ov@�>B@�@���@��m@���@��w@��X@��@�f�@�+�@��@��/@��@��@�w�@�Q�@�8�@�@���@�j@��@���@��b@��o@�>B@��F@���@�j�@�IR@�!�@��+@��@�˒@�j@��@��}@��@�m�@�7@�ƨ@���@�~�@�+@��/@��O@�H@��@��m@���@���@��C@���@�/@��2@�Xy@�O@�	@�@��{@�;d@���@��+@�Z@�:*@� �@��}@���@�v`@�-w@��@��}@�y>@�GE@�(�@�ԕ@��F@��'@��@�^�@� \@��e@�[�@��@���@��n@�L�@�ѷ@�~�@�0U@��@���@��@��@�H@�D�@�6@���@�o @� \@�ں@�u�@�7@��m@��@��@�}�@�Dg@�;@���@�ی@���@�.�@�@�	@+@~��@~d�@}��@}:�@|��@|�v@|��@|D�@{�@{\)@{�@z�\@y�@yA @x��@x�@w��@w��@w_p@w6z@w�@w i@v^5@u�d@urG@t�z@s��@s@ri�@q�^@q-w@q�@pĜ@p:�@o�k@oa@o8@n�8@n��@n~�@m�o@m�h@mS&@m�@m%@l��@l��@lbN@l�@k�@k��@k�[@k6z@j�<@j��@jz@jTa@i�9@i�C@i:�@h��@g��@g�@f��@fO@eJ�@d�j@d`�@d�@cy�@b�@bs�@au�@`�p@_�@_qv@_�@^u%@]�@]�@\Ɇ@\Xy@[�k@[&@Z�s@Z��@Zxl@Z6�@Y�H@Y��@Yp�@Yhs@Y\�@Y*0@X�@X|�@X:�@W�0@Wo�@WRT@W4�@V�}@V{@U�@U��@U��@US&@T�@S��@S�@R�@Q�N@P�@P�@P<�@P$@P7@O��@O��@On/@O\)@O"�@O�@N�2@N�x@N-@M�Z@M�H@M�n@M�7@M`B@ML�@MB�@M0�@M!�@M	l@L�@L4n@K˒@KF�@J��@J��@JR�@I��@I`B@I<6@I�@H�E@H~@GRT@G�@G i@F��@F�}@F4@E��@E�X@Ec�@EDg@E \@D��@D�_@D@C�K@C��@C>�@B_@Am]@A+@@�	@@�f@@�f@@�9@@|�@@Z@@M@@M@@7�@@@?��@?��@?4�@>�@>�}@>h
@=�D@=�H@=c@=<6@<��@<K^@;�@;خ@;�@@;��@;F�@:�@:��@9�@9w2@9@@8Ĝ@8tT@7�]@7ƨ@7��@7K�@7�@6��@6��@6i�@65?@6O@6_@5��@5hs@5?}@4�`@4��@4r�@41@3�Q@3�@3��@2��@2v�@2\�@2@�@1�D@1��@1p�@1<6@1%@0Ĝ@0l"@/��@/��@/x@/�@.�M@.��@..�@-�3@-�h@-u�@-A @-+@,֡@,��@,g8@,M@+��@+�V@+�@*ȴ@*�@*Ta@)��@)�=@)S&@(�E@(��@(w�@(m�@(D�@'�]@'�r@'�@'�k@'{J@'=@'
=@&�B@&��@&a|@&0U@%�>@%��@%S&@%@$��@$�@$l"@$S�@$>B@$,=@#��@#��@#l�@#X�@#+@"��@"q�@"	@!�.@!rG@!IR@!0�@!�@ �K@ �@ ��@ N�@ :�@�
@U�@�@͟@l�@B[@�@��@��@��@p�@?}@��@�e@��@_@9X@�A@�K@�F@�q@�@y�@iD@]�@�@z@-@@�@V@0U@_@�D@c@�@��@�$@�@�@�@�[@�@�y@�y@��@ں@ȴ@��@H�@.�@)�@�@�@�@hs@Q�@&�@�	@ѷ@��@g8@7�@ �@G@�@�q@_p@6z@$t@�@��@i�@�@�@�d@��@��@o @\�@O�@/@!�@+@�@�@��@Xy@C-@ �@@�@�4@/�@�@��@}V@W�@)�@�@��@�t@��@m]@<6@��@Ɇ@y>@<�@x@ݘ@��@\)@>�@
��@
�B@
�!@
{�@
5?@
@	��@	�=@	G�@	@@	%@�f@�p@��@c�@Z@K^@K^@I�@7�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aݑ AݗYAݗYAݗYAݑ�Aݏ\Aݍ�Aݐ.AݍPA݌~A݌JA݇�A݂uA݁A�~�A݁A݁oA݂uA݁;A݁�A݂AA݂uA݃{A݁A݁�A݂A݁�A݀�A݂AA݃A݂uA݂�A�VmA�]dAٲ�A��)AؽA׿A�p�A�"hAԎ"A�d�A�A�~]A��AƔ�AĄMA� \A�|PA�*0A��A��HA�Q�A���A��mA���A�NA�'�A�t�A���A��A�ٴA�fA�`vA�1[A��A�[�A�+kA�\]A��A��A��fA�q�A���A�{A�d�A��TA��A�*0A��FA���A���Az�sAu�+Ar�_AnjAl��Aj�Ai1'Aey�Ad�A_�RAZ��AW��AV1�AT�AR�APD�AMC�AJ�AGc AG$AF�|AG+�AE�AB�mA@MjA=��A7��A3��A0�>A.g�A,;�A,�A+�hA)��A(ɆA(+kA'��A&�+A&�A%<�A#0UA"ZA!�.A w�A�A��A�]A��A)�A{JA4�AaA�HA��A�AZ�A��A��AqvA��A��A \A��A��A$�A`�A�+A��Aa|A�A�XA_pAA7�A��A�A~�A�A-wA�KAZ�A�KA�AY�AbA\�Aq�AhsA�|AѷA��Ae,A iA
�*A
�hA
��A
��A
G�A	͟Ap�A�"A/�A�AxAa�A�AZ�A��A��AhsA/�A�A�FAu%A֡A��AVA	A ��A �VA ZA �@��*@�c�@��@��@��@@��@�ߤ@�E�@���@���@�s@���@�6@��)@��*@�Y@��U@���@��m@��S@��@�@O@���@�@�z�@��@�@�$@�~@�n/@��@�	@�U�@���@�D@�@��Q@���@�@��@�}@��@�Q�@��@�p;@��&@��@�J@�rG@���@�$�@�g�@��@�K^@�ԕ@�(�@�L0@۝�@���@�M�@�3�@�N<@��B@ڌ@�=q@��@���@ف�@�Z�@��@�U�@ֲ�@�q�@�خ@ԧ@�G�@��@���@��U@ҳh@�}V@�V�@��@�rG@�<6@��@�N�@��@�H�@���@�9X@��@�Y@�h�@�7@˓@ʱ�@���@�'�@�Dg@��@��,@ȗ�@�J�@�5?@��@��a@�G�@�{@���@şV@�S@���@ĩ�@� �@�+@�~�@�u@�{J@��K@���@�>B@��W@���@�G@���@�F@�Ɇ@�;�@�+k@�($@�"h@��@�{@��A@��@�u%@�3�@���@�@O@�ی@���@��@�خ@�c�@���@��<@�y>@�;�@�>B@�4@��	@�1�@���@��@��Q@���@���@��@���@�4n@�J@���@�4�@�@���@��@�:�@���@�S&@�^�@�v`@�͟@�҉@���@�u�@�4@��d@��[@�p�@��@���@�Z�@��)@�l�@�8@��@���@�_p@��@��h@�GE@���@���@�p�@���@�"h@�\�@�@��8@��/@�K^@��C@�x�@�N<@�J�@�A @���@�S�@��r@�ԕ@���@�T�@�_@��@��@��@�@��@���@��{@�|�@�x�@�G�@���@���@�GE@�1'@�e@��o@��~@�(�@��B@�{�@�B[@�M@��-@�e�@���@��@�}V@�[�@�H�@�1'@��r@��@���@�v�@�Ov@�>B@�@���@��m@���@��w@��X@��@�f�@�+�@��@��/@��@��@�w�@�Q�@�8�@�@���@�j@��@���@��b@��o@�>B@��F@���@�j�@�IR@�!�@��+@��@�˒@�j@��@��}@��@�m�@�7@�ƨ@���@�~�@�+@��/@��O@�H@��@��m@���@���@��C@���@�/@��2@�Xy@�O@�	@�@��{@�;d@���@��+@�Z@�:*@� �@��}@���@�v`@�-w@��@��}@�y>@�GE@�(�@�ԕ@��F@��'@��@�^�@� \@��e@�[�@��@���@��n@�L�@�ѷ@�~�@�0U@��@���@��@��@�H@�D�@�6@���@�o @� \@�ں@�u�@�7@��m@��@��@�}�@�Dg@�;@���@�ی@���@�.�@�@�	@+@~��@~d�@}��@}:�@|��@|�v@|��@|D�@{�@{\)@{�@z�\@y�@yA @x��@x�@w��@w��@w_p@w6z@w�@w i@v^5@u�d@urG@t�z@s��@s@ri�@q�^@q-w@q�@pĜ@p:�@o�k@oa@o8@n�8@n��@n~�@m�o@m�h@mS&@m�@m%@l��@l��@lbN@l�@k�@k��@k�[@k6z@j�<@j��@jz@jTa@i�9@i�C@i:�@h��@g��@g�@f��@fO@eJ�@d�j@d`�@d�@cy�@b�@bs�@au�@`�p@_�@_qv@_�@^u%@]�@]�@\Ɇ@\Xy@[�k@[&@Z�s@Z��@Zxl@Z6�@Y�H@Y��@Yp�@Yhs@Y\�@Y*0@X�@X|�@X:�@W�0@Wo�@WRT@W4�@V�}@V{@U�@U��@U��@US&@T�@S��@S�@R�@Q�N@P�@P�@P<�@P$@P7@O��@O��@On/@O\)@O"�@O�@N�2@N�x@N-@M�Z@M�H@M�n@M�7@M`B@ML�@MB�@M0�@M!�@M	l@L�@L4n@K˒@KF�@J��@J��@JR�@I��@I`B@I<6@I�@H�E@H~@GRT@G�@G i@F��@F�}@F4@E��@E�X@Ec�@EDg@E \@D��@D�_@D@C�K@C��@C>�@B_@Am]@A+@@�	@@�f@@�f@@�9@@|�@@Z@@M@@M@@7�@@@?��@?��@?4�@>�@>�}@>h
@=�D@=�H@=c@=<6@<��@<K^@;�@;خ@;�@@;��@;F�@:�@:��@9�@9w2@9@@8Ĝ@8tT@7�]@7ƨ@7��@7K�@7�@6��@6��@6i�@65?@6O@6_@5��@5hs@5?}@4�`@4��@4r�@41@3�Q@3�@3��@2��@2v�@2\�@2@�@1�D@1��@1p�@1<6@1%@0Ĝ@0l"@/��@/��@/x@/�@.�M@.��@..�@-�3@-�h@-u�@-A @-+@,֡@,��@,g8@,M@+��@+�V@+�@*ȴ@*�@*Ta@)��@)�=@)S&@(�E@(��@(w�@(m�@(D�@'�]@'�r@'�@'�k@'{J@'=@'
=@&�B@&��@&a|@&0U@%�>@%��@%S&@%@$��@$�@$l"@$S�@$>B@$,=@#��@#��@#l�@#X�@#+@"��@"q�@"	@!�.@!rG@!IR@!0�@!�@ �K@ �@ ��@ N�@ :�@�
@U�@�@͟@l�@B[@�@��@��@��@p�@?}@��@�e@��@_@9X@�A@�K@�F@�q@�@y�@iD@]�@�@z@-@@�@V@0U@_@�D@c@�@��@�$@�@�@�@�[@�@�y@�y@��@ں@ȴ@��@H�@.�@)�@�@�@�@hs@Q�@&�@�	@ѷ@��@g8@7�@ �@G@�@�q@_p@6z@$t@�@��@i�@�@�@�d@��@��@o @\�@O�@/@!�@+@�@�@��@Xy@C-@ �@@�@�4@/�@�@��@}V@W�@)�@�@��@�t@��@m]@<6@��@Ɇ@y>@<�@x@ݘ@��@\)@>�@
��@
�B@
�!@
{�@
5?@
@	��@	�=@	G�@	@@	%@�f@�p@��@c�@Z@K^@K^@I�@7�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B�jB��B��B��B�jB��B��B��B��B�!B�!B�;B�vB�BB��B�HB��B�4B�4B�B� B��B	B	(�B	?cB	a�B	R�B	:xB	=qB	]IB	LB	xB	BB	�B	# B	9�B	^�B	�B	��B	�iB	�]B
C�B
T{B
y�B
��B
�0B
��B
�Ba�Br�BU2BEBUMBlqB�[B��B��B��B��B�B~BiBK�B[B
�#B
�B
IB
	�B	�+B	�8B	�fB	ɠB	��B	��B	~(B	n�B	bhB	V�B	=�B	2|B	�B	�B	�B�lB��B�FBۦBЗB�PB̈́B�QB�*B	�B	&LB	%FB	"�B	�B��B�/B�pB�B�B�fBƨB��B�JBѷB�B��B�B�B�B� B��B�8B��BߊBϫB�B̳B޸B�UB�B	"B	SB	 B	0�B	C�B	JrB	P}B	YB	u�B	��B	��B	��B	�$B	��B	�B	�B	��B	�xB	��B	�B	�+B	��B	�B	��B	��B	�PB	�iB	�oB	�;B	�}B	��B	��B	�VB	�dB	��B	��B	�B	�RB	��B	�B	��B	�GB	�+B	�0B	��B	�sB	׍B	��B	ҽB	��B	�hB	ϑB	ΥB	͟B	�B	ΊB	�(B	�pB	�6B	�dB	��B	�0B	�0B	�B	�jB	��B	��B	ɠB	�lB	�B	�B	ȴB	ȴB	�KB	��B	ȚB	�B	�^B	�DB	�^B	�^B	��B	ΊB	��B	��B	�uB	ԯB	�_B	յB	�B	��B	چB	��B	��B	��B	�FB	�FB	өB	�[B	�&B	҉B	҉B	�B	��B	ҽB	�{B	�kB	�~B	��B	��B	�B	��B	��B	��B	�$B	֡B	՛B	�FB	�$B	׍B	�
B	�9B	ևB	֡B	�?B	��B	֡B	�mB	�B	ބB	�B	�B	�\B	�\B	�B	�LB	�B	�B	�>B	�LB	�B	�B	�B	�B	��B	�IB	��B	�'B	�B	��B	�zB	�B	�B	�0B	�kB	�B	�B	��B	�B	�B	��B	�B	�B	��B	�"B	�6B	�B	�=B	�"B	�B	��B	��B	�B	�}B	�B	�B	�UB	�-B	��B	�TB	��B	�TB	�B	��B	��B	�B	�hB	��B	�B	��B	��B	��B	�LB	�?B	�B	�B	�MB	�MB	��B	�ZB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�lB	��B	��B	��B	��B	�B	�$B	�>B	��B	�rB	�xB	�dB	��B	�B	��B	�	B	��B	��B	�B	��B	�jB	�VB	��B	��B	�"B	��B	�"B	�<B	�B	�(B	�HB
-B
�B
�B
�B
mB
B
�B
B
�B
zB
�B
YB
�B
�B
%B
B
B
�B
�B
�B
B
�B
�B
�B
�B
�B
}B
�B
�B
\B
\B
"B
�B
B
�B
�B
�B
�B
�B
�B
B
B
�B
@B
�B
�B
�B
�B
?B
?B
?B
$B
$B
�B
�B
�B

B
$B
sB
�B
�B
�B
B
EB
EB
�B
B
�B
B
�B
B
B
�B
�B
kB
�B
B
B
�B
B
7B
kB
�B
�B
�B
�B
�B
�B
#B
#B
WB
#B
#B
WB
#B
=B
�B
�B
xB
xB
xB
]B
�B
~B
�B
�B
�B
�B
�B
B
;B
 'B
 BB
 �B
 �B
 �B
!-B
!bB
!bB
!�B
"hB
"4B
"hB
"�B
#B
#nB
$ZB
%`B
&�B
($B
'8B
'�B
)_B
(�B
(�B
)DB
)�B
*0B
+6B
+B
+6B
+�B
,B
,qB
,�B
,WB
,qB
,�B
-)B
-wB
-�B
-wB
-�B
.B
.B
./B
.IB
.cB
.�B
/B
/�B
/�B
/�B
0B
0UB
0�B
0�B
1B
1AB
1�B
2|B
2�B
2�B
2�B
3B
3�B
3�B
4B
4�B
5B
5?B
5?B
5ZB
5tB
5�B
6B
6B
6B
6+B
6�B
6�B
7B
72B
7LB
7�B
8B
8RB
7�B
7�B
8�B
8�B
9	B
9	B
9	B
9rB
9XB
9�B
9rB
9�B
:DB
:*B
:DB
:�B
:�B
:�B
:�B
;B
:�B
;dB
;�B
;�B
;�B
<B
<�B
<�B
<�B
<�B
="B
="B
=<B
=<B
=VB
=VB
=�B
=�B
>BB
>]B
>(B
>]B
>�B
>�B
>�B
>�B
>�B
?.B
?�B
?�B
?�B
?�B
?�B
@OB
@4B
AB
A B
AoB
A�B
A�B
B�B
B[B
B�B
B�B
B�B
C{B
C�B
DgB
E�B
ESB
FYB
FtB
FYB
F�B
F�B
G+B
GzB
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I7B
IlB
I7B
IlB
I7B
I�B
I�B
J	B
I�B
JrB
JrB
J�B
J=B
J�B
KB
KDB
K^B
K)B
K^B
K�B
MB
L�B
M�B
NVB
O(B
OBB
OvB
OvB
OvB
O�B
O�B
O�B
PB
P.B
PHB
PHB
PbB
P�B
P�B
QB
QB
Q4B
QNB
QNB
Q�B
QNB
Q�B
QNB
Q�B
Q�B
RTB
R�B
R�B
R�B
SB
S�B
S�B
S�B
S�B
S�B
T�B
T�B
UMB
T�B
UB
UB
U�B
U�B
VB
VB
VmB
V�B
V9B
V�B
W
B
W$B
V�B
W?B
XyB
X�B
X�B
Y1B
X�B
X�B
YKB
Y1B
YeB
Y�B
YKB
Y�B
YeB
Y�B
Y�B
ZB
ZQB
Z7B
Z�B
Z�B
Z�B
[	B
[=B
[�B
[�B
\]B
\)B
\]B
\xB
\xB
\�B
\�B
]IB
]~B
^B
^B
^�B
^�B
^�B
^�B
_!B
_B
_;B
_�B
_pB
_�B
_�B
_�B
_�B
`'B
`'B
`vB
`BB
`�B
`�B
`�B
`�B
a-B
a�B
bB
a�B
bB
b4B
bNB
b4B
a�B
bNB
bhB
b�B
c:B
cnB
c�B
c�B
c�B
d&B
d�B
d�B
eB
eFB
ezB
e�B
e�B
e�B
f�B
f�B
f�B
gRB
g�B
g�B
h
B
h>B
h�B
h�B
h�B
i_B
i�B
i�B
i�B
jKB
j�B
j�B
j�B
j�B
j�B
j�B
kB
kB
k�B
k�B
lB
l�B
l�B
m)B
mB
mwB
m]B
mwB
m�B
m�B
m�B
m�B
m�B
n/B
n/B
n}B
o B
o5B
o�B
o�B
pB
p!B
p!B
p!B
pUB
p;B
p�B
p�B
p�B
q'B
qvB
q�B
q�B
raB
r|B
r�B
r�B
r�B
r�B
sB
s3B
s�B
s�B
tB
tTB
t�B
u�B
u�B
u�B
u�B
u�B
vB
vFB
v�B
w�B
w�B
w�B
wB
v�B
xB
xRB
x8B
xB
xB
w�B
x�B
yXB
y�B
yrB
y>B
z�B
zB
zB
zB
z*B
zB
zDB
z�B
{dB
{�B
{�B
{�B
{�B
|PB
|�B
|�B
|�B
}<B
}<B
}qB
}�B
}�B
}�B
}�B
~B
}�B
~BB
~]B
~]B
~]B
~�B
~�B
~�B
B
.B
.B
HB
.B
.B
~�B
.B
.B
HB
cB
�B
�B
� B
�B
�B
�B
�B
�iB
�iB
��B
��B
�UB
� B
�;B
�;B
�;B
��B
��B
��B
��B
�B
�B
�B
�[B
�uB
��B
��B
��B
�B
�{B
��B
��B
��B
�3B
�3B
�gB
��B
�B
�SB
�SB
��B
�mB
��B
�B
��B
�%B
�B
�%B
�?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B�jB��B��B��B�jB��B��B��B��B�!B�!B�;B�vB�BB��B�HB��B�4B�4B�B� B��B	B	(�B	?cB	a�B	R�B	:xB	=qB	]IB	LB	xB	BB	�B	# B	9�B	^�B	�B	��B	�iB	�]B
C�B
T{B
y�B
��B
�0B
��B
�Ba�Br�BU2BEBUMBlqB�[B��B��B��B��B�B~BiBK�B[B
�#B
�B
IB
	�B	�+B	�8B	�fB	ɠB	��B	��B	~(B	n�B	bhB	V�B	=�B	2|B	�B	�B	�B�lB��B�FBۦBЗB�PB̈́B�QB�*B	�B	&LB	%FB	"�B	�B��B�/B�pB�B�B�fBƨB��B�JBѷB�B��B�B�B�B� B��B�8B��BߊBϫB�B̳B޸B�UB�B	"B	SB	 B	0�B	C�B	JrB	P}B	YB	u�B	��B	��B	��B	�$B	��B	�B	�B	��B	�xB	��B	�B	�+B	��B	�B	��B	��B	�PB	�iB	�oB	�;B	�}B	��B	��B	�VB	�dB	��B	��B	�B	�RB	��B	�B	��B	�GB	�+B	�0B	��B	�sB	׍B	��B	ҽB	��B	�hB	ϑB	ΥB	͟B	�B	ΊB	�(B	�pB	�6B	�dB	��B	�0B	�0B	�B	�jB	��B	��B	ɠB	�lB	�B	�B	ȴB	ȴB	�KB	��B	ȚB	�B	�^B	�DB	�^B	�^B	��B	ΊB	��B	��B	�uB	ԯB	�_B	յB	�B	��B	چB	��B	��B	��B	�FB	�FB	өB	�[B	�&B	҉B	҉B	�B	��B	ҽB	�{B	�kB	�~B	��B	��B	�B	��B	��B	��B	�$B	֡B	՛B	�FB	�$B	׍B	�
B	�9B	ևB	֡B	�?B	��B	֡B	�mB	�B	ބB	�B	�B	�\B	�\B	�B	�LB	�B	�B	�>B	�LB	�B	�B	�B	�B	��B	�IB	��B	�'B	�B	��B	�zB	�B	�B	�0B	�kB	�B	�B	��B	�B	�B	��B	�B	�B	��B	�"B	�6B	�B	�=B	�"B	�B	��B	��B	�B	�}B	�B	�B	�UB	�-B	��B	�TB	��B	�TB	�B	��B	��B	�B	�hB	��B	�B	��B	��B	��B	�LB	�?B	�B	�B	�MB	�MB	��B	�ZB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�lB	��B	��B	��B	��B	�B	�$B	�>B	��B	�rB	�xB	�dB	��B	�B	��B	�	B	��B	��B	�B	��B	�jB	�VB	��B	��B	�"B	��B	�"B	�<B	�B	�(B	�HB
-B
�B
�B
�B
mB
B
�B
B
�B
zB
�B
YB
�B
�B
%B
B
B
�B
�B
�B
B
�B
�B
�B
�B
�B
}B
�B
�B
\B
\B
"B
�B
B
�B
�B
�B
�B
�B
�B
B
B
�B
@B
�B
�B
�B
�B
?B
?B
?B
$B
$B
�B
�B
�B

B
$B
sB
�B
�B
�B
B
EB
EB
�B
B
�B
B
�B
B
B
�B
�B
kB
�B
B
B
�B
B
7B
kB
�B
�B
�B
�B
�B
�B
#B
#B
WB
#B
#B
WB
#B
=B
�B
�B
xB
xB
xB
]B
�B
~B
�B
�B
�B
�B
�B
B
;B
 'B
 BB
 �B
 �B
 �B
!-B
!bB
!bB
!�B
"hB
"4B
"hB
"�B
#B
#nB
$ZB
%`B
&�B
($B
'8B
'�B
)_B
(�B
(�B
)DB
)�B
*0B
+6B
+B
+6B
+�B
,B
,qB
,�B
,WB
,qB
,�B
-)B
-wB
-�B
-wB
-�B
.B
.B
./B
.IB
.cB
.�B
/B
/�B
/�B
/�B
0B
0UB
0�B
0�B
1B
1AB
1�B
2|B
2�B
2�B
2�B
3B
3�B
3�B
4B
4�B
5B
5?B
5?B
5ZB
5tB
5�B
6B
6B
6B
6+B
6�B
6�B
7B
72B
7LB
7�B
8B
8RB
7�B
7�B
8�B
8�B
9	B
9	B
9	B
9rB
9XB
9�B
9rB
9�B
:DB
:*B
:DB
:�B
:�B
:�B
:�B
;B
:�B
;dB
;�B
;�B
;�B
<B
<�B
<�B
<�B
<�B
="B
="B
=<B
=<B
=VB
=VB
=�B
=�B
>BB
>]B
>(B
>]B
>�B
>�B
>�B
>�B
>�B
?.B
?�B
?�B
?�B
?�B
?�B
@OB
@4B
AB
A B
AoB
A�B
A�B
B�B
B[B
B�B
B�B
B�B
C{B
C�B
DgB
E�B
ESB
FYB
FtB
FYB
F�B
F�B
G+B
GzB
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I7B
IlB
I7B
IlB
I7B
I�B
I�B
J	B
I�B
JrB
JrB
J�B
J=B
J�B
KB
KDB
K^B
K)B
K^B
K�B
MB
L�B
M�B
NVB
O(B
OBB
OvB
OvB
OvB
O�B
O�B
O�B
PB
P.B
PHB
PHB
PbB
P�B
P�B
QB
QB
Q4B
QNB
QNB
Q�B
QNB
Q�B
QNB
Q�B
Q�B
RTB
R�B
R�B
R�B
SB
S�B
S�B
S�B
S�B
S�B
T�B
T�B
UMB
T�B
UB
UB
U�B
U�B
VB
VB
VmB
V�B
V9B
V�B
W
B
W$B
V�B
W?B
XyB
X�B
X�B
Y1B
X�B
X�B
YKB
Y1B
YeB
Y�B
YKB
Y�B
YeB
Y�B
Y�B
ZB
ZQB
Z7B
Z�B
Z�B
Z�B
[	B
[=B
[�B
[�B
\]B
\)B
\]B
\xB
\xB
\�B
\�B
]IB
]~B
^B
^B
^�B
^�B
^�B
^�B
_!B
_B
_;B
_�B
_pB
_�B
_�B
_�B
_�B
`'B
`'B
`vB
`BB
`�B
`�B
`�B
`�B
a-B
a�B
bB
a�B
bB
b4B
bNB
b4B
a�B
bNB
bhB
b�B
c:B
cnB
c�B
c�B
c�B
d&B
d�B
d�B
eB
eFB
ezB
e�B
e�B
e�B
f�B
f�B
f�B
gRB
g�B
g�B
h
B
h>B
h�B
h�B
h�B
i_B
i�B
i�B
i�B
jKB
j�B
j�B
j�B
j�B
j�B
j�B
kB
kB
k�B
k�B
lB
l�B
l�B
m)B
mB
mwB
m]B
mwB
m�B
m�B
m�B
m�B
m�B
n/B
n/B
n}B
o B
o5B
o�B
o�B
pB
p!B
p!B
p!B
pUB
p;B
p�B
p�B
p�B
q'B
qvB
q�B
q�B
raB
r|B
r�B
r�B
r�B
r�B
sB
s3B
s�B
s�B
tB
tTB
t�B
u�B
u�B
u�B
u�B
u�B
vB
vFB
v�B
w�B
w�B
w�B
wB
v�B
xB
xRB
x8B
xB
xB
w�B
x�B
yXB
y�B
yrB
y>B
z�B
zB
zB
zB
z*B
zB
zDB
z�B
{dB
{�B
{�B
{�B
{�B
|PB
|�B
|�B
|�B
}<B
}<B
}qB
}�B
}�B
}�B
}�B
~B
}�B
~BB
~]B
~]B
~]B
~�B
~�B
~�B
B
.B
.B
HB
.B
.B
~�B
.B
.B
HB
cB
�B
�B
� B
�B
�B
�B
�B
�iB
�iB
��B
��B
�UB
� B
�;B
�;B
�;B
��B
��B
��B
��B
�B
�B
�B
�[B
�uB
��B
��B
��B
�B
�{B
��B
��B
��B
�3B
�3B
�gB
��B
�B
�SB
�SB
��B
�mB
��B
�B
��B
�%B
�B
�%B
�?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104849  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172533  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172533  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172533                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022541  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022541  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                