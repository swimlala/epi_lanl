CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-10-15T03:40:59Z creation;2022-10-15T03:41:00Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20221015034059  20221015035843  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��;L���1   @��;��j@._;dZ��c�?|�h1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBF��BO��BW��B`  Bh  Bp  Bx  B�  B�ffB�  B�ffB���B���B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�33B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDu  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�C3Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�3D�C3D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @.�R@u�@��\@��\AG�A=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?�RBF�BN�BV�B_Q�BgQ�BoQ�BwQ�BQ�B�\B���B�\B�u�B�u�B���B���B���B���B���B��)B�u�B�B�B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�B�u�B�u�B�u�B��B��B��)B��)B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�C-�C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�Dn�D�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$��D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�Dt{�Dt�DuuDu�DvuDv�DwuDw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�=�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�=�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�z�D⺏D���D�:�D�z�D㺏D���D�:�D�z�D亏D���D�:�D�z�D序D��\D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D躏D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�d)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AܕA�S&A�,=A��AA���AۖSAۃ�A��A�{JA�t�A�r�A�poA�k�A�iDA�Z�A�9�A���A�49A��GA��
A��|A��9A٬A�|PA�!�A�<6A���Aն�A�^5A�N<A�EA���Aͣ:A�4�A��WA���Aȷ�AƄ�A�=Aċ�A��A��2A��A�s�A��0A���A�z�A�B�A�NpA���A���A�� A���A�QA��lA���A��]A��A�F�A��A�j�A�aA�'�A�d�A�P�A���A��YA��AA�OBA�7�A���A���A��A���A��A�A���A�A�9XA�*�A�!A�j�A���A��!A���A��A~˒A}�A}+A|�~AzAw�As�Ap@Anl�Aj�+Ag��Ad��Aa�?A^~(A[6�AY7LAW��AVbNAO~�AM�AMVAL�AL�AJ�[AH��AGAEjAAD�A=^5A;4nA::�A8s�A7��A6�A3��A2�oA1�ZA0ѷA/�A,ZA+��A+�mA*�5A*1'A)��A(xA&�A%ݘA$��A#�cA"IRA!&�A =qAx�A˒A��A�DA��A�Ac A�"AA��A�yA~�A�2A�A�cA�A��A�"A��AcA&�A�rAAJ�A��A�_A	lA��A��A-A��A�Ag8A�A�NArGA�|A�6AaA�jAo�A{JA
�ZA
RTA	�7A		�AںA		lA�A	
=A	\�A��A�$A��A��A��A��Ag8A�A\�A˒A�A;dA�!AjA �)A (�@���@�w2@�!-@�0�@�j@�1�@��@� �@���@�X@��9@��@�v`@�6�@�$�@�$�@��:@�Y�@�%F@��@�kQ@�^5@�:*@��@�V@���@�P@��	@�q�@��@�J#@���@�  @�͟@�P@�@�Dg@��@�A�@�+@�8�@��@�<�@鐗@�r�@��W@�I@�c�@�9X@�F@�kQ@㢜@�P�@��@�?@�c�@�_�@��@��]@ޅ�@��@ܭ�@ܕ@�i�@�kQ@�h
@�>B@���@�x�@�A @�ں@�l"@�,=@ٴ�@�J#@��s@�O@�#�@�*�@ծ�@�o@Ա�@�S�@��)@�]�@�~�@ќ�@���@ДF@�Xy@�6@�@�6z@ίO@�^5@�1'@���@͌~@�X�@�@@��p@�w�@���@��	@�S�@�"h@��@�rG@��@Ȼ�@�w�@�>B@Ǳ[@�'�@ƅ�@�($@ŨX@ľ�@Č�@�bN@�;�@ð�@���@§�@�}V@�Ov@���@�\�@�9�@�!�@��@�w�@���@��@�4n@��@���@�Ĝ@�w�@��@�T�@��U@��&@�8�@�֡@��@�RT@��@��R@��x@��A@�N�@���@���@�8�@��@���@���@���@��@���@��+@�,=@�ƨ@��@��f@���@���@�q@�;�@�_@�Vm@���@���@�P�@�#�@�@@�
=@��@��@�7@��@���@��{@�U�@�IR@��@���@�>B@��)@��@@�m]@��B@�oi@�:�@�($@��@��[@���@�c@�X@�H�@���@�J�@���@�e,@��@�ߤ@���@�A�@��@��j@��@@�=�@��,@���@���@�Q@���@��S@�qv@�/�@�@@��j@�Ft@�O@��@��f@�S�@���@��[@���@�xl@��@��H@�t�@��@�bN@�6@�M@���@��@��_@�C-@�$�@�4@�@��
@���@��C@��M@�8�@���@���@�ϫ@�A @��f@��[@��!@���@�h
@�Ft@�$�@�@��-@�x�@���@��@���@�c @�"h@���@��w@�l�@�`B@�!�@���@���@���@��.@���@�K�@�G�@�=@��"@�N�@�  @��3@��n@�=�@���@��\@�{�@�kQ@�[�@�Z@���@��@�_�@��@��F@���@�RT@��@��p@��e@�^5@� �@��@��Q@���@�Dg@��!@�e�@�K^@�@�@�E�@��@��@���@��X@���@���@�J�@�&@��	@�ȴ@���@�kQ@�M@���@���@�ݘ@��C@���@�;d@��@���@� �@��d@�~�@�G�@��@��'@���@�z�@�@�@��@��"@��~@���@�$t@���@�kQ@�;�@�@�ϫ@��@�{J@�S&@�@���@�M@�@��@��*@�w2@�A @� \@��@��P@��@��@���@�l�@�1�@��@�
@Y@~��@~�@}��@}e,@}�@|��@|�@|��@{�W@{S�@z�r@y�T@ym]@xĜ@xbN@x%�@w�F@wRT@v�@v�h@vOv@v4@u��@t�e@t(�@s�P@s�@r҉@r~�@rh
@r=q@rO@q�Z@q�~@q�@q�@p�@o��@o��@o�m@oݘ@o�	@oe�@oP�@o4�@n�c@nv�@n-@m��@m�@l�)@l�v@l��@lh�@l!@l�@l�@k��@k��@kl�@j�b@jC�@i�@i�N@if�@h�@g i@f��@f�x@f� @f+k@eX@eF@e^�@e�@d��@ce�@a��@a \@`�)@`�z@_��@_��@_iD@_Y@^��@^ȴ@^n�@^8�@]�#@]f�@\��@[�@[�@Za|@YO�@X��@XtT@X|�@XXy@W��@W�@V�"@VZ�@UG�@TM@Tm�@T9X@S�@Se�@R�h@RE�@Q�@Qm]@QrG@QT�@Q�@Pr�@PPH@O��@O�f@N�'@M��@L��@L��@L�@L�U@MO�@M \@L�5@L��@L��@LFt@Kݘ@K�V@K33@K�@J��@J��@J4@I��@Ic@I`B@IDg@H��@H9X@G��@Ge�@G>�@G�@F�1@FJ@E�-@E�@EN<@D�@D�?@D�.@DN�@C��@C��@C��@C=@CS@B�B@BZ�@A�@A}�@A^�@A&�@@�	@@Ɇ@@�9@@��@?�@?��@?��@>�M@>&�@=��@=<6@=�@<֡@<�D@<[�@<V�@<q@<%�@;�W@;�@;W?@;9�@:��@:��@:��@:ں@:z@9�.@9Vm@8��@8�@8�@8C-@7�;@7��@7x@7A�@6�c@6xl@6$�@5�)@5��@5w2@5+@4�u@4D�@4 �@3��@3l�@3@2�h@2J�@1��@1��@1Dg@1#�@0�@0��@0e�@02�@/�r@/��@/J#@.�M@.��@.�@.W�@.�@-��@-�C@-J�@,�P@,��@,K^@,�@+��@+!-@*��@*��@*J@)k�@) \@(��@(�@(��@(��@(u�@("h@'�f@'
=@&��@&��@&��@&a|@&($@%�@%c@%=�@$�`@$��@$oi@$$@#ݘ@#�q@#|�@#=@#�@"�"@"��@"kQ@"�@!@!zx@!!�@ ��@ %�@�@��@��@{J@]�@o@�@i�@5?@.�@@�Z@��@m]@Vm@�K@��@��@c�@x@�q@��@��@��@g�@4�@�@�c@��@3�@�T@�z@zx@+�@�@;@�5@�@��@�e@�@�@��@��@iD@/�@�@@�8@�@�@�L@h
@=q@O@�@u@�3@��@�M@��@hs@B�@��@�@Z@<�@7@�@�m@خ@�0@dZ@&@�B@�h@v�@YK@E�@-@@�-@��@�@�7@p�@\�@Dg@%@�@�o@~(@�@oi@PH@(�@�@�@�@�
@\)@@�@��@�@�\@n�@a|@=q@!�@�D@�@�@��@T�@�@�`@Ɇ@�.@]d@	�@�m@ƨ@��@W?@+@�@
ߤ@
��@
GE@
�@
	@
e@
u@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AܕA�S&A�,=A��AA���AۖSAۃ�A��A�{JA�t�A�r�A�poA�k�A�iDA�Z�A�9�A���A�49A��GA��
A��|A��9A٬A�|PA�!�A�<6A���Aն�A�^5A�N<A�EA���Aͣ:A�4�A��WA���Aȷ�AƄ�A�=Aċ�A��A��2A��A�s�A��0A���A�z�A�B�A�NpA���A���A�� A���A�QA��lA���A��]A��A�F�A��A�j�A�aA�'�A�d�A�P�A���A��YA��AA�OBA�7�A���A���A��A���A��A�A���A�A�9XA�*�A�!A�j�A���A��!A���A��A~˒A}�A}+A|�~AzAw�As�Ap@Anl�Aj�+Ag��Ad��Aa�?A^~(A[6�AY7LAW��AVbNAO~�AM�AMVAL�AL�AJ�[AH��AGAEjAAD�A=^5A;4nA::�A8s�A7��A6�A3��A2�oA1�ZA0ѷA/�A,ZA+��A+�mA*�5A*1'A)��A(xA&�A%ݘA$��A#�cA"IRA!&�A =qAx�A˒A��A�DA��A�Ac A�"AA��A�yA~�A�2A�A�cA�A��A�"A��AcA&�A�rAAJ�A��A�_A	lA��A��A-A��A�Ag8A�A�NArGA�|A�6AaA�jAo�A{JA
�ZA
RTA	�7A		�AںA		lA�A	
=A	\�A��A�$A��A��A��A��Ag8A�A\�A˒A�A;dA�!AjA �)A (�@���@�w2@�!-@�0�@�j@�1�@��@� �@���@�X@��9@��@�v`@�6�@�$�@�$�@��:@�Y�@�%F@��@�kQ@�^5@�:*@��@�V@���@�P@��	@�q�@��@�J#@���@�  @�͟@�P@�@�Dg@��@�A�@�+@�8�@��@�<�@鐗@�r�@��W@�I@�c�@�9X@�F@�kQ@㢜@�P�@��@�?@�c�@�_�@��@��]@ޅ�@��@ܭ�@ܕ@�i�@�kQ@�h
@�>B@���@�x�@�A @�ں@�l"@�,=@ٴ�@�J#@��s@�O@�#�@�*�@ծ�@�o@Ա�@�S�@��)@�]�@�~�@ќ�@���@ДF@�Xy@�6@�@�6z@ίO@�^5@�1'@���@͌~@�X�@�@@��p@�w�@���@��	@�S�@�"h@��@�rG@��@Ȼ�@�w�@�>B@Ǳ[@�'�@ƅ�@�($@ŨX@ľ�@Č�@�bN@�;�@ð�@���@§�@�}V@�Ov@���@�\�@�9�@�!�@��@�w�@���@��@�4n@��@���@�Ĝ@�w�@��@�T�@��U@��&@�8�@�֡@��@�RT@��@��R@��x@��A@�N�@���@���@�8�@��@���@���@���@��@���@��+@�,=@�ƨ@��@��f@���@���@�q@�;�@�_@�Vm@���@���@�P�@�#�@�@@�
=@��@��@�7@��@���@��{@�U�@�IR@��@���@�>B@��)@��@@�m]@��B@�oi@�:�@�($@��@��[@���@�c@�X@�H�@���@�J�@���@�e,@��@�ߤ@���@�A�@��@��j@��@@�=�@��,@���@���@�Q@���@��S@�qv@�/�@�@@��j@�Ft@�O@��@��f@�S�@���@��[@���@�xl@��@��H@�t�@��@�bN@�6@�M@���@��@��_@�C-@�$�@�4@�@��
@���@��C@��M@�8�@���@���@�ϫ@�A @��f@��[@��!@���@�h
@�Ft@�$�@�@��-@�x�@���@��@���@�c @�"h@���@��w@�l�@�`B@�!�@���@���@���@��.@���@�K�@�G�@�=@��"@�N�@�  @��3@��n@�=�@���@��\@�{�@�kQ@�[�@�Z@���@��@�_�@��@��F@���@�RT@��@��p@��e@�^5@� �@��@��Q@���@�Dg@��!@�e�@�K^@�@�@�E�@��@��@���@��X@���@���@�J�@�&@��	@�ȴ@���@�kQ@�M@���@���@�ݘ@��C@���@�;d@��@���@� �@��d@�~�@�G�@��@��'@���@�z�@�@�@��@��"@��~@���@�$t@���@�kQ@�;�@�@�ϫ@��@�{J@�S&@�@���@�M@�@��@��*@�w2@�A @� \@��@��P@��@��@���@�l�@�1�@��@�
@Y@~��@~�@}��@}e,@}�@|��@|�@|��@{�W@{S�@z�r@y�T@ym]@xĜ@xbN@x%�@w�F@wRT@v�@v�h@vOv@v4@u��@t�e@t(�@s�P@s�@r҉@r~�@rh
@r=q@rO@q�Z@q�~@q�@q�@p�@o��@o��@o�m@oݘ@o�	@oe�@oP�@o4�@n�c@nv�@n-@m��@m�@l�)@l�v@l��@lh�@l!@l�@l�@k��@k��@kl�@j�b@jC�@i�@i�N@if�@h�@g i@f��@f�x@f� @f+k@eX@eF@e^�@e�@d��@ce�@a��@a \@`�)@`�z@_��@_��@_iD@_Y@^��@^ȴ@^n�@^8�@]�#@]f�@\��@[�@[�@Za|@YO�@X��@XtT@X|�@XXy@W��@W�@V�"@VZ�@UG�@TM@Tm�@T9X@S�@Se�@R�h@RE�@Q�@Qm]@QrG@QT�@Q�@Pr�@PPH@O��@O�f@N�'@M��@L��@L��@L�@L�U@MO�@M \@L�5@L��@L��@LFt@Kݘ@K�V@K33@K�@J��@J��@J4@I��@Ic@I`B@IDg@H��@H9X@G��@Ge�@G>�@G�@F�1@FJ@E�-@E�@EN<@D�@D�?@D�.@DN�@C��@C��@C��@C=@CS@B�B@BZ�@A�@A}�@A^�@A&�@@�	@@Ɇ@@�9@@��@?�@?��@?��@>�M@>&�@=��@=<6@=�@<֡@<�D@<[�@<V�@<q@<%�@;�W@;�@;W?@;9�@:��@:��@:��@:ں@:z@9�.@9Vm@8��@8�@8�@8C-@7�;@7��@7x@7A�@6�c@6xl@6$�@5�)@5��@5w2@5+@4�u@4D�@4 �@3��@3l�@3@2�h@2J�@1��@1��@1Dg@1#�@0�@0��@0e�@02�@/�r@/��@/J#@.�M@.��@.�@.W�@.�@-��@-�C@-J�@,�P@,��@,K^@,�@+��@+!-@*��@*��@*J@)k�@) \@(��@(�@(��@(��@(u�@("h@'�f@'
=@&��@&��@&��@&a|@&($@%�@%c@%=�@$�`@$��@$oi@$$@#ݘ@#�q@#|�@#=@#�@"�"@"��@"kQ@"�@!@!zx@!!�@ ��@ %�@�@��@��@{J@]�@o@�@i�@5?@.�@@�Z@��@m]@Vm@�K@��@��@c�@x@�q@��@��@��@g�@4�@�@�c@��@3�@�T@�z@zx@+�@�@;@�5@�@��@�e@�@�@��@��@iD@/�@�@@�8@�@�@�L@h
@=q@O@�@u@�3@��@�M@��@hs@B�@��@�@Z@<�@7@�@�m@خ@�0@dZ@&@�B@�h@v�@YK@E�@-@@�-@��@�@�7@p�@\�@Dg@%@�@�o@~(@�@oi@PH@(�@�@�@�@�
@\)@@�@��@�@�\@n�@a|@=q@!�@�D@�@�@��@T�@�@�`@Ɇ@�.@]d@	�@�m@ƨ@��@W?@+@�@
ߤ@
��@
GE@
�@
	@
e@
u@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B\B\BB(B�B�B�BVB�B�B\BvBBMB�B72B?�BFYBLBMBP�B]�Bb�Bt�B�B�FB��B�+B�>B��B��B��B�}B�B˒B�XBSB*�BM�BlWBx�BuBu%Bx�B�~B��B��B�HB��B��B�^B��B�mB��Bu?B^BQhBG�B>�B/ BsB��B�B��B�sB�[B�B��B}qB^�B6�B�B�B
�FB
��B
�zB
�1B
��B
��B
�3B
w2B
RB
0B
B

�B
gB	�}B	�*B	�eB	��B	ðB	�wB	�-B	��B	x�B	gmB	U�B	E�B	5tB	(XB	VB	�B	MB��B��B�*B�fB�MB��B�B��B׍B��B�WB�@B�B��B�FB�B�0B�B�B	�B	MB	�B	(sB	2�B	72B	>�B	=B	<�B	A;B	>BB	>�B	8�B	/B	)�B	&�B	#:B	!�B	 \B	�B	bB	�B		RB	!B	;�B	@�B	A;B	@�B	E�B	KxB	\CB	p!B	tTB	s�B	sMB	tnB	u�B	�DB	�2B	�PB	��B	�B	�jB	��B	��B	�MB	�AB	��B	� B	cB	�GB	�	B	��B	�NB	��B	��B	��B	��B	��B	��B	�B	�XB	�<B	��B	�-B	��B	ҽB	�B	ɺB	��B	˒B	�\B	�4B	ЗB	ΥB	�6B	�JB	�<B	��B	��B	�=B	�tB	��B	��B	�jB	��B	ҽB	��B	��B	�FB	��B	��B	ݘB	�~B	چB	ՁB	ԕB	�B	��B	�[B	�,B	ӏB	�YB	�1B	��B	��B	ؓB	�1B	ߤB	�hB	��B	��B	��B	�wB	��B	��B	�dB	��B	��B	�DB	�xB	��B	��B	��B	��B	��B	��B	��B	�tB	��B	��B	��B	�B	�B	��B	��B	��B	�nB	�9B	�B	��B	�TB	�nB	�%B	��B	�ZB	��B	��B	��B	��B	�2B	��B	�B	�B	��B	��B	��B	��B	�FB	��B	��B	��B	��B	�zB	��B	�FB	�FB	��B	�2B	�B	��B	��B	��B	�fB	��B	�RB	��B	��B	�lB	�RB	�lB	��B	�B	�lB	��B	��B	�lB	��B	�B	�B	�8B	��B	��B	��B	�RB	��B	�XB	��B	��B	�XB	�B	�dB	�B	�B	�PB	��B	��B	�B	�VB	��B	�(B	�(B	�wB	�.B	�}B	��B
 �B
 �B
�B
oB
;B
B
 �B
 �B
�B
�B
�B
�B
GB
GB
aB
aB
aB
�B
B
�B
�B
SB
�B
�B
�B
�B
�B
�B
tB
�B
zB
�B
�B
B
EB
EB
B
KB
�B
KB
�B
	B
	7B

	B

�B

�B
B
B
�B
B
6B
�B
�B
B
jB
6B
�B
�B
<B
�B
6B
�B
�B
bB
�B
�B
:B
�B
�B
�B
B
.B
�B
B
.B
�B
�B
HB
 B
:B
�B
�B
�B
�B
[B
@B
�B
�B
�B
B
�B
B
$B
sB
�B
_B
EB
�B
B
B
eB
B
�B
B
B
�B
�B
]B
IB
�B
�B
�B
�B
�B
/B
B
/B
IB
�B
�B
�B
�B
�B
�B
�B
�B
�B
 BB
 �B
 �B
!|B
!�B
"�B
"NB
"hB
!�B
"�B
"4B
"4B
!�B
!B
 �B
�B
�B
B
B
!B
pB
�B
 B
�B
B
~B
�B
IB
�B
OB
�B
;B
VB
�B
 'B
#�B
$tB
"�B
!�B
!�B
"�B
#B
"�B
"�B
"�B
"�B
#B
"�B
#�B
$�B
$�B
%,B
$�B
$�B
$�B
%,B
%�B
%�B
&�B
'B
'B
'RB
(
B
(
B
(XB
)�B
*KB
)�B
*0B
*B
*eB
*�B
+6B
+�B
,B
+�B
+6B
*�B
+6B
+�B
,WB
,qB
,�B
,�B
,�B
,�B
.B
.B
-�B
-�B
.}B
/�B
/�B
/�B
0UB
0�B
0�B
0�B
0�B
0�B
0�B
1B
1�B
1�B
1vB
1�B
1�B
2|B
3B
33B
3�B
3�B
3�B
3�B
4B
4B
4B
4�B
4�B
4�B
5?B
5ZB
5tB
5tB
5tB
5?B
5�B
5�B
5�B
6+B
6�B
6zB
6zB
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
88B
9$B
9	B
9>B
9�B
:DB
:�B
:xB
;B
;�B
;B
;�B
:�B
;�B
<PB
<�B
=�B
>]B
>�B
>wB
>�B
>BB
>B
=�B
=�B
=�B
?.B
?�B
@iB
A;B
A;B
AUB
AoB
AoB
AUB
@�B
@�B
@�B
A�B
A�B
A�B
@iB
@4B
@OB
AUB
B�B
A�B
B�B
C�B
EB
EB
D�B
D�B
CaB
C{B
DB
D�B
E�B
E�B
FB
F%B
F�B
FYB
F%B
E�B
ESB
D�B
D�B
EB
E�B
F%B
G�B
I�B
KxB
K�B
MB
L�B
L�B
L0B
KDB
J#B
K^B
L�B
M�B
M�B
L�B
L�B
MB
M�B
O\B
Q4B
P}B
PB
PbB
O�B
O�B
N�B
MB
LdB
L�B
NB
PHB
R�B
S�B
S�B
S�B
TaB
TaB
TFB
T�B
U�B
U�B
U�B
U�B
VB
VmB
VmB
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
X+B
X+B
XyB
X_B
XyB
XyB
X�B
X�B
X�B
X�B
YKB
Y1B
YeB
YB
Y�B
ZB
ZkB
Z�B
Z�B
Z�B
[	B
[#B
[	B
[#B
[�B
[qB
[WB
[�B
[�B
[#B
[#B
[WB
[#B
Z�B
Z�B
[#B
\B
\]B
]dB
^5B
^5B
^5B
^�B
^�B
^jB
^OB
^�B
_B
_�B
_�B
_�B
_�B
`vB
`vB
`�B
`�B
a-B
abB
a�B
a�B
b4B
bB
bB
bNB
b�B
b�B
b�B
cTB
cTB
c�B
c�B
d&B
dtB
d�B
d�B
d�B
e,B
eFB
ezB
e�B
e�B
e�B
f�B
gB
f�B
gB
g8B
gmB
gmB
g�B
g�B
h$B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
jKB
j�B
kB
kB
kQB
kQB
kkB
kkB
k�B
l"B
l�B
l�B
m)B
mCB
m]B
m�B
m�B
m�B
n/B
ncB
n�B
n�B
n�B
o B
o5B
oOB
o�B
o�B
o�B
o�B
o�B
pB
p;B
p�B
p�B
p�B
qvB
q�B
q�B
q�B
q�B
q�B
r-B
rGB
r�B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
tB
tB
tB
tTB
t�B
uB
uB
uB
u%B
u%B
utB
u�B
u�B
u�B
vFB
v�B
v�B
v�B
wfB
wfB
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y>B
y>B
yXB
yXB
yXB
yrB
y�B
y�B
y�B
zB
zB
zDB
z^B
zxB
zxB
z^B
z�B
z�B
{dB
{�B
{B
{B
{�B
{�B
{�B
{�B
{�B
|6B
|PB
|�B
|�B
|�B
|�B
}B
}B
|�B
}VB
}VB
}�B
}�B
~B
~B
~(B
~wB
~�B
~�B
~�B
~�B
B
B
HB
.B
.B
.B
HB
.B
B
.B
}B
}B
�B
�B
�B
�B
�B
�B
� B
� B
�4B
�OB
��B
��B
��B
�B
� B
��B
��B
��B
��B
��B
�B
�'B
�AB
�uB
��B
��B
��B
��B
��B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B\B\BB(B�B�B�BVB�B�B\BvBBMB�B72B?�BFYBLBMBP�B]�Bb�Bt�B�B�FB��B�+B�>B��B��B��B�}B�B˒B�XBSB*�BM�BlWBx�BuBu%Bx�B�~B��B��B�HB��B��B�^B��B�mB��Bu?B^BQhBG�B>�B/ BsB��B�B��B�sB�[B�B��B}qB^�B6�B�B�B
�FB
��B
�zB
�1B
��B
��B
�3B
w2B
RB
0B
B

�B
gB	�}B	�*B	�eB	��B	ðB	�wB	�-B	��B	x�B	gmB	U�B	E�B	5tB	(XB	VB	�B	MB��B��B�*B�fB�MB��B�B��B׍B��B�WB�@B�B��B�FB�B�0B�B�B	�B	MB	�B	(sB	2�B	72B	>�B	=B	<�B	A;B	>BB	>�B	8�B	/B	)�B	&�B	#:B	!�B	 \B	�B	bB	�B		RB	!B	;�B	@�B	A;B	@�B	E�B	KxB	\CB	p!B	tTB	s�B	sMB	tnB	u�B	�DB	�2B	�PB	��B	�B	�jB	��B	��B	�MB	�AB	��B	� B	cB	�GB	�	B	��B	�NB	��B	��B	��B	��B	��B	��B	�B	�XB	�<B	��B	�-B	��B	ҽB	�B	ɺB	��B	˒B	�\B	�4B	ЗB	ΥB	�6B	�JB	�<B	��B	��B	�=B	�tB	��B	��B	�jB	��B	ҽB	��B	��B	�FB	��B	��B	ݘB	�~B	چB	ՁB	ԕB	�B	��B	�[B	�,B	ӏB	�YB	�1B	��B	��B	ؓB	�1B	ߤB	�hB	��B	��B	��B	�wB	��B	��B	�dB	��B	��B	�DB	�xB	��B	��B	��B	��B	��B	��B	��B	�tB	��B	��B	��B	�B	�B	��B	��B	��B	�nB	�9B	�B	��B	�TB	�nB	�%B	��B	�ZB	��B	��B	��B	��B	�2B	��B	�B	�B	��B	��B	��B	��B	�FB	��B	��B	��B	��B	�zB	��B	�FB	�FB	��B	�2B	�B	��B	��B	��B	�fB	��B	�RB	��B	��B	�lB	�RB	�lB	��B	�B	�lB	��B	��B	�lB	��B	�B	�B	�8B	��B	��B	��B	�RB	��B	�XB	��B	��B	�XB	�B	�dB	�B	�B	�PB	��B	��B	�B	�VB	��B	�(B	�(B	�wB	�.B	�}B	��B
 �B
 �B
�B
oB
;B
B
 �B
 �B
�B
�B
�B
�B
GB
GB
aB
aB
aB
�B
B
�B
�B
SB
�B
�B
�B
�B
�B
�B
tB
�B
zB
�B
�B
B
EB
EB
B
KB
�B
KB
�B
	B
	7B

	B

�B

�B
B
B
�B
B
6B
�B
�B
B
jB
6B
�B
�B
<B
�B
6B
�B
�B
bB
�B
�B
:B
�B
�B
�B
B
.B
�B
B
.B
�B
�B
HB
 B
:B
�B
�B
�B
�B
[B
@B
�B
�B
�B
B
�B
B
$B
sB
�B
_B
EB
�B
B
B
eB
B
�B
B
B
�B
�B
]B
IB
�B
�B
�B
�B
�B
/B
B
/B
IB
�B
�B
�B
�B
�B
�B
�B
�B
�B
 BB
 �B
 �B
!|B
!�B
"�B
"NB
"hB
!�B
"�B
"4B
"4B
!�B
!B
 �B
�B
�B
B
B
!B
pB
�B
 B
�B
B
~B
�B
IB
�B
OB
�B
;B
VB
�B
 'B
#�B
$tB
"�B
!�B
!�B
"�B
#B
"�B
"�B
"�B
"�B
#B
"�B
#�B
$�B
$�B
%,B
$�B
$�B
$�B
%,B
%�B
%�B
&�B
'B
'B
'RB
(
B
(
B
(XB
)�B
*KB
)�B
*0B
*B
*eB
*�B
+6B
+�B
,B
+�B
+6B
*�B
+6B
+�B
,WB
,qB
,�B
,�B
,�B
,�B
.B
.B
-�B
-�B
.}B
/�B
/�B
/�B
0UB
0�B
0�B
0�B
0�B
0�B
0�B
1B
1�B
1�B
1vB
1�B
1�B
2|B
3B
33B
3�B
3�B
3�B
3�B
4B
4B
4B
4�B
4�B
4�B
5?B
5ZB
5tB
5tB
5tB
5?B
5�B
5�B
5�B
6+B
6�B
6zB
6zB
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
88B
9$B
9	B
9>B
9�B
:DB
:�B
:xB
;B
;�B
;B
;�B
:�B
;�B
<PB
<�B
=�B
>]B
>�B
>wB
>�B
>BB
>B
=�B
=�B
=�B
?.B
?�B
@iB
A;B
A;B
AUB
AoB
AoB
AUB
@�B
@�B
@�B
A�B
A�B
A�B
@iB
@4B
@OB
AUB
B�B
A�B
B�B
C�B
EB
EB
D�B
D�B
CaB
C{B
DB
D�B
E�B
E�B
FB
F%B
F�B
FYB
F%B
E�B
ESB
D�B
D�B
EB
E�B
F%B
G�B
I�B
KxB
K�B
MB
L�B
L�B
L0B
KDB
J#B
K^B
L�B
M�B
M�B
L�B
L�B
MB
M�B
O\B
Q4B
P}B
PB
PbB
O�B
O�B
N�B
MB
LdB
L�B
NB
PHB
R�B
S�B
S�B
S�B
TaB
TaB
TFB
T�B
U�B
U�B
U�B
U�B
VB
VmB
VmB
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
X+B
X+B
XyB
X_B
XyB
XyB
X�B
X�B
X�B
X�B
YKB
Y1B
YeB
YB
Y�B
ZB
ZkB
Z�B
Z�B
Z�B
[	B
[#B
[	B
[#B
[�B
[qB
[WB
[�B
[�B
[#B
[#B
[WB
[#B
Z�B
Z�B
[#B
\B
\]B
]dB
^5B
^5B
^5B
^�B
^�B
^jB
^OB
^�B
_B
_�B
_�B
_�B
_�B
`vB
`vB
`�B
`�B
a-B
abB
a�B
a�B
b4B
bB
bB
bNB
b�B
b�B
b�B
cTB
cTB
c�B
c�B
d&B
dtB
d�B
d�B
d�B
e,B
eFB
ezB
e�B
e�B
e�B
f�B
gB
f�B
gB
g8B
gmB
gmB
g�B
g�B
h$B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
jKB
j�B
kB
kB
kQB
kQB
kkB
kkB
k�B
l"B
l�B
l�B
m)B
mCB
m]B
m�B
m�B
m�B
n/B
ncB
n�B
n�B
n�B
o B
o5B
oOB
o�B
o�B
o�B
o�B
o�B
pB
p;B
p�B
p�B
p�B
qvB
q�B
q�B
q�B
q�B
q�B
r-B
rGB
r�B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
tB
tB
tB
tTB
t�B
uB
uB
uB
u%B
u%B
utB
u�B
u�B
u�B
vFB
v�B
v�B
v�B
wfB
wfB
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y>B
y>B
yXB
yXB
yXB
yrB
y�B
y�B
y�B
zB
zB
zDB
z^B
zxB
zxB
z^B
z�B
z�B
{dB
{�B
{B
{B
{�B
{�B
{�B
{�B
{�B
|6B
|PB
|�B
|�B
|�B
|�B
}B
}B
|�B
}VB
}VB
}�B
}�B
~B
~B
~(B
~wB
~�B
~�B
~�B
~�B
B
B
HB
.B
.B
.B
HB
.B
B
.B
}B
}B
�B
�B
�B
�B
�B
�B
� B
� B
�4B
�OB
��B
��B
��B
�B
� B
��B
��B
��B
��B
��B
�B
�'B
�AB
�uB
��B
��B
��B
��B
��B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221015034048  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221015034059  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221015034100  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221015034100                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221015124104  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221015124104  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221015035843                      G�O�G�O�G�O�                