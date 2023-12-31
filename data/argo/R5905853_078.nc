CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:36:42Z creation;2022-06-04T17:36:43Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604173642  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               NA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�hR;Z��1   @�hR�Ӡ@0ܬ1&��c���n�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp��Bx  B�  B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B���B�33B�33B�  B���B�  B�ffB�  B�  B�33B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CFL�CG�fCJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�CrL�Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Q�@u�@��\@��\AG�A=G�A]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�Bp�BwQ�BQ�B���B�u�B���B�u�B���B���B���B���B���B���B���B���B���B��)B�\B�B�B�u�B��)B��)BϨ�B�u�Bר�B�\Bߨ�B��B��)B��B�u�B��B���B���B���C�{C�{C�{C�{C	�{C�{C�{C�{C�{C�C�{C�{C��C�{C�{C�{C!�{C#�{C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CF!HCG��CI�{CK�{CM��CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�Cr!HCs��Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��pC��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��
C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&{�D&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�DtuDt�DuuDu�DvuDv�DwuDw�DxuDx�DyuDy�DzuDz�D{uD{�D|uD|�D}uD}�D~uD~�DuD�D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�Dº�D���D�:�D�z�Dú�D���D�:�D�z�Dĺ�D���D�:�D�z�Dź�D���D�:�D�z�Dƺ�D���D�:�D�z�DǺ�D���D�:�D�z�DȺ�D���D�:�D�z�Dɺ�D���D�:�D�z�Dʺ�D���D�:�D�z�D˺�D���D�:�D�z�D̺�D���D�:�D�z�Dͺ�D���D�:�D�z�Dκ�D���D�:�D�z�DϺ�D���D�:�D�z�Dк�D���D�:�D�z�DѺ�D���D�:�D�z�DҺ�D���D�:�D�z�DӺ�D���D�:�D�z�DԺ�D���D�:�D�z�Dպ�D���D�:�D�z�Dֺ�D���D�:�D�z�D׺�D���D�:�D�z�Dغ�D���D�:�D�z�Dٺ�D���D�:�D�z�Dں�D���D�:�D�z�Dۺ�D���D�:�D�z�Dܺ�D���D�:�D�z�Dݺ�D���D�:�D�z�D޺�D���D�:�D�z�Dߺ�D���D�:�D�z�DຏD���D�:�D�z�DẏD���D�:�D�z�D⺏D���D�:�D�z�D㺏D���D�:�D�z�D亏D���D�:�D�z�D序D���D�:�D�z�D溏D���D�:�D�z�D纏D���D�:�D�z�D躏D���D�:�D�z�D麏D���D�:�D�z�D꺏D���D�:�D�z�D뺏D���D�:�D�z�D캏D���D�:�D�z�D���D���D�:�D�z�DD���D�:�D�z�DﺏD���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D�D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�z�D���D���D�:�D�g\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�K�A�GEA�9$A�5A�.}A�D�A�C�A�&A�hA��A�GA��A�;A���A���A��A��`A��aAǾ�Aǩ�Aǥ�AǥFAǤtAǥFAǠ�AǛ	AǗ�AǗ�AǗ�AǌAǁ�A�.A�rGA�[WA���Aư�A�7�A��A�^�A�K�A�PHA�K�A�GA�.A��tAĬqA���A�n�A�NA�}�A��LA���A�&�A�oA��A�JXA�� A��dA���A�e�A�"A���A���A���A��A�ɺA���A��hA�`vA�A�N�A���A���A��A���A��A�sMA��A�VmA�a�A���A�(XA���A�YA��A�`BA�u�A��[A�RTA���A��A�4�A�W?A�~�A���A�(�A�&�A��AA��IA���A�/�A�_pA|��As�Anh�Ak{�Ae�?Aa;�AZ,�AY!AYbAX�3AUzAR iAQMjAP��AN�AL��AKXAH�>AF��AER�AB?�A@A�A?�A;�DA8ѷA8kQA3��A2/A0b�A/�IA/VmA/(�A-�mA,u�A+P�A+�A*�A)a|A'�A'�xA%��A$��A"��A"IRA�jA?}A��AE�A�OA �+A �9A!0UA"N�A$=qA$��A$t�A$~�A${A#�EA#�:A!��A!��A �mA�XA�IA+A��A�LA��A�RA�#A��A^5A�QA��A�)A�Ae,AdZATaA�A��A9XA��A�A\�A��A�XA?}AeAv�A��A^5A��A�A��A#:A�A�cAƨA?A�AiDA�)A�,A�	A�A��A��AMjA��A�A��A��Az�A��A;dA��AA�A�A�KA.�A�DAԕA��A�mA��AB�A�`A�-Ay�AB[A
��A
_pA
`�A
ZA	��A	�A��A%A��A��A��A;dAqA��A͟A�zA�LA&A�mA��AR�A�AA?A ��A >B@�e�@�@�@��t@�y�@��@�1'@���@���@�J�@��@��y@�l"@�Ov@�0U@��0@�_p@�C@��h@��@�"�@��@�4@��@��0@���@�/�@��"@�p;@�@O@�}@�	�@��&@���@�|�@��@�Y@�@��@�z�@뇔@�@�dZ@�c�@�qv@�m]@��@�^�@�S@�ߤ@�1@�(�@���@俱@��@⤩@���@�ں@���@ߙ�@�J#@��@��)@ީ�@�c @�.�@��@ܚ@�خ@�qv@�@O@��@�7�@ٸ�@�~�@��@ئL@�A�@�`B@� �@�\)@�G@Ӂ�@�!�@�=q@��@ѭC@�%@��}@�)_@γh@�?�@�K�@��6@�0�@��8@ʿ�@ʗ�@ʇ+@�j@�O@��@ɓ@��@ȣ�@�%�@�}�@ǁ�@�F�@��'@�Xy@�G@�e,@Ą�@��@��N@�˒@�U2@���@ċD@��@ô�@�5�@�@��o@�;@�Dg@��@��H@��!@��+@�r�@�_�@�D�@��@�s�@�_p@�Vm@�2a@��E@��+@�.�@�ϫ@�}�@���@�}V@�@���@�_p@���@��x@�=q@���@�;d@��H@���@���@�)�@���@�;d@��z@��@���@�[W@� \@��@�e@��$@�_p@�B�@�-w@��@�͟@�C�@���@�1�@��@��#@��:@�$t@��@���@�u%@�tT@�u@���@� i@�V�@�
�@���@�#�@�%@���@�ߤ@��E@���@��L@�{�@�E�@���@��{@���@�1�@��@�Vm@��@��v@��@��F@��@�Xy@��@���@��H@�{J@�@@���@�`�@��@��S@�>�@�֡@���@��L@�`�@�GE@�1'@�*�@���@���@�dZ@�C�@��@���@�{�@�	@�˒@��n@�l�@�>�@��@���@�kQ@�K^@��@��#@���@�`B@�4�@��f@���@�M@��V@�X@��'@���@�H�@�0U@�,=@�G@��W@���@�Q�@�;@���@�(�@��@�	l@��]@��$@���@�h
@�>B@�-@�e@���@��f@�S&@�͟@���@�'R@��D@���@�o @�͟@�Xy@��@��H@�C�@���@���@���@�oi@�;�@�e@���@���@���@���@�J�@��@���@�u%@�Ft@�O@��@��@��@��@���@��	@�zx@�hs@��@���@�-�@�ϫ@�p�@��@��@��@��!@��+@�xl@�;�@�x@��@��@�^�@�@��]@��e@�z@�;�@�@��Q@���@�`B@��@���@��X@���@�w�@�]d@�^5@���@���@��q@�H�@�C@��"@���@���@�@��@�+@��@���@�9X@��@�e�@�4@��@��K@�ȴ@��m@���@��@��x@��@���@�xl@�e�@�_�@�U2@�M@�Ta@�PH@�K^@�5?@�	@��@���@�x�@�"�@��@�l"@�@~��@~.�@~.�@~.�@~_@}:�@|�O@|6@{X�@zJ@y&�@x��@w�0@vOv@u��@u�d@t�)@t!@s��@sF�@r�1@q�z@qQ�@pI�@o�w@o�@n��@m��@m�@m��@m[W@l��@l�I@l�D@ll"@lA�@l~@k�@k��@k=@jh
@i��@i�>@i�@i�@h	�@g��@f�2@f�x@fff@fM�@f$�@f�@e�@e��@e}�@e%@d��@d|�@c�@c� @cx@cH�@cK�@cF�@c�@b��@b
�@a�@a��@a%@`9X@`M@_s@^��@]�@\��@\�Y@\Ft@\�@[�;@[��@Z:*@Y��@X�@X@Ws@V�"@V��@V�8@V�@V}V@U�N@U*0@TV�@Sݘ@S@O@RL0@Q�@Q��@Qe,@Q \@Pѷ@PU2@O�}@N�@N��@Mԕ@M5�@M5�@L��@L2�@K��@K>�@K�@J��@J�F@I�@Io @H�[@Hz�@HU2@G�w@G{J@Gx@GS@F��@Fxl@FW�@F�@D��@Dj@Dc�@Dm�@Dg8@Dj@Doi@Dq@DC-@D$@D�@C�@C��@C@O@B�@A�Z@AA @@�I@@tT@@N�@?��@?S�@>�M@>	@=��@=��@=}�@=c�@=0�@<�K@<�.@<l"@<Q�@<D�@<  @;��@;$t@:��@9��@9�@9�@9��@9��@9\�@9%F@8�5@8w�@8�@8�@7�]@7خ@7��@7�*@7n/@7�@6��@6@�@6O@5�)@5��@5+@4�Y@4V�@3�w@2�+@2-@1�>@1��@1��@1j@1?}@0�z@0u�@0U2@0Ft@0�@/�W@/�K@/��@/e�@//�@.��@.�}@.5?@-�o@-��@-O�@,�)@,4n@+�A@+��@+�k@++@*��@*�@*a|@*M�@*E�@*&�@*e@)��@)��@)��@)�@(�4@(bN@(<�@'�k@'iD@&�@&�,@&�@&�r@&@�@%��@%��@%�@%x�@%5�@%-w@$�@$��@#��@#�f@#J#@#&@#'�@#@"��@"�@"�@"�'@"��@"�r@"^5@!�@!��@!N<@!-w@ �@ �O@ �@ oi@ PH@ -�@�@��@��@@O@�X@��@}V@1�@ �@��@Vm@<6@��@�@��@c�@>B@ �@�W@�@��@X�@�@�}@�@~�@s�@s�@Q@�@�)@��@��@�-@p�@=�@*0@+@�@ѷ@tT@1'@*�@'R@G@�r@�@�W@��@��@y�@H�@C@�@��@��@��@}V@l�@ff@a|@8�@@�)@�@��@�t@��@��@J�@�@��@�$@��@c�@�@�@��@�@�@qv@9�@�@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�K�A�GEA�9$A�5A�.}A�D�A�C�A�&A�hA��A�GA��A�;A���A���A��A��`A��aAǾ�Aǩ�Aǥ�AǥFAǤtAǥFAǠ�AǛ	AǗ�AǗ�AǗ�AǌAǁ�A�.A�rGA�[WA���Aư�A�7�A��A�^�A�K�A�PHA�K�A�GA�.A��tAĬqA���A�n�A�NA�}�A��LA���A�&�A�oA��A�JXA�� A��dA���A�e�A�"A���A���A���A��A�ɺA���A��hA�`vA�A�N�A���A���A��A���A��A�sMA��A�VmA�a�A���A�(XA���A�YA��A�`BA�u�A��[A�RTA���A��A�4�A�W?A�~�A���A�(�A�&�A��AA��IA���A�/�A�_pA|��As�Anh�Ak{�Ae�?Aa;�AZ,�AY!AYbAX�3AUzAR iAQMjAP��AN�AL��AKXAH�>AF��AER�AB?�A@A�A?�A;�DA8ѷA8kQA3��A2/A0b�A/�IA/VmA/(�A-�mA,u�A+P�A+�A*�A)a|A'�A'�xA%��A$��A"��A"IRA�jA?}A��AE�A�OA �+A �9A!0UA"N�A$=qA$��A$t�A$~�A${A#�EA#�:A!��A!��A �mA�XA�IA+A��A�LA��A�RA�#A��A^5A�QA��A�)A�Ae,AdZATaA�A��A9XA��A�A\�A��A�XA?}AeAv�A��A^5A��A�A��A#:A�A�cAƨA?A�AiDA�)A�,A�	A�A��A��AMjA��A�A��A��Az�A��A;dA��AA�A�A�KA.�A�DAԕA��A�mA��AB�A�`A�-Ay�AB[A
��A
_pA
`�A
ZA	��A	�A��A%A��A��A��A;dAqA��A͟A�zA�LA&A�mA��AR�A�AA?A ��A >B@�e�@�@�@��t@�y�@��@�1'@���@���@�J�@��@��y@�l"@�Ov@�0U@��0@�_p@�C@��h@��@�"�@��@�4@��@��0@���@�/�@��"@�p;@�@O@�}@�	�@��&@���@�|�@��@�Y@�@��@�z�@뇔@�@�dZ@�c�@�qv@�m]@��@�^�@�S@�ߤ@�1@�(�@���@俱@��@⤩@���@�ں@���@ߙ�@�J#@��@��)@ީ�@�c @�.�@��@ܚ@�خ@�qv@�@O@��@�7�@ٸ�@�~�@��@ئL@�A�@�`B@� �@�\)@�G@Ӂ�@�!�@�=q@��@ѭC@�%@��}@�)_@γh@�?�@�K�@��6@�0�@��8@ʿ�@ʗ�@ʇ+@�j@�O@��@ɓ@��@ȣ�@�%�@�}�@ǁ�@�F�@��'@�Xy@�G@�e,@Ą�@��@��N@�˒@�U2@���@ċD@��@ô�@�5�@�@��o@�;@�Dg@��@��H@��!@��+@�r�@�_�@�D�@��@�s�@�_p@�Vm@�2a@��E@��+@�.�@�ϫ@�}�@���@�}V@�@���@�_p@���@��x@�=q@���@�;d@��H@���@���@�)�@���@�;d@��z@��@���@�[W@� \@��@�e@��$@�_p@�B�@�-w@��@�͟@�C�@���@�1�@��@��#@��:@�$t@��@���@�u%@�tT@�u@���@� i@�V�@�
�@���@�#�@�%@���@�ߤ@��E@���@��L@�{�@�E�@���@��{@���@�1�@��@�Vm@��@��v@��@��F@��@�Xy@��@���@��H@�{J@�@@���@�`�@��@��S@�>�@�֡@���@��L@�`�@�GE@�1'@�*�@���@���@�dZ@�C�@��@���@�{�@�	@�˒@��n@�l�@�>�@��@���@�kQ@�K^@��@��#@���@�`B@�4�@��f@���@�M@��V@�X@��'@���@�H�@�0U@�,=@�G@��W@���@�Q�@�;@���@�(�@��@�	l@��]@��$@���@�h
@�>B@�-@�e@���@��f@�S&@�͟@���@�'R@��D@���@�o @�͟@�Xy@��@��H@�C�@���@���@���@�oi@�;�@�e@���@���@���@���@�J�@��@���@�u%@�Ft@�O@��@��@��@��@���@��	@�zx@�hs@��@���@�-�@�ϫ@�p�@��@��@��@��!@��+@�xl@�;�@�x@��@��@�^�@�@��]@��e@�z@�;�@�@��Q@���@�`B@��@���@��X@���@�w�@�]d@�^5@���@���@��q@�H�@�C@��"@���@���@�@��@�+@��@���@�9X@��@�e�@�4@��@��K@�ȴ@��m@���@��@��x@��@���@�xl@�e�@�_�@�U2@�M@�Ta@�PH@�K^@�5?@�	@��@���@�x�@�"�@��@�l"@�@~��@~.�@~.�@~.�@~_@}:�@|�O@|6@{X�@zJ@y&�@x��@w�0@vOv@u��@u�d@t�)@t!@s��@sF�@r�1@q�z@qQ�@pI�@o�w@o�@n��@m��@m�@m��@m[W@l��@l�I@l�D@ll"@lA�@l~@k�@k��@k=@jh
@i��@i�>@i�@i�@h	�@g��@f�2@f�x@fff@fM�@f$�@f�@e�@e��@e}�@e%@d��@d|�@c�@c� @cx@cH�@cK�@cF�@c�@b��@b
�@a�@a��@a%@`9X@`M@_s@^��@]�@\��@\�Y@\Ft@\�@[�;@[��@Z:*@Y��@X�@X@Ws@V�"@V��@V�8@V�@V}V@U�N@U*0@TV�@Sݘ@S@O@RL0@Q�@Q��@Qe,@Q \@Pѷ@PU2@O�}@N�@N��@Mԕ@M5�@M5�@L��@L2�@K��@K>�@K�@J��@J�F@I�@Io @H�[@Hz�@HU2@G�w@G{J@Gx@GS@F��@Fxl@FW�@F�@D��@Dj@Dc�@Dm�@Dg8@Dj@Doi@Dq@DC-@D$@D�@C�@C��@C@O@B�@A�Z@AA @@�I@@tT@@N�@?��@?S�@>�M@>	@=��@=��@=}�@=c�@=0�@<�K@<�.@<l"@<Q�@<D�@<  @;��@;$t@:��@9��@9�@9�@9��@9��@9\�@9%F@8�5@8w�@8�@8�@7�]@7خ@7��@7�*@7n/@7�@6��@6@�@6O@5�)@5��@5+@4�Y@4V�@3�w@2�+@2-@1�>@1��@1��@1j@1?}@0�z@0u�@0U2@0Ft@0�@/�W@/�K@/��@/e�@//�@.��@.�}@.5?@-�o@-��@-O�@,�)@,4n@+�A@+��@+�k@++@*��@*�@*a|@*M�@*E�@*&�@*e@)��@)��@)��@)�@(�4@(bN@(<�@'�k@'iD@&�@&�,@&�@&�r@&@�@%��@%��@%�@%x�@%5�@%-w@$�@$��@#��@#�f@#J#@#&@#'�@#@"��@"�@"�@"�'@"��@"�r@"^5@!�@!��@!N<@!-w@ �@ �O@ �@ oi@ PH@ -�@�@��@��@@O@�X@��@}V@1�@ �@��@Vm@<6@��@�@��@c�@>B@ �@�W@�@��@X�@�@�}@�@~�@s�@s�@Q@�@�)@��@��@�-@p�@=�@*0@+@�@ѷ@tT@1'@*�@'R@G@�r@�@�W@��@��@y�@H�@C@�@��@��@��@}V@l�@ff@a|@8�@@�)@�@��@�t@��@��@J�@�@��@�$@��@c�@�@�@��@�@�@qv@9�@�@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�=B��B��B�7B��B�QB��B��B��B�1B��B�B��B�pB��B�FB�B�vB��B�9B��B��B�EB�nB��B��B��B�B��B�.B	[B	�B		RB	�B	4B	DB	�jB	�FB
�B
4�B
A�B
NVB
U�B
~BB
��B
�,B
�BB
�KB
ɺB
�[B
�B}B�B#B(�B5ZB:*B:�B:*BRoBsB��B�ZB��B�(BخB�pB�B�<B�}B��B��BרBּB�bB�rB��B��B�jB��B��B��BxlBpUB_�BBuB/5BEB
�BB
��B
��B
�eB
�WB
ǮB
�rB
�IB
��B
�,B
l=B
ZB
9XB
�B	��B	żB	�OB	��B	k�B	T�B	/�B	)�B	(�B	%zB	�B	}B	�B		�B	YB	%B	�B	�B	 4B	 �B	 B	B	 �B	,"B	 �B	�B		�B�cB�fB�-B��B�)B��B��B�B��B�.B	DB	oB	B	�B	&B	B	
#B	�B	SB	B	,�B	OB	n�B	u�B	�uB	�B	ǮB	�VB	�B	�B	�B	�sB	�}B	��B	یB	�B	�qB	�1B	רB	޸B	��B	�
B	�MB	��B	�B	�B
B
�B
�B
B	�B	��B
�B
B
pB
 �B
$&B
($B
-]B
2�B
7�B
G�B
K�B
J#B
OB
SB
RB
Q�B
RB
R�B
S&B
SB
S&B
S�B
R�B
RB
O�B
N�B
L�B
IB
F�B
F�B
MB
PB
N�B
MB
LdB
J�B
H�B
G_B
E�B
DgB
B�B
A�B
@iB
?�B
@ B
@�B
?cB
>wB
>�B
=�B
<�B
<B
<B
;B
9	B
9	B
9�B
:�B
9	B
8B
5tB
2�B
/�B
,�B
,=B
(�B
&�B
)DB
,"B
,B
+�B
*�B
*B
)*B
&�B
#�B
!HB
 B
�B
QB
�B
B
WB
=B
�B
�B
kB
7B
B
#B
]B
B
B
WB
�B
kB
�B
�B
B
�B
7B
7B
�B
kB
eB
KB
�B
�B
�B

B
�B
SB
FB
�B
JB
	�B
B
�B
B
[B
[B
B
MB
�B
�B
MB
�B
B
B
 �B	��B	�<B	�DB	�$B	��B	��B	�B	�fB	�B	�B	��B	��B	��B	��B	��B	�FB	�zB	�+B	��B	�LB	�LB	�B	��B	�FB	�%B	�%B	�B	�GB	�oB	��B	��B	�OB	�}B	�IB	�CB	�B	��B	�B	��B	��B	�IB	�B	��B	� B	� B	� B	� B	�B	�B	��B	��B	�B	�'B	�-B	�nB	�B	�nB	��B	�B	�B	��B	�B	��B	�B	�$B	��B	��B	��B	�(B	�VB	�B	��B	��B
 OB
�B
�B
'B
�B
�B
�B
AB
�B
�B
�B
�B
�B
�B
 B
 B
 �B
 �B
 �B
 B
B
�B
GB
[B
oB
 �B
 B	��B	��B
B
'B
�B
 iB	��B	�BB	�VB	�B	�dB	�B	�6B	��B	��B	��B	��B	�B	�<B	�VB
 �B
�B
 B
 �B
�B
'B
�B
3B
�B
�B

	B
	�B
	7B

=B

�B
xB
�B
B
�B
~B
dB
0B
�B
�B
�B
�B
�B
DB
^B

�B

rB

�B

�B

�B

�B

�B

�B
�B
B
0B
B
jB
<B
�B
jB
B
�B
�B
"B
�B
6B
�B
(B
�B
vB
�B
.B
�B
�B
�B
HB
�B
�B
bB
NB
�B
B
4B
NB
NB
TB
�B
,B
aB
uB
uB
B
�B
�B
�B
�B
�B
�B
�B
2B
�B
�B
�B
B
2B
�B
SB
SB
SB
�B
SB
B
?B
�B
?B
�B
�B
B
�B
sB
�B
EB
�B
�B
�B
�B
�B
kB
eB
�B
7B
B
�B
�B
QB
�B
�B
�B
�B
=B
�B
qB
�B
B
�B
�B
B
B
B
�B
VB
pB
�B
 'B
�B
 BB
 \B
 \B
 \B
 �B
!|B
"NB
"�B
"�B
"�B
#�B
$@B
$tB
$tB
$�B
%FB
%zB
%�B
&B
%�B
%�B
%�B
&B
%�B
&�B
&�B
&�B
'B
'B
'mB
'�B
'8B
(sB
'�B
(
B
($B
(�B
(�B
(XB
'�B
(�B
*�B
+B
+6B
+�B
,�B
,�B
-wB
-�B
-�B
-�B
.}B
.B
./B
.IB
.B
.B
.}B
.cB
./B
.cB
.B
.�B
-�B
.�B
.}B
.B
.�B
.�B
.�B
/ B
/�B
/�B
/�B
1�B
2�B
3B
2aB
2GB
2aB
2�B
2|B
2B
2-B
33B
3B
3B
4B
3�B
4B
4�B
4TB
3hB
2-B
1�B
1�B
0�B
0�B
1[B
0�B
0�B
0�B
1[B
0�B
0�B
1vB
2�B
3�B
2�B
33B
3�B
3�B
3�B
3�B
3�B
4TB
4�B
4�B
3�B
5tB
4�B
4nB
4�B
5�B
6�B
72B
9	B
8�B
9$B
9XB
:�B
;�B
<PB
<6B
<�B
<PB
<�B
=�B
=qB
>B
>�B
?�B
?cB
@B
?�B
?�B
@ B
?cB
@OB
@�B
A B
A B
@�B
AB
AB
@�B
@iB
@iB
@iB
@�B
@�B
A�B
A�B
BAB
A�B
AoB
A�B
BuB
C-B
C�B
D3B
D3B
E�B
EmB
E�B
E�B
E�B
E�B
E�B
E�B
E9B
E9B
EB
EB
E9B
E�B
E�B
FtB
F�B
F�B
F�B
F�B
F�B
F�B
F�B
FtB
F�B
G_B
G+B
F�B
G�B
HB
HB
G�B
HB
K�B
L�B
L�B
LJB
L�B
L�B
L~B
LJB
L�B
MB
M6B
MjB
NB
N�B
O�B
P.B
P�B
Q�B
QB
Q�B
RTB
QhB
R B
R B
R:B
R B
Q�B
R B
R�B
RTB
SuB
R�B
R�B
R�B
S�B
S�B
S�B
TB
T�B
T�B
U2B
UgB
UgB
U�B
T�B
UB
VB
V�B
V�B
V�B
WsB
W�B
W?B
W�B
X�B
X�B
YKB
YeB
Y�B
YB
Y�B
ZQB
ZB
[	B
[�B
[�B
[qB
Z�B
[�B
[�B
[qB
]B
\�B
\�B
\�B
\�B
\�B
\]B
]dB
\�B
]IB
^�B
_;B
_pB
_�B
`�B
`�B
`�B
a�B
a�B
a|B
a-B
a�B
bB
b�B
c B
cB
b�B
c�B
cTB
c�B
c�B
c�B
c�B
dZB
d&B
d�B
ezB
ezB
f2B
fB
e�B
e�B
ffB
gB
f�B
f�B
gB
g8B
gB
gmB
h
B
iDB
i�B
i�B
i�B
iyB
jeB
jKB
j�B
j0B
jKB
j0B
kkB
kB
k�B
k�B
k�B
k�B
lB
mB
l�B
lWB
m)B
m]B
mB
m]B
mB
m�B
m�B
m�B
n/B
m�B
n/B
m�B
n}B
ncB
nB
n�B
o B
oOB
oB
oB
oOB
oOB
o�B
o�B
pB
p�B
p�B
p�B
qB
p�B
qB
qAB
qAB
q[B
qvB
qAB
q�B
q�B
rB
rB
raB
q�B
raB
shB
s3B
r�B
s�B
s�B
sB
sMB
s�B
shB
s�B
tB
t�B
t�B
t�B
u?B
u�B
u�B
u�B
u?B
u%B
t�B
uZB
u�B
uB
uZB
u�B
u�B
uZB
u�B
u�B
v�B
v�B
v`B
v�B
w2B
v�B
wB
wLB
wLB
w�B
w�B
xB
xRB
xl1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�=B��B��B�7B��B�QB��B��B��B�1B��B�B��B�pB��B�FB�B�vB��B�9B��B��B�EB�nB��B��B��B�B��B�.B	[B	�B		RB	�B	4B	DB	�jB	�FB
�B
4�B
A�B
NVB
U�B
~BB
��B
�,B
�BB
�KB
ɺB
�[B
�B}B�B#B(�B5ZB:*B:�B:*BRoBsB��B�ZB��B�(BخB�pB�B�<B�}B��B��BרBּB�bB�rB��B��B�jB��B��B��BxlBpUB_�BBuB/5BEB
�BB
��B
��B
�eB
�WB
ǮB
�rB
�IB
��B
�,B
l=B
ZB
9XB
�B	��B	żB	�OB	��B	k�B	T�B	/�B	)�B	(�B	%zB	�B	}B	�B		�B	YB	%B	�B	�B	 4B	 �B	 B	B	 �B	,"B	 �B	�B		�B�cB�fB�-B��B�)B��B��B�B��B�.B	DB	oB	B	�B	&B	B	
#B	�B	SB	B	,�B	OB	n�B	u�B	�uB	�B	ǮB	�VB	�B	�B	�B	�sB	�}B	��B	یB	�B	�qB	�1B	רB	޸B	��B	�
B	�MB	��B	�B	�B
B
�B
�B
B	�B	��B
�B
B
pB
 �B
$&B
($B
-]B
2�B
7�B
G�B
K�B
J#B
OB
SB
RB
Q�B
RB
R�B
S&B
SB
S&B
S�B
R�B
RB
O�B
N�B
L�B
IB
F�B
F�B
MB
PB
N�B
MB
LdB
J�B
H�B
G_B
E�B
DgB
B�B
A�B
@iB
?�B
@ B
@�B
?cB
>wB
>�B
=�B
<�B
<B
<B
;B
9	B
9	B
9�B
:�B
9	B
8B
5tB
2�B
/�B
,�B
,=B
(�B
&�B
)DB
,"B
,B
+�B
*�B
*B
)*B
&�B
#�B
!HB
 B
�B
QB
�B
B
WB
=B
�B
�B
kB
7B
B
#B
]B
B
B
WB
�B
kB
�B
�B
B
�B
7B
7B
�B
kB
eB
KB
�B
�B
�B

B
�B
SB
FB
�B
JB
	�B
B
�B
B
[B
[B
B
MB
�B
�B
MB
�B
B
B
 �B	��B	�<B	�DB	�$B	��B	��B	�B	�fB	�B	�B	��B	��B	��B	��B	��B	�FB	�zB	�+B	��B	�LB	�LB	�B	��B	�FB	�%B	�%B	�B	�GB	�oB	��B	��B	�OB	�}B	�IB	�CB	�B	��B	�B	��B	��B	�IB	�B	��B	� B	� B	� B	� B	�B	�B	��B	��B	�B	�'B	�-B	�nB	�B	�nB	��B	�B	�B	��B	�B	��B	�B	�$B	��B	��B	��B	�(B	�VB	�B	��B	��B
 OB
�B
�B
'B
�B
�B
�B
AB
�B
�B
�B
�B
�B
�B
 B
 B
 �B
 �B
 �B
 B
B
�B
GB
[B
oB
 �B
 B	��B	��B
B
'B
�B
 iB	��B	�BB	�VB	�B	�dB	�B	�6B	��B	��B	��B	��B	�B	�<B	�VB
 �B
�B
 B
 �B
�B
'B
�B
3B
�B
�B

	B
	�B
	7B

=B

�B
xB
�B
B
�B
~B
dB
0B
�B
�B
�B
�B
�B
DB
^B

�B

rB

�B

�B

�B

�B

�B

�B
�B
B
0B
B
jB
<B
�B
jB
B
�B
�B
"B
�B
6B
�B
(B
�B
vB
�B
.B
�B
�B
�B
HB
�B
�B
bB
NB
�B
B
4B
NB
NB
TB
�B
,B
aB
uB
uB
B
�B
�B
�B
�B
�B
�B
�B
2B
�B
�B
�B
B
2B
�B
SB
SB
SB
�B
SB
B
?B
�B
?B
�B
�B
B
�B
sB
�B
EB
�B
�B
�B
�B
�B
kB
eB
�B
7B
B
�B
�B
QB
�B
�B
�B
�B
=B
�B
qB
�B
B
�B
�B
B
B
B
�B
VB
pB
�B
 'B
�B
 BB
 \B
 \B
 \B
 �B
!|B
"NB
"�B
"�B
"�B
#�B
$@B
$tB
$tB
$�B
%FB
%zB
%�B
&B
%�B
%�B
%�B
&B
%�B
&�B
&�B
&�B
'B
'B
'mB
'�B
'8B
(sB
'�B
(
B
($B
(�B
(�B
(XB
'�B
(�B
*�B
+B
+6B
+�B
,�B
,�B
-wB
-�B
-�B
-�B
.}B
.B
./B
.IB
.B
.B
.}B
.cB
./B
.cB
.B
.�B
-�B
.�B
.}B
.B
.�B
.�B
.�B
/ B
/�B
/�B
/�B
1�B
2�B
3B
2aB
2GB
2aB
2�B
2|B
2B
2-B
33B
3B
3B
4B
3�B
4B
4�B
4TB
3hB
2-B
1�B
1�B
0�B
0�B
1[B
0�B
0�B
0�B
1[B
0�B
0�B
1vB
2�B
3�B
2�B
33B
3�B
3�B
3�B
3�B
3�B
4TB
4�B
4�B
3�B
5tB
4�B
4nB
4�B
5�B
6�B
72B
9	B
8�B
9$B
9XB
:�B
;�B
<PB
<6B
<�B
<PB
<�B
=�B
=qB
>B
>�B
?�B
?cB
@B
?�B
?�B
@ B
?cB
@OB
@�B
A B
A B
@�B
AB
AB
@�B
@iB
@iB
@iB
@�B
@�B
A�B
A�B
BAB
A�B
AoB
A�B
BuB
C-B
C�B
D3B
D3B
E�B
EmB
E�B
E�B
E�B
E�B
E�B
E�B
E9B
E9B
EB
EB
E9B
E�B
E�B
FtB
F�B
F�B
F�B
F�B
F�B
F�B
F�B
FtB
F�B
G_B
G+B
F�B
G�B
HB
HB
G�B
HB
K�B
L�B
L�B
LJB
L�B
L�B
L~B
LJB
L�B
MB
M6B
MjB
NB
N�B
O�B
P.B
P�B
Q�B
QB
Q�B
RTB
QhB
R B
R B
R:B
R B
Q�B
R B
R�B
RTB
SuB
R�B
R�B
R�B
S�B
S�B
S�B
TB
T�B
T�B
U2B
UgB
UgB
U�B
T�B
UB
VB
V�B
V�B
V�B
WsB
W�B
W?B
W�B
X�B
X�B
YKB
YeB
Y�B
YB
Y�B
ZQB
ZB
[	B
[�B
[�B
[qB
Z�B
[�B
[�B
[qB
]B
\�B
\�B
\�B
\�B
\�B
\]B
]dB
\�B
]IB
^�B
_;B
_pB
_�B
`�B
`�B
`�B
a�B
a�B
a|B
a-B
a�B
bB
b�B
c B
cB
b�B
c�B
cTB
c�B
c�B
c�B
c�B
dZB
d&B
d�B
ezB
ezB
f2B
fB
e�B
e�B
ffB
gB
f�B
f�B
gB
g8B
gB
gmB
h
B
iDB
i�B
i�B
i�B
iyB
jeB
jKB
j�B
j0B
jKB
j0B
kkB
kB
k�B
k�B
k�B
k�B
lB
mB
l�B
lWB
m)B
m]B
mB
m]B
mB
m�B
m�B
m�B
n/B
m�B
n/B
m�B
n}B
ncB
nB
n�B
o B
oOB
oB
oB
oOB
oOB
o�B
o�B
pB
p�B
p�B
p�B
qB
p�B
qB
qAB
qAB
q[B
qvB
qAB
q�B
q�B
rB
rB
raB
q�B
raB
shB
s3B
r�B
s�B
s�B
sB
sMB
s�B
shB
s�B
tB
t�B
t�B
t�B
u?B
u�B
u�B
u�B
u?B
u%B
t�B
uZB
u�B
uB
uZB
u�B
u�B
uZB
u�B
u�B
v�B
v�B
v`B
v�B
w2B
v�B
wB
wLB
wLB
w�B
w�B
xB
xRB
xl1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104916  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173642  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173642  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173643                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023650  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023650  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                