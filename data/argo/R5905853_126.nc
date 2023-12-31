CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-07-26T12:40:09Z creation;2022-07-26T12:40:11Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220726124009  20220726125803  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ~A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @���Zt�1   @���;�G@,���"���c���Q�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�ffB�ffB���B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<33C>  C?�fCA�fCC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�C3DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @  @vff@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffB��fB��B��B�L�B��3B��B��3B��3B��3B��3B��3B��3B��3B��fB�� B�� B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	�4CٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC<�C=ٚC?� CA� CC� CEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCo�4Cq�4CsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD� DvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN��DOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�>fD�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�SA�SA��A��A��A��A��A�1A��A�kA�=A�A��A�~A��A� �A� �A�"hA�#nA�$tA�%zA�%�A�&�A�'RA�(XA�*�A�+kA�,qA�,qA�,�A�+6A�&LA�)�AȳhAǌJA�<jA�+A�I�A�:^A��_A��DA�dZA��JA��*A�{�A���A��VA�%A���A��8A�g�A��A�qAA�r|A�e,A�R�A��BA�2�A���A�p;A��*A�ܒA��-A�ǮA�7�A�s�A��;A�}�A�c�A�S&A�:*A�U�A�� A�p;A�[�A�HKA��UA�B'A�bA�C�A�A���A�x�A��A�9�A��A�$A��tA���A���A�GzA~��A}�XA{8�Ax~�Av&Ao��Aj�Afx�Ad��Aa�PAZdZAY�AVCAR��AOW�AK��AHk�AE�+AD_�ACK�AA$A>�aA<�A:!A9m]A8"�A5�,A4`�A1�2A1!�A/�	A.�$A.$tA,�-A*E�A)xlA('�A'�A%S&A%{A$��A$l"A#B�A!ɆA I�A�A(�A�A<6A�)A/A�NAe�A�aA\�A�)AN<A�.A�]AK�A~(A�A�|A4nA��A6�A�	A��A�AA�KA��AK�A�A��A1�A��A��A�rA(�Ay>A�A�[A8�A.IAϫA��A;A�VA0�Ay�AL�A��A$�A�aA�!A1�A
��A
��A
.�A	1�A��A]�A�!AjAOvA{A��AN�A�yA��AK^A�	A��Ai�A!�A�DAT�A_pA*0A(A iA�|A�A� A�AuA��AFtA ��A Dg@�P�@��h@�-�@��)@��$@�a|@�~(@�X@�c@�@@�tT@�[@�&�@��f@��`@��@�@��o@��@���@� �@�=@��@�K�@�D@��@锯@�0�@���@���@�A�@���@��6@�`B@�8@�-w@�#�@�R@�8�@��.@㹌@�X�@�-�@��@�I�@ߌ~@ޚ@�_@�)_@ܵ�@�$@�L�@�`�@�s@�J�@�`�@��@׋�@�L�@��@���@�c�@�u�@�4n@�@Ӓ:@�Q@���@ѿH@�e,@Ѻ^@ў�@�W?@��]@а!@Ї�@�M@�q@Ь@Ъe@�;�@��D@�o @��"@���@μj@ΦL@�r�@͜@̛�@��Z@��@��@ɸ�@�dZ@�!-@�M�@��#@�33@��@Ɵ�@��@Ų-@�c�@�YK@��W@é*@�;d@�>B@���@�l�@�A @�o@��}@�^5@��T@�o�@��@��B@�9X@���@�Q�@��[@���@�c�@�+k@��V@�?}@�S@�?@���@�h
@�(�@�4@��r@���@�s�@�C@���@���@���@�]d@�:*@���@�=�@�#�@�%@��]@�<�@�c@�Dg@���@�{@�b�@�%@��@���@��@�6@��V@�;d@�-w@���@�h�@�7@���@�$t@��)@�I�@���@���@�}�@�T�@�?}@�2a@�'�@�+@��@�ff@�*�@��&@���@�W?@�@��?@��D@���@�|@�k�@�H�@�ی@�s�@�$�@�G@��@�=@��m@��.@��@�s�@�]d@�9X@�G@��w@���@��S@�&�@��@��@���@�w�@�oi@�H@��@�ԕ@��k@�S�@��@���@���@�K^@��@���@���@�Mj@�@���@�$@��)@��#@��-@�x�@�+@��e@�YK@�e@��@���@�4�@���@��@�GE@���@���@�N<@��@���@�{�@�7�@��@���@�#�@��@���@�Q@���@���@��@���@�F@��@��,@��@�l�@�)�@��w@�_p@�*0@�%@���@�y>@�	@���@���@��-@�N<@���@���@�J@��o@��T@��Q@��@�w2@�9�@��]@�n�@�($@��@�w2@�U�@��@���@�I�@��;@���@�<6@��|@��z@��o@�8�@��Q@���@�s�@�\)@��@���@�D�@��@��o@��X@�F@��`@���@�q�@�5?@��@��a@���@�c�@���@�v�@�R�@��@���@���@���@�o�@�J#@�V@���@��@��]@��@�q@�x@���@��=@�l�@�Mj@�#�@�Ĝ@���@���@�&�@)_@~��@~Z�@~B[@~&�@}�Z@}�N@}�@}O�@};@|�@{ƨ@{Mj@{o@z��@z��@zO@yx�@yJ�@x��@x�u@x6@w��@wX�@v��@v� @vV@v+k@u�9@us�@u?}@u�@t�)@tN�@s�4@r�}@r0U@q��@qk�@p�u@o��@os@n�M@n�+@n@�@n@m��@mw2@mIR@m�@l�O@loi@l:�@kݘ@j��@jO@i�M@hی@hq@g��@g]�@g(@f=q@e�~@e*0@d�?@d��@dw�@d*�@c�&@c�@b�h@bQ@a��@a}�@azx@ae,@a&�@`[�@_�@_��@^�c@^�A@^{@]��@]%F@\��@\w�@\Q�@[��@[��@\G@[��@[iD@[$t@[
=@Z�R@Z�@YL�@X��@X�?@X?�@W��@WE9@V��@V��@V^5@V6�@VJ@U�H@Uj@T��@T!@Sb�@Rp;@R0U@R�@Q�)@Q��@Q��@Q�h@Q+�@P,=@N�@N)�@M��@M�N@M�@M�'@Mj@M#�@L�/@L��@Lr�@L(�@K�a@Kv`@KS@JkQ@J	@I��@I�@I��@I@H��@H�@H�D@HPH@G�]@G��@GF�@GC@F��@F�F@Fl�@FO@E�@E�d@E[W@E@DɆ@D��@D��@Dl"@C��@C��@C�4@CC�@C@B�+@BW�@B�@A��@A��@A[W@A4@@��@@bN@@@?��@>{�@=s�@<��@<[�@;��@;C�@:�@:{�@:!�@9�X@8��@8��@8�@8��@8K^@8�@7�@7�@6�c@6� @6L0@5��@5�=@5o @5+�@5�@4��@4�o@4@3�+@3��@3_p@3)_@2��@2Z�@24@1�@1��@1Q�@1�@0�@0��@0��@0��@0*�@/�+@/��@/�F@/j�@/�@.��@.Ta@.5?@-�n@-7L@-�@,ی@,�$@,b@+~�@+K�@+8@*��@*�}@*J@)��@)��@)|@)�@(�v@(�$@(S�@(@'�@'�W@'��@'�@'|�@'t�@'O@'�@&�<@&)�@%��@%��@%��@$�@$�O@$U2@$�@#��@#O@"�c@"s�@"c @"6�@"@!��@!��@!w2@!B�@ ��@ �[@ �O@ ��@ �D@ N�@�@��@��@j�@E9@!-@�"@҉@�@��@��@d�@�.@�)@�d@��@��@��@V�@7�@�@�[@�@~�@E9@!-@�2@��@��@5?@�d@�n@�h@Vm@4@@��@�@�@�D@m�@6@�+@�}@j�@H�@&@�]@�h@kQ@L0@�@��@��@zx@hs@Vm@=�@�@;@�v@�9@r�@]d@6@��@��@��@qv@E9@�@�@͟@�@s�@_�@@�@{@_@��@�@�H@x�@e,@Vm@Q�@8�@�@ѷ@z�@PH@!@�@�@�@�@l�@H�@!-@��@p;@W�@B[@e@�@��@j@G�@4@�@�5@Ɇ@��@�e@�z@��@Q�@�@� @�V@t�@U�@A�@/�@
��@
��@
��@
M�@
�@
�@	�.@	��@	��@	�t@	��@	�"@	m]@	L�@	7L@�@�j@tT@9X@*�@@��@�@ƨ@��@�f@t�@J#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�SA�SA��A��A��A��A��A�1A��A�kA�=A�A��A�~A��A� �A� �A�"hA�#nA�$tA�%zA�%�A�&�A�'RA�(XA�*�A�+kA�,qA�,qA�,�A�+6A�&LA�)�AȳhAǌJA�<jA�+A�I�A�:^A��_A��DA�dZA��JA��*A�{�A���A��VA�%A���A��8A�g�A��A�qAA�r|A�e,A�R�A��BA�2�A���A�p;A��*A�ܒA��-A�ǮA�7�A�s�A��;A�}�A�c�A�S&A�:*A�U�A�� A�p;A�[�A�HKA��UA�B'A�bA�C�A�A���A�x�A��A�9�A��A�$A��tA���A���A�GzA~��A}�XA{8�Ax~�Av&Ao��Aj�Afx�Ad��Aa�PAZdZAY�AVCAR��AOW�AK��AHk�AE�+AD_�ACK�AA$A>�aA<�A:!A9m]A8"�A5�,A4`�A1�2A1!�A/�	A.�$A.$tA,�-A*E�A)xlA('�A'�A%S&A%{A$��A$l"A#B�A!ɆA I�A�A(�A�A<6A�)A/A�NAe�A�aA\�A�)AN<A�.A�]AK�A~(A�A�|A4nA��A6�A�	A��A�AA�KA��AK�A�A��A1�A��A��A�rA(�Ay>A�A�[A8�A.IAϫA��A;A�VA0�Ay�AL�A��A$�A�aA�!A1�A
��A
��A
.�A	1�A��A]�A�!AjAOvA{A��AN�A�yA��AK^A�	A��Ai�A!�A�DAT�A_pA*0A(A iA�|A�A� A�AuA��AFtA ��A Dg@�P�@��h@�-�@��)@��$@�a|@�~(@�X@�c@�@@�tT@�[@�&�@��f@��`@��@�@��o@��@���@� �@�=@��@�K�@�D@��@锯@�0�@���@���@�A�@���@��6@�`B@�8@�-w@�#�@�R@�8�@��.@㹌@�X�@�-�@��@�I�@ߌ~@ޚ@�_@�)_@ܵ�@�$@�L�@�`�@�s@�J�@�`�@��@׋�@�L�@��@���@�c�@�u�@�4n@�@Ӓ:@�Q@���@ѿH@�e,@Ѻ^@ў�@�W?@��]@а!@Ї�@�M@�q@Ь@Ъe@�;�@��D@�o @��"@���@μj@ΦL@�r�@͜@̛�@��Z@��@��@ɸ�@�dZ@�!-@�M�@��#@�33@��@Ɵ�@��@Ų-@�c�@�YK@��W@é*@�;d@�>B@���@�l�@�A @�o@��}@�^5@��T@�o�@��@��B@�9X@���@�Q�@��[@���@�c�@�+k@��V@�?}@�S@�?@���@�h
@�(�@�4@��r@���@�s�@�C@���@���@���@�]d@�:*@���@�=�@�#�@�%@��]@�<�@�c@�Dg@���@�{@�b�@�%@��@���@��@�6@��V@�;d@�-w@���@�h�@�7@���@�$t@��)@�I�@���@���@�}�@�T�@�?}@�2a@�'�@�+@��@�ff@�*�@��&@���@�W?@�@��?@��D@���@�|@�k�@�H�@�ی@�s�@�$�@�G@��@�=@��m@��.@��@�s�@�]d@�9X@�G@��w@���@��S@�&�@��@��@���@�w�@�oi@�H@��@�ԕ@��k@�S�@��@���@���@�K^@��@���@���@�Mj@�@���@�$@��)@��#@��-@�x�@�+@��e@�YK@�e@��@���@�4�@���@��@�GE@���@���@�N<@��@���@�{�@�7�@��@���@�#�@��@���@�Q@���@���@��@���@�F@��@��,@��@�l�@�)�@��w@�_p@�*0@�%@���@�y>@�	@���@���@��-@�N<@���@���@�J@��o@��T@��Q@��@�w2@�9�@��]@�n�@�($@��@�w2@�U�@��@���@�I�@��;@���@�<6@��|@��z@��o@�8�@��Q@���@�s�@�\)@��@���@�D�@��@��o@��X@�F@��`@���@�q�@�5?@��@��a@���@�c�@���@�v�@�R�@��@���@���@���@�o�@�J#@�V@���@��@��]@��@�q@�x@���@��=@�l�@�Mj@�#�@�Ĝ@���@���@�&�@)_@~��@~Z�@~B[@~&�@}�Z@}�N@}�@}O�@};@|�@{ƨ@{Mj@{o@z��@z��@zO@yx�@yJ�@x��@x�u@x6@w��@wX�@v��@v� @vV@v+k@u�9@us�@u?}@u�@t�)@tN�@s�4@r�}@r0U@q��@qk�@p�u@o��@os@n�M@n�+@n@�@n@m��@mw2@mIR@m�@l�O@loi@l:�@kݘ@j��@jO@i�M@hی@hq@g��@g]�@g(@f=q@e�~@e*0@d�?@d��@dw�@d*�@c�&@c�@b�h@bQ@a��@a}�@azx@ae,@a&�@`[�@_�@_��@^�c@^�A@^{@]��@]%F@\��@\w�@\Q�@[��@[��@\G@[��@[iD@[$t@[
=@Z�R@Z�@YL�@X��@X�?@X?�@W��@WE9@V��@V��@V^5@V6�@VJ@U�H@Uj@T��@T!@Sb�@Rp;@R0U@R�@Q�)@Q��@Q��@Q�h@Q+�@P,=@N�@N)�@M��@M�N@M�@M�'@Mj@M#�@L�/@L��@Lr�@L(�@K�a@Kv`@KS@JkQ@J	@I��@I�@I��@I@H��@H�@H�D@HPH@G�]@G��@GF�@GC@F��@F�F@Fl�@FO@E�@E�d@E[W@E@DɆ@D��@D��@Dl"@C��@C��@C�4@CC�@C@B�+@BW�@B�@A��@A��@A[W@A4@@��@@bN@@@?��@>{�@=s�@<��@<[�@;��@;C�@:�@:{�@:!�@9�X@8��@8��@8�@8��@8K^@8�@7�@7�@6�c@6� @6L0@5��@5�=@5o @5+�@5�@4��@4�o@4@3�+@3��@3_p@3)_@2��@2Z�@24@1�@1��@1Q�@1�@0�@0��@0��@0��@0*�@/�+@/��@/�F@/j�@/�@.��@.Ta@.5?@-�n@-7L@-�@,ی@,�$@,b@+~�@+K�@+8@*��@*�}@*J@)��@)��@)|@)�@(�v@(�$@(S�@(@'�@'�W@'��@'�@'|�@'t�@'O@'�@&�<@&)�@%��@%��@%��@$�@$�O@$U2@$�@#��@#O@"�c@"s�@"c @"6�@"@!��@!��@!w2@!B�@ ��@ �[@ �O@ ��@ �D@ N�@�@��@��@j�@E9@!-@�"@҉@�@��@��@d�@�.@�)@�d@��@��@��@V�@7�@�@�[@�@~�@E9@!-@�2@��@��@5?@�d@�n@�h@Vm@4@@��@�@�@�D@m�@6@�+@�}@j�@H�@&@�]@�h@kQ@L0@�@��@��@zx@hs@Vm@=�@�@;@�v@�9@r�@]d@6@��@��@��@qv@E9@�@�@͟@�@s�@_�@@�@{@_@��@�@�H@x�@e,@Vm@Q�@8�@�@ѷ@z�@PH@!@�@�@�@�@l�@H�@!-@��@p;@W�@B[@e@�@��@j@G�@4@�@�5@Ɇ@��@�e@�z@��@Q�@�@� @�V@t�@U�@A�@/�@
��@
��@
��@
M�@
�@
�@	�.@	��@	��@	�t@	��@	�"@	m]@	L�@	7L@�@�j@tT@9X@*�@@��@�@ƨ@��@�f@t�@J#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B
żB
żB
��B
żB
��B
żB
żB
��B
��B
��B
��B
żB
żB
��B
��B
��B
��B
��B
��B
�B
�B
�%B
�B
�B
�B
�%B
�B
�?B
�?B
�tB
ƨB
ŢB�RB��B��B9B+kBN�BvB��B�B�WB��B��B��B�jB�BB�\B�:B�
B��B��B��B�^B��Bu%BoiBj�BcBS�BMPB;�B4B.cB'�B �BB�B	RB�VB��B�FBԕB��B�eB�0Bm�BK^B/B�B
��B
��B
�?B
�sB
��B
f�B
K)B
+�B
1B
�B
�B	��B	�0B	�9B	�B	��B	�fB	pB	g�B	Z�B	DMB	;�B	2|B	)*B	�B	�B		RB��B�%B�aB��B�@B�TB�B�BߤB�
B׍B�B�@B�NB�B�4B�B��B�9B	�B	�B	�B	 BB	&�B	*�B	*B	$�B	$�B	*�B	>�B	DMB	NVB	YeB	_B	c B	l�B	uZB	t�B	t�B	utB	}B	}�B	�_B	��B	��B	��B	�aB	��B	�@B	�}B	ѝB	�B	�NB	�B	�_B	�B	�)B	�B	�qB	�B	�B	�6B	��B	��B	�CB	�B	�cB	��B	�B	�6B	�BB	��B	�qB	�0B	��B	�B	��B	�B	�MB	�B	�B	�B	��B	��B	��B	��B	�fB	�zB	�2B	��B	�`B	�B	�:B	��B	� B	��B	�|B	��B	�HB	�vB	�TB	��B	�$B	��B	��B	�B	�B	�mB	�B	�B	�ZB	�B	��B	�B	��B	��B	ڠB	�
B	ңB	҉B	�{B	ңB	�@B	��B	ԯB	��B	�&B	��B	��B	��B	�?B	��B	ּB	յB	خB	�yB	ևB	өB	�B	��B	��B	�2B	՛B	�$B	�eB	�=B	�=B	�qB	�#B	�WB	��B	ܒB	��B	�IB	�B	�VB	�B	�]B	��B	�kB	�=B	�dB	޸B	�vB	�bB	�\B	ބB	ܒB	��B	��B	ݲB	�/B	�CB	ܒB	�-B	��B	�B	�\B	�`B	�FB	�hB	�zB	�B	��B	�IB	�}B	�5B	��B	��B	�wB	�CB	�'B	�TB	�tB	�LB	�XB	��B	��B	��B	��B	��B	�$B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�fB	�B	�fB	�fB	�LB	�B	��B	�B	��B	��B	�B	�LB	�2B	��B	��B	�B	�8B	�8B	�B	�8B	�8B	�B	��B	�	B	�XB	�^B	�B	��B	��B	��B	��B	�xB	�B	��B	�JB	��B	��B	��B	�B	�6B	��B	��B	�"B	��B	��B	��B	�]B	��B	��B	��B	��B	��B	�}B	��B
 B
B
B
 �B
 �B
 B
�B
AB
�B
'B
�B
�B
[B
uB
�B
aB
�B
�B
B
mB
�B
�B
B
tB
�B
�B
_B
KB
�B
�B
�B
B
�B
�B
�B
	�B
	lB
	7B
�B
1B
�B
	B
�B
�B
	RB

�B

�B
)B
)B
DB
DB
�B
�B
�B
�B
dB
�B
�B
6B
B
B
�B
�B
�B
VB
�B
vB
vB
�B
.B
HB
bB
bB
�B
�B
hB
B
 B
�B
�B
�B
 B
�B
�B
&B
@B
[B
�B
,B
FB
�B
�B
�B
2B
B
SB
�B
�B
$B
�B
�B
+B
�B
1B
�B
�B
�B
7B
kB
�B
=B
	B
�B
)B
�B
�B
5B
B
�B
�B
B
�B
�B
�B
B
pB
 'B
 �B
 �B
 �B
 �B
 �B
!bB
!�B
"NB
"hB
"B
"hB
#:B
#TB
#�B
$@B
$�B
%B
%zB
%�B
&2B
&�B
&�B
&�B
'RB
'�B
'�B
(
B
($B
($B
(�B
(sB
(XB
(sB
)DB
)�B
*�B
*�B
+6B
+QB
+�B
,"B
,=B
-B
-CB
-)B
-�B
-�B
-�B
-�B
./B
.�B
.�B
.�B
.�B
.�B
/ B
/5B
/iB
/iB
/OB
/�B
/B
.�B
/iB
/OB
0�B
1�B
2B
2GB
2�B
2�B
3B
3�B
3�B
4B
4B
4TB
4�B
5tB
5�B
5�B
5�B
5�B
6`B
6�B
7LB
8RB
88B
8�B
8�B
8�B
9	B
9$B
9>B
9rB
9�B
:B
:DB
:*B
:DB
:B
:*B
:�B
:�B
:�B
:�B
;dB
<6B
;�B
<PB
<jB
<jB
<�B
<�B
<�B
<jB
<jB
<PB
<PB
<jB
<6B
;�B
<jB
;�B
<B
<B
<PB
<PB
<�B
=�B
>�B
>�B
?B
?.B
?�B
@OB
@B
@B
@B
@B
@iB
@�B
B�B
C�B
C�B
DB
C�B
C�B
C{B
C�B
DB
C�B
DB
D3B
DgB
D�B
EB
GB
I�B
J#B
JXB
J�B
J�B
KDB
K�B
LJB
L~B
L�B
MB
MPB
MjB
M�B
M�B
M�B
NB
M�B
N<B
NpB
OB
O�B
PB
P�B
P�B
P�B
P}B
P}B
P�B
P}B
P�B
QNB
R B
R:B
RTB
RoB
R�B
R�B
S@B
S[B
S�B
TB
T,B
TFB
TaB
TaB
TaB
T�B
T�B
UB
T�B
UMB
V9B
V�B
V�B
V�B
W$B
W?B
W�B
W�B
W�B
XEB
XEB
XEB
X�B
X�B
X�B
YB
Y1B
YB
Y�B
YB
YB
Y�B
ZB
ZQB
ZQB
ZkB
Z�B
Z�B
[	B
[WB
[qB
[�B
[�B
[�B
\]B
\]B
\]B
[�B
[WB
[qB
[qB
\CB
\)B
\]B
\�B
]B
]dB
^B
^B
^5B
^jB
_B
_;B
_!B
_pB
_�B
`B
`BB
`�B
a-B
a�B
a�B
a�B
bhB
b�B
cB
c�B
d&B
d�B
d�B
dtB
dtB
d�B
d�B
e`B
ezB
e�B
e�B
fLB
f�B
f�B
g8B
gRB
g�B
g�B
h>B
h$B
h�B
h�B
h�B
iDB
h�B
h�B
h�B
i�B
i�B
iyB
iDB
i*B
iB
iDB
i�B
i_B
iDB
i�B
iyB
i�B
i�B
jeB
jeB
jKB
jeB
jeB
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
l"B
l�B
lqB
l�B
mCB
m)B
m]B
m]B
mwB
m�B
n�B
n�B
oiB
o B
oB
oB
oB
o5B
o�B
o�B
p!B
pUB
p;B
pUB
p;B
p�B
p�B
p�B
p�B
p�B
q[B
qAB
q[B
q�B
r-B
r|B
r�B
r�B
sB
shB
s�B
s�B
tB
tB
t9B
tnB
t�B
t�B
utB
u�B
u�B
u�B
vFB
vFB
v�B
v�B
v�B
v�B
wB
wfB
w�B
w�B
w�B
w�B
w�B
xRB
x�B
y	B
y$B
yrB
y�B
y�B
zB
zB
z*B
z^B
zxB
z�B
z�B
z�B
{0B
{0B
{JB
{B
{�B
|B
|B
|PB
|�B
|�B
|�B
|jB
|�B
|�B
}<B
}VB
}�B
}�B
}�B
}�B
~B
~B
~B
}�B
~B
~]B
~wB
~�B
~�B
~�B
~�B
.B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
�B
�UB
��B
��B
��B
��B
�'B
�AB
�[B
�[B
�AB
�[B
��B
��B
�B
�{B
�{B
��B
��B
��B
�B
�3B
�gB
��B
��B
��B
��B
��B
��B
�B
�B
�SB
�mB
��B
��B
�%B
��B
�tB
��B
�tB
��B
��B
��B
�B
�EB
�EB
�EB
�z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B
żB
żB
��B
żB
��B
żB
żB
��B
��B
��B
��B
żB
żB
��B
��B
��B
��B
��B
��B
�B
�B
�%B
�B
�B
�B
�%B
�B
�?B
�?B
�tB
ƨB
ŢB�RB��B��B9B+kBN�BvB��B�B�WB��B��B��B�jB�BB�\B�:B�
B��B��B��B�^B��Bu%BoiBj�BcBS�BMPB;�B4B.cB'�B �BB�B	RB�VB��B�FBԕB��B�eB�0Bm�BK^B/B�B
��B
��B
�?B
�sB
��B
f�B
K)B
+�B
1B
�B
�B	��B	�0B	�9B	�B	��B	�fB	pB	g�B	Z�B	DMB	;�B	2|B	)*B	�B	�B		RB��B�%B�aB��B�@B�TB�B�BߤB�
B׍B�B�@B�NB�B�4B�B��B�9B	�B	�B	�B	 BB	&�B	*�B	*B	$�B	$�B	*�B	>�B	DMB	NVB	YeB	_B	c B	l�B	uZB	t�B	t�B	utB	}B	}�B	�_B	��B	��B	��B	�aB	��B	�@B	�}B	ѝB	�B	�NB	�B	�_B	�B	�)B	�B	�qB	�B	�B	�6B	��B	��B	�CB	�B	�cB	��B	�B	�6B	�BB	��B	�qB	�0B	��B	�B	��B	�B	�MB	�B	�B	�B	��B	��B	��B	��B	�fB	�zB	�2B	��B	�`B	�B	�:B	��B	� B	��B	�|B	��B	�HB	�vB	�TB	��B	�$B	��B	��B	�B	�B	�mB	�B	�B	�ZB	�B	��B	�B	��B	��B	ڠB	�
B	ңB	҉B	�{B	ңB	�@B	��B	ԯB	��B	�&B	��B	��B	��B	�?B	��B	ּB	յB	خB	�yB	ևB	өB	�B	��B	��B	�2B	՛B	�$B	�eB	�=B	�=B	�qB	�#B	�WB	��B	ܒB	��B	�IB	�B	�VB	�B	�]B	��B	�kB	�=B	�dB	޸B	�vB	�bB	�\B	ބB	ܒB	��B	��B	ݲB	�/B	�CB	ܒB	�-B	��B	�B	�\B	�`B	�FB	�hB	�zB	�B	��B	�IB	�}B	�5B	��B	��B	�wB	�CB	�'B	�TB	�tB	�LB	�XB	��B	��B	��B	��B	��B	�$B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�fB	�B	�fB	�fB	�LB	�B	��B	�B	��B	��B	�B	�LB	�2B	��B	��B	�B	�8B	�8B	�B	�8B	�8B	�B	��B	�	B	�XB	�^B	�B	��B	��B	��B	��B	�xB	�B	��B	�JB	��B	��B	��B	�B	�6B	��B	��B	�"B	��B	��B	��B	�]B	��B	��B	��B	��B	��B	�}B	��B
 B
B
B
 �B
 �B
 B
�B
AB
�B
'B
�B
�B
[B
uB
�B
aB
�B
�B
B
mB
�B
�B
B
tB
�B
�B
_B
KB
�B
�B
�B
B
�B
�B
�B
	�B
	lB
	7B
�B
1B
�B
	B
�B
�B
	RB

�B

�B
)B
)B
DB
DB
�B
�B
�B
�B
dB
�B
�B
6B
B
B
�B
�B
�B
VB
�B
vB
vB
�B
.B
HB
bB
bB
�B
�B
hB
B
 B
�B
�B
�B
 B
�B
�B
&B
@B
[B
�B
,B
FB
�B
�B
�B
2B
B
SB
�B
�B
$B
�B
�B
+B
�B
1B
�B
�B
�B
7B
kB
�B
=B
	B
�B
)B
�B
�B
5B
B
�B
�B
B
�B
�B
�B
B
pB
 'B
 �B
 �B
 �B
 �B
 �B
!bB
!�B
"NB
"hB
"B
"hB
#:B
#TB
#�B
$@B
$�B
%B
%zB
%�B
&2B
&�B
&�B
&�B
'RB
'�B
'�B
(
B
($B
($B
(�B
(sB
(XB
(sB
)DB
)�B
*�B
*�B
+6B
+QB
+�B
,"B
,=B
-B
-CB
-)B
-�B
-�B
-�B
-�B
./B
.�B
.�B
.�B
.�B
.�B
/ B
/5B
/iB
/iB
/OB
/�B
/B
.�B
/iB
/OB
0�B
1�B
2B
2GB
2�B
2�B
3B
3�B
3�B
4B
4B
4TB
4�B
5tB
5�B
5�B
5�B
5�B
6`B
6�B
7LB
8RB
88B
8�B
8�B
8�B
9	B
9$B
9>B
9rB
9�B
:B
:DB
:*B
:DB
:B
:*B
:�B
:�B
:�B
:�B
;dB
<6B
;�B
<PB
<jB
<jB
<�B
<�B
<�B
<jB
<jB
<PB
<PB
<jB
<6B
;�B
<jB
;�B
<B
<B
<PB
<PB
<�B
=�B
>�B
>�B
?B
?.B
?�B
@OB
@B
@B
@B
@B
@iB
@�B
B�B
C�B
C�B
DB
C�B
C�B
C{B
C�B
DB
C�B
DB
D3B
DgB
D�B
EB
GB
I�B
J#B
JXB
J�B
J�B
KDB
K�B
LJB
L~B
L�B
MB
MPB
MjB
M�B
M�B
M�B
NB
M�B
N<B
NpB
OB
O�B
PB
P�B
P�B
P�B
P}B
P}B
P�B
P}B
P�B
QNB
R B
R:B
RTB
RoB
R�B
R�B
S@B
S[B
S�B
TB
T,B
TFB
TaB
TaB
TaB
T�B
T�B
UB
T�B
UMB
V9B
V�B
V�B
V�B
W$B
W?B
W�B
W�B
W�B
XEB
XEB
XEB
X�B
X�B
X�B
YB
Y1B
YB
Y�B
YB
YB
Y�B
ZB
ZQB
ZQB
ZkB
Z�B
Z�B
[	B
[WB
[qB
[�B
[�B
[�B
\]B
\]B
\]B
[�B
[WB
[qB
[qB
\CB
\)B
\]B
\�B
]B
]dB
^B
^B
^5B
^jB
_B
_;B
_!B
_pB
_�B
`B
`BB
`�B
a-B
a�B
a�B
a�B
bhB
b�B
cB
c�B
d&B
d�B
d�B
dtB
dtB
d�B
d�B
e`B
ezB
e�B
e�B
fLB
f�B
f�B
g8B
gRB
g�B
g�B
h>B
h$B
h�B
h�B
h�B
iDB
h�B
h�B
h�B
i�B
i�B
iyB
iDB
i*B
iB
iDB
i�B
i_B
iDB
i�B
iyB
i�B
i�B
jeB
jeB
jKB
jeB
jeB
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
l"B
l�B
lqB
l�B
mCB
m)B
m]B
m]B
mwB
m�B
n�B
n�B
oiB
o B
oB
oB
oB
o5B
o�B
o�B
p!B
pUB
p;B
pUB
p;B
p�B
p�B
p�B
p�B
p�B
q[B
qAB
q[B
q�B
r-B
r|B
r�B
r�B
sB
shB
s�B
s�B
tB
tB
t9B
tnB
t�B
t�B
utB
u�B
u�B
u�B
vFB
vFB
v�B
v�B
v�B
v�B
wB
wfB
w�B
w�B
w�B
w�B
w�B
xRB
x�B
y	B
y$B
yrB
y�B
y�B
zB
zB
z*B
z^B
zxB
z�B
z�B
z�B
{0B
{0B
{JB
{B
{�B
|B
|B
|PB
|�B
|�B
|�B
|jB
|�B
|�B
}<B
}VB
}�B
}�B
}�B
}�B
~B
~B
~B
}�B
~B
~]B
~wB
~�B
~�B
~�B
~�B
.B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
�B
�UB
��B
��B
��B
��B
�'B
�AB
�[B
�[B
�AB
�[B
��B
��B
�B
�{B
�{B
��B
��B
��B
�B
�3B
�gB
��B
��B
��B
��B
��B
��B
�B
�B
�SB
�mB
��B
��B
�%B
��B
�tB
��B
�tB
��B
��B
��B
�B
�EB
�EB
�EB
�z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220726124001  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220726124009  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220726124011  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220726124011                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220726214016  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220726214016  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220726125803                      G�O�G�O�G�O�                