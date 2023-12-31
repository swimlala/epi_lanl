CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:38:22Z creation;2022-06-04T17:38:23Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604173822  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               WA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�~�r(1   @�~���a�@/bM����c|�n��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�33B���B���C   C�fC  C  C  C
  C  C  C  C  C�C�C�fC  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD�CE�fCG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr33Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{�fD|  D|y�D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D�|�D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @ff@vff@�33@�33A��A=��A]��A}��A�  A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBG��BOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��B�� B��3B�L�B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B��B� B�3B��fB�L�B�� B��3C� CٚCٚCٚC	ٚCٚCٚCٚCٚC�4C�4C� CٚCٚCٚCٚC!ٚC#ٚC%� C'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCA�4CC�4CE� CG� CIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCo�4Cr�Cs� CuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD|�D�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{|�D{�fD|p D|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�~fD��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�x D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��fD�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�4�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AΦAα�AζFAηLAε�Aε�Aε?AίOAβaAΰ�AζzAΠ'A�x�A�:�A�SA͞�A͈1A�.A�z�A�u�A�qA�o�A�o�A�m�A�h>A�a|A�_pA�_;A�]�A�[�A�V�A�FA�#�A��A��[A�jA�p�A�1[A�A�A�/OA�XA��aA�y>A�~A�jA��'A��A�ZQA��0A��#A��	A��pA�HKA�MA�'RA�=A�"hA���A���A��iA�PHA�J�A��A��A���A�P}A��$A��A��A�!�A�4A�FA�#:A��A�"�A���A�VA���A���A�s�A���A�?�A�E�A�*0A���A�H�A���A�v�A�B�A�ƨA�?HA��RA�	�A��+A���A�FtA�bA�Y�A��A}�wAy^5As��AoE9Am��Al��Aj%�AhtTAf�.Ad	A`�A_�A^��A\��AW��AV#:AT��AQ�AK��AF�ACv`A@o�A?$�A=CA:�HA9�WA9�~A9HA9*0A9�A8��A7�VA6��A6o�A6�KA7z�A6;A5�A2-A.3�A,�$A+�A)�A)(�A(Q�A'�A&�:A&�A%��A%�nA%�EA&S&A&DgA&7LA%n/A$y�A#�A#�SA#aA"�SA#r�A#��A"�A!ĜA!��A!��A��A�xA(�A�fA8�A/�A�+A�zA�A��A�A��A�Ai�A6�AE�AH�A9XA��A8�A�A%FAe�A�EA]dA�A��Aa|A�A�qA�A��A=�A�vAVA
ݘA
�A
�KA
�A	�gA	o A�fA��AԕA^5A�A�qAU�A{A��A1'A�A�A��A��A;�A�A�IAa�A�'A=�ASA ԕA �A A @���@�'�@���@�u@�c@�ԕ@�O�@���@�Ta@�J@���@��@�n�@��@�(@�Ɇ@��h@�o@�y�@�[�@�D@��.@�j@���@�'�@�S�@�e@��@��z@�[@�m]@�Ɇ@�@��@�G@��@���@��@��f@�A@��@�U2@�S@�-�@�@�P@�S@�ݘ@唯@��8@�!@��@�/@�H@�e�@�.I@�v`@ⷀ@�&@ߜ@ߧ�@�9�@���@��@�e@�0U@�zx@��B@�H�@�  @���@��@�!@ق�@�Q�@�@�ں@�tT@ׅ�@�33@��@��|@�{�@չ�@�]�@���@�$@ӏ�@��@��@�g�@Ч�@�n/@���@�q�@�8�@�@��Z@�X�@��	@���@��v@��@�@�xl@�˒@��@�*�@ƌ@��T@žw@ōP@�S�@Ƶ�@Ƈ+@�N<@���@�@Ė�@�8�@Ã{@+@�[�@�!�@�U�@��@�w�@�YK@��@�u@�c@�,�@�,�@�0�@�Vm@�S�@�a@�C@���@���@��>@�L�@���@�B�@��#@�n/@��h@�m�@��@���@��@���@�(�@���@���@��@��@���@��@��@��F@��@���@��@���@�%�@��k@�/@���@��@���@��@��4@��@��[@�z�@�0U@��A@���@��7@�dZ@�Z�@���@��@@�o�@�Vm@�B�@�o@���@���@���@�F@��@���@�+k@��-@�`B@���@��c@���@�l"@�1'@��q@���@�S&@��@��,@���@��@�#:@��k@���@�,�@���@�.�@���@���@�j@���@��@���@��@��o@�j�@��@��0@��a@�%@�}V@��h@�A�@��3@�S&@�*0@���@�5�@�	l@��@�H�@�O@�<�@�/�@� �@��@��V@�@O@���@��,@�v�@�(�@���@��C@�k�@��@�g8@�M�@�7�@��@���@��S@�n/@�'�@�%@���@��<@���@�Ta@�($@�  @��@��q@�Z�@�1�@�@@��8@��X@���@�ff@�bN@�J�@���@��}@��H@���@�t�@��@���@�]d@��@��}@��F@�zx@�F@�ߤ@�z�@�+k@�O@���@���@�n/@�&�@��@�l�@�J�@�#:@��@���@���@�C�@���@�bN@��@���@���@�j�@�=@���@��1@�Ov@��z@�F@��e@�J�@�?�@��&@��@�X�@���@��1@�p;@�9X@�4@��]@���@���@��@�@��[@�@�@�7@�4@��]@���@�˒@��@�qv@�)_@���@�xl@�6@��D@��k@��8@�v�@�@O@U�@(@~p;@}�7@}7L@}(�@|�@|��@|�_@|�@{ƨ@{n/@{�@zq�@y��@y�@y�@x��@xq@w�q@w.I@vȴ@v�@vh
@v)�@up�@t�j@tM@s�@r��@r�1@rZ�@q��@q#�@q%@p�f@pɆ@p�$@p~(@pj@p[�@p>B@p�@o��@o��@oP�@o@n�b@nE�@m�@m��@m&�@l�)@l�$@lm�@lZ@l  @k��@k33@j��@i�j@i�M@i(�@h�o@h:�@hM@g��@g�[@g�{@g'�@f��@f)�@e�H@e��@e\�@eX@e�@d�/@d~(@d@cƨ@c�V@cv`@c1�@b�@b��@bQ@b5?@aԕ@a��@a\�@a(�@`�@`�p@`��@`7�@_��@_e�@_@^i�@]�'@]�@\��@\��@\m�@[��@['�@Zd�@Z�@Y�T@Y��@YIR@Y?}@X�@X�@W�@Wg�@WS@V�'@V�@U(�@T�p@T~(@Th�@T2�@T�@S��@R��@Rxl@Q��@Q<6@P�@P�@P�o@PH@P1'@O�@O�@@O6z@Ns�@N�@M��@MX@M�@L�@L��@L>B@K˒@Kx@K+@Jߤ@J��@Jff@J3�@I�9@I�7@I@H?�@G��@G��@Gl�@G4�@F�H@Fa|@F-@F	@E�@E�t@E�X@E�h@E!�@D�9@De�@C��@C��@CH�@B�!@B�@A��@A�M@AF@A@@A�@@��@?��@?��@?|�@?t�@?U�@?4�@>�2@>�x@>i�@>#:@=�9@=rG@=+@<��@;�@;�F@;�@:��@:��@:�x@:q�@:-@:�@9�d@9|@8�p@8�I@8��@8y>@87@7�[@7e�@6�@6�b@6��@6��@6p;@6Ta@6J�@6=q@6=q@6$�@5�@5��@5�H@5�@5zx@5=�@5�@4��@4�p@4Z@41'@3�@3o�@2�M@2��@2�@2a|@2#:@2_@1��@1��@1<6@0��@02�@0b@/�]@/��@/��@/�4@/_p@/'�@.�"@.�]@.�'@.=q@.!�@.#:@. �@-�d@-��@-�h@-�@-[W@-+�@-�@,ѷ@,��@,��@,e�@,1@+�
@+��@+�	@+qv@+y�@+dZ@+6z@*��@*�@)��@)��@)S&@)�@(�5@(Ɇ@(�e@(��@(4n@'�K@'��@'��@'�V@';d@''�@'�@&�H@&��@&�A@&l�@&!�@%��@%Y�@%8�@$��@$�D@#��@#�@"�@"��@"ff@!�@!��@!c@!Q�@!B�@!%F@ �f@ �@ ��@ �j@ Ĝ@ �@ �e@ S�@ 	�@ �@�g@�@�P@P�@�@v�@_�@Q@�C@��@�=@�h@��@�@Ft@��@��@��@>�@�}@�\@E�@�@u@	@��@�)@�@�3@�^@�=@F@��@�I@bN@9X@6@4n@�@�]@��@�}@�{@X�@6z@+@+@�@�H@�@��@i�@_�@8�@�@J@��@�j@�=@u�@�@�|@�)@�Y@D�@"h@�}@a@8@�@��@��@��@W�@($@��@��@�~@}�@rG@^�@%@Ɇ@��@_@S�@M111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AΦAα�AζFAηLAε�Aε�Aε?AίOAβaAΰ�AζzAΠ'A�x�A�:�A�SA͞�A͈1A�.A�z�A�u�A�qA�o�A�o�A�m�A�h>A�a|A�_pA�_;A�]�A�[�A�V�A�FA�#�A��A��[A�jA�p�A�1[A�A�A�/OA�XA��aA�y>A�~A�jA��'A��A�ZQA��0A��#A��	A��pA�HKA�MA�'RA�=A�"hA���A���A��iA�PHA�J�A��A��A���A�P}A��$A��A��A�!�A�4A�FA�#:A��A�"�A���A�VA���A���A�s�A���A�?�A�E�A�*0A���A�H�A���A�v�A�B�A�ƨA�?HA��RA�	�A��+A���A�FtA�bA�Y�A��A}�wAy^5As��AoE9Am��Al��Aj%�AhtTAf�.Ad	A`�A_�A^��A\��AW��AV#:AT��AQ�AK��AF�ACv`A@o�A?$�A=CA:�HA9�WA9�~A9HA9*0A9�A8��A7�VA6��A6o�A6�KA7z�A6;A5�A2-A.3�A,�$A+�A)�A)(�A(Q�A'�A&�:A&�A%��A%�nA%�EA&S&A&DgA&7LA%n/A$y�A#�A#�SA#aA"�SA#r�A#��A"�A!ĜA!��A!��A��A�xA(�A�fA8�A/�A�+A�zA�A��A�A��A�Ai�A6�AE�AH�A9XA��A8�A�A%FAe�A�EA]dA�A��Aa|A�A�qA�A��A=�A�vAVA
ݘA
�A
�KA
�A	�gA	o A�fA��AԕA^5A�A�qAU�A{A��A1'A�A�A��A��A;�A�A�IAa�A�'A=�ASA ԕA �A A @���@�'�@���@�u@�c@�ԕ@�O�@���@�Ta@�J@���@��@�n�@��@�(@�Ɇ@��h@�o@�y�@�[�@�D@��.@�j@���@�'�@�S�@�e@��@��z@�[@�m]@�Ɇ@�@��@�G@��@���@��@��f@�A@��@�U2@�S@�-�@�@�P@�S@�ݘ@唯@��8@�!@��@�/@�H@�e�@�.I@�v`@ⷀ@�&@ߜ@ߧ�@�9�@���@��@�e@�0U@�zx@��B@�H�@�  @���@��@�!@ق�@�Q�@�@�ں@�tT@ׅ�@�33@��@��|@�{�@չ�@�]�@���@�$@ӏ�@��@��@�g�@Ч�@�n/@���@�q�@�8�@�@��Z@�X�@��	@���@��v@��@�@�xl@�˒@��@�*�@ƌ@��T@žw@ōP@�S�@Ƶ�@Ƈ+@�N<@���@�@Ė�@�8�@Ã{@+@�[�@�!�@�U�@��@�w�@�YK@��@�u@�c@�,�@�,�@�0�@�Vm@�S�@�a@�C@���@���@��>@�L�@���@�B�@��#@�n/@��h@�m�@��@���@��@���@�(�@���@���@��@��@���@��@��@��F@��@���@��@���@�%�@��k@�/@���@��@���@��@��4@��@��[@�z�@�0U@��A@���@��7@�dZ@�Z�@���@��@@�o�@�Vm@�B�@�o@���@���@���@�F@��@���@�+k@��-@�`B@���@��c@���@�l"@�1'@��q@���@�S&@��@��,@���@��@�#:@��k@���@�,�@���@�.�@���@���@�j@���@��@���@��@��o@�j�@��@��0@��a@�%@�}V@��h@�A�@��3@�S&@�*0@���@�5�@�	l@��@�H�@�O@�<�@�/�@� �@��@��V@�@O@���@��,@�v�@�(�@���@��C@�k�@��@�g8@�M�@�7�@��@���@��S@�n/@�'�@�%@���@��<@���@�Ta@�($@�  @��@��q@�Z�@�1�@�@@��8@��X@���@�ff@�bN@�J�@���@��}@��H@���@�t�@��@���@�]d@��@��}@��F@�zx@�F@�ߤ@�z�@�+k@�O@���@���@�n/@�&�@��@�l�@�J�@�#:@��@���@���@�C�@���@�bN@��@���@���@�j�@�=@���@��1@�Ov@��z@�F@��e@�J�@�?�@��&@��@�X�@���@��1@�p;@�9X@�4@��]@���@���@��@�@��[@�@�@�7@�4@��]@���@�˒@��@�qv@�)_@���@�xl@�6@��D@��k@��8@�v�@�@O@U�@(@~p;@}�7@}7L@}(�@|�@|��@|�_@|�@{ƨ@{n/@{�@zq�@y��@y�@y�@x��@xq@w�q@w.I@vȴ@v�@vh
@v)�@up�@t�j@tM@s�@r��@r�1@rZ�@q��@q#�@q%@p�f@pɆ@p�$@p~(@pj@p[�@p>B@p�@o��@o��@oP�@o@n�b@nE�@m�@m��@m&�@l�)@l�$@lm�@lZ@l  @k��@k33@j��@i�j@i�M@i(�@h�o@h:�@hM@g��@g�[@g�{@g'�@f��@f)�@e�H@e��@e\�@eX@e�@d�/@d~(@d@cƨ@c�V@cv`@c1�@b�@b��@bQ@b5?@aԕ@a��@a\�@a(�@`�@`�p@`��@`7�@_��@_e�@_@^i�@]�'@]�@\��@\��@\m�@[��@['�@Zd�@Z�@Y�T@Y��@YIR@Y?}@X�@X�@W�@Wg�@WS@V�'@V�@U(�@T�p@T~(@Th�@T2�@T�@S��@R��@Rxl@Q��@Q<6@P�@P�@P�o@PH@P1'@O�@O�@@O6z@Ns�@N�@M��@MX@M�@L�@L��@L>B@K˒@Kx@K+@Jߤ@J��@Jff@J3�@I�9@I�7@I@H?�@G��@G��@Gl�@G4�@F�H@Fa|@F-@F	@E�@E�t@E�X@E�h@E!�@D�9@De�@C��@C��@CH�@B�!@B�@A��@A�M@AF@A@@A�@@��@?��@?��@?|�@?t�@?U�@?4�@>�2@>�x@>i�@>#:@=�9@=rG@=+@<��@;�@;�F@;�@:��@:��@:�x@:q�@:-@:�@9�d@9|@8�p@8�I@8��@8y>@87@7�[@7e�@6�@6�b@6��@6��@6p;@6Ta@6J�@6=q@6=q@6$�@5�@5��@5�H@5�@5zx@5=�@5�@4��@4�p@4Z@41'@3�@3o�@2�M@2��@2�@2a|@2#:@2_@1��@1��@1<6@0��@02�@0b@/�]@/��@/��@/�4@/_p@/'�@.�"@.�]@.�'@.=q@.!�@.#:@. �@-�d@-��@-�h@-�@-[W@-+�@-�@,ѷ@,��@,��@,e�@,1@+�
@+��@+�	@+qv@+y�@+dZ@+6z@*��@*�@)��@)��@)S&@)�@(�5@(Ɇ@(�e@(��@(4n@'�K@'��@'��@'�V@';d@''�@'�@&�H@&��@&�A@&l�@&!�@%��@%Y�@%8�@$��@$�D@#��@#�@"�@"��@"ff@!�@!��@!c@!Q�@!B�@!%F@ �f@ �@ ��@ �j@ Ĝ@ �@ �e@ S�@ 	�@ �@�g@�@�P@P�@�@v�@_�@Q@�C@��@�=@�h@��@�@Ft@��@��@��@>�@�}@�\@E�@�@u@	@��@�)@�@�3@�^@�=@F@��@�I@bN@9X@6@4n@�@�]@��@�}@�{@X�@6z@+@+@�@�H@�@��@i�@_�@8�@�@J@��@�j@�=@u�@�@�|@�)@�Y@D�@"h@�}@a@8@�@��@��@��@W�@($@��@��@�~@}�@rG@^�@%@Ɇ@��@_@S�@M111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
}�B
}�B
~B
}�B
}�B
}�B
}�B
}�B
~B
}B
}VB
{�B
v�B
r�B
o�B
hsB
g�B
g8B
gB
f�B
e�B
e�B
e�B
e�B
e,B
dZB
dZB
d�B
d�B
d�B
e�B
e�B
d@B
b�B
\�B
R B
CB
O�B
@ B
2�B
*�B
1'B
K�B
H�B
M6B
m�B
�?B
��B
��B
��B�BTB:BdZBlqBw�B�AB�?B�B��B��B��B�9B��B�-B�iB��B�MB�B�B�B��B��B�{B{�B��B��B��B��B�|B�]B��B�#B��B�sB�B��B�B�B��B��Bj�B1B
=B
��B
��B
f�B
NpB
B	��B	�B	��B	��B	��B	�wB	�|B	��B	�JB	B	p�B	e`B	b�B	W
B	D�B	5�B	,WB	"NB	
#B��B�FB��B�B�B	�B	'B	*�B	*�B	)�B	)*B	,"B	=VB	K)B	V�B	}<B	�B	��B	��B	�&B	r�B	c�B	abB	\]B	_B	`\B	Z�B	^OB	h�B	r�B	y	B	��B	��B	��B	�iB	�"B	��B	�B	��B	��B	��B	�{B	�{B	�B	˒B	�(B	ѷB	�=B	��B	��B	�$B	��B	��B	�PB	�.B	̈́B	ɺB	�tB	��B	��B	�%B	�XB	��B	ˬB	��B	�+B	��B	�LB	�B	� B	��B	�}B	�B	�B	��B	��B	�dB	��B	��B	��B	� B	��B	��B	�oB	�GB	�3B	��B	��B	��B	� B	�iB	��B	� B	��B	��B	��B	�qB	��B	�HB	��B	��B	��B	��B	��B	��B	��B	��B	�FB	�2B	��B	��B	��B	��B	�;B	��B	B	��B	ΥB	�VB	�\B	�.B	�SB	ٚB	��B	ٴB	�_B	�B	��B	�B	�,B	�TB	��B	�zB	�,B	��B	��B
�B
%B
MB
B
�B
	7B
	�B
	�B
	B
�B
�B	�B	�PB	��B	��B	��B	�$B	�B	�KB	�B	�qB	�B	�B	��B	�B	�B	��B	�B	�OB	�cB	�CB	�B	�B	�B	�)B	�*B	�=B	�B	�]B	��B	�5B	��B	�aB	�B	�B	��B	�GB	�B	�oB	�oB	�!B	��B	�B	�B	��B	��B	��B	�'B	�[B	�AB	��B	�UB	�B	�B	�-B	��B	�AB	�B	��B	�WB	�6B	�/B	��B	�B	�!B	�B	�'B	�B	��B	�/B	�B	�B	�B	��B	�2B	�ZB	�B	�ZB	�B	��B	�}B	�B	�=B	�UB	�!B	�B	�wB	�B	�B	�B	�)B	��B	�QB	��B	�B	�B	�B	�B	�B	��B	�B	�9B	�B	��B	��B	�	B	��B	��B	��B	�B	��B	�/B	�CB	�WB	�kB	��B	�B	�XB	��B	�RB	�XB	�B	��B	�B	�kB	�eB	�yB	�_B	�B	�_B	�>B	�B	�B	��B	�6B	�kB	�)B	�B	��B	�B	�IB	�B	�B	�B	�;B	��B	�B	�B	�hB	�B	��B	��B	��B	��B	��B	�tB	�B	�zB	�?B	�zB	��B	�2B	�B	��B	��B	�>B	�$B	��B	�*B	��B	�0B	��B	�PB	�<B	��B	��B	�qB	�B	��B	��B	��B	��B
 OB
 �B
�B
�B
HB
�B
	�B
1B
�B
gB
�B
aB
aB
�B
%B
�B
�B
�B
�B
DB
DB
�B
�B
�B
�B
B
�B
FB
�B
gB
�B
B
�B
B

B
$B
�B
sB
B
�B
+B
�B
�B
1B
�B
�B
�B
�B
�B
7B
�B
�B
�B
	B
�B
�B
B
�B
#B
�B
�B
�B
�B
�B
�B
B
OB
�B
�B
�B
�B
�B
VB
VB
�B
 B
�B
 vB
 �B
 �B
!B
!bB
 �B
!HB
!HB
!�B
"B
!�B
"�B
"4B
#B
"�B
#�B
#�B
$B
$�B
$@B
$�B
$�B
$�B
%,B
%�B
%�B
&�B
'B
'8B
'�B
'8B
(
B
(sB
(�B
(�B
)B
)*B
)�B
)�B
*B
)�B
)yB
)yB
+B
*eB
,B
+�B
+�B
+�B
,"B
,"B
,WB
,�B
-)B
-)B
-wB
-�B
.�B
.�B
/�B
0;B
0�B
1'B
1AB
1�B
0�B
0;B
0;B
0;B
0;B
0;B
2-B
2|B
2GB
2GB
2�B
3MB
49B
3�B
4nB
5tB
4�B
6`B
6�B
6�B
6�B
6�B
72B
6�B
72B
8lB
8B
7�B
8lB
9$B
:B
;JB
:�B
;0B
;�B
;�B
<PB
;�B
;�B
;�B
;B
;dB
<PB
<PB
<�B
=<B
>BB
=�B
=�B
?B
>�B
>]B
>�B
>�B
?HB
>�B
?HB
?�B
@ B
@OB
@�B
@�B
A B
A;B
AUB
A�B
A�B
BB
BAB
B[B
B�B
B�B
B�B
B�B
CGB
B�B
CGB
C{B
C�B
C�B
C�B
D3B
DB
DMB
EB
D�B
E9B
D�B
EmB
D�B
E�B
EmB
E�B
E�B
F%B
F?B
F%B
F�B
G�B
G+B
HB
G�B
G�B
H�B
H�B
H�B
I7B
I7B
I�B
J#B
IlB
I�B
J�B
I�B
J#B
J�B
JrB
KB
K^B
LB
LJB
K�B
LdB
LdB
M6B
M�B
N�B
N�B
N�B
OB
O�B
OvB
O�B
O�B
O�B
P.B
PbB
P�B
P}B
QNB
QNB
P�B
QNB
Q4B
Q�B
R�B
R:B
R�B
S&B
R�B
S�B
S&B
S�B
S�B
TB
T{B
T�B
T�B
UB
UgB
UgB
U�B
U�B
VmB
VSB
VSB
V�B
VmB
V�B
V�B
V�B
W�B
WYB
W�B
X+B
X_B
XyB
X�B
YKB
X�B
X�B
YeB
YB
Y�B
ZB
YKB
Z7B
ZB
Z�B
[#B
[#B
[qB
[WB
[�B
[�B
\CB
\�B
\xB
]~B
]�B
]/B
]dB
\�B
]�B
]dB
^5B
]�B
^OB
^�B
^�B
^�B
^�B
_�B
_pB
`�B
`vB
`�B
_�B
`BB
`\B
`�B
`�B
`B
`BB
`�B
`�B
`�B
`�B
aHB
aHB
a�B
aB
aB
a|B
a�B
a�B
a�B
b�B
bNB
b�B
b�B
b�B
b�B
c B
b�B
cB
cnB
dZB
d&B
c�B
d@B
d�B
d@B
d�B
e,B
d�B
ezB
d�B
f2B
fB
e�B
f2B
fB
fLB
f2B
ffB
f�B
f�B
f�B
f�B
g8B
gB
g�B
g�B
g8B
gmB
g�B
h�B
h
B
h$B
h$B
h�B
h�B
iDB
i�B
i�B
i�B
i�B
j0B
i�B
i�B
jB
j�B
j�B
j�B
jeB
kkB
k�B
k�B
k�B
lB
k�B
l"B
k�B
l�B
mCB
lqB
m)B
mCB
n/B
n/B
n�B
nB
n}B
n�B
o�B
oOB
oOB
oOB
oiB
pB
o5B
o�B
o�B
oiB
o�B
o5B
pB
p�B
p!B
poB
p�B
poB
p�B
q�B
q[B
q�B
q�B
raB
rGB
rB
r�B
rB
s�B
shB
s�B
s�B
s�B
tTB
uB
t�B
u?B
u�B
u�B
uB
utB
utB
uB
u?B
u�B
uZB
u�B
v�B
v`B
v�B
wfB
v�B
vzB
wfB
w�B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
xB
w�B
xB
xRB
xB
xB
xRB
x�B
xRB
x�B
x�B
y	B
x�B
y�B
y$B
y>B
y�B
zB
zB
z^B
z�B
{B
{JB
{dB
{dB
{0B
{�B
{�B
|6B
|B
|�B
|�B
|�B
|�B
|�B
}VB
}qB
}�B
}�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
}�B
}�B
~B
}�B
}�B
}�B
}�B
}�B
~B
}B
}VB
{�B
v�B
r�B
o�B
hsB
g�B
g8B
gB
f�B
e�B
e�B
e�B
e�B
e,B
dZB
dZB
d�B
d�B
d�B
e�B
e�B
d@B
b�B
\�B
R B
CB
O�B
@ B
2�B
*�B
1'B
K�B
H�B
M6B
m�B
�?B
��B
��B
��B�BTB:BdZBlqBw�B�AB�?B�B��B��B��B�9B��B�-B�iB��B�MB�B�B�B��B��B�{B{�B��B��B��B��B�|B�]B��B�#B��B�sB�B��B�B�B��B��Bj�B1B
=B
��B
��B
f�B
NpB
B	��B	�B	��B	��B	��B	�wB	�|B	��B	�JB	B	p�B	e`B	b�B	W
B	D�B	5�B	,WB	"NB	
#B��B�FB��B�B�B	�B	'B	*�B	*�B	)�B	)*B	,"B	=VB	K)B	V�B	}<B	�B	��B	��B	�&B	r�B	c�B	abB	\]B	_B	`\B	Z�B	^OB	h�B	r�B	y	B	��B	��B	��B	�iB	�"B	��B	�B	��B	��B	��B	�{B	�{B	�B	˒B	�(B	ѷB	�=B	��B	��B	�$B	��B	��B	�PB	�.B	̈́B	ɺB	�tB	��B	��B	�%B	�XB	��B	ˬB	��B	�+B	��B	�LB	�B	� B	��B	�}B	�B	�B	��B	��B	�dB	��B	��B	��B	� B	��B	��B	�oB	�GB	�3B	��B	��B	��B	� B	�iB	��B	� B	��B	��B	��B	�qB	��B	�HB	��B	��B	��B	��B	��B	��B	��B	��B	�FB	�2B	��B	��B	��B	��B	�;B	��B	B	��B	ΥB	�VB	�\B	�.B	�SB	ٚB	��B	ٴB	�_B	�B	��B	�B	�,B	�TB	��B	�zB	�,B	��B	��B
�B
%B
MB
B
�B
	7B
	�B
	�B
	B
�B
�B	�B	�PB	��B	��B	��B	�$B	�B	�KB	�B	�qB	�B	�B	��B	�B	�B	��B	�B	�OB	�cB	�CB	�B	�B	�B	�)B	�*B	�=B	�B	�]B	��B	�5B	��B	�aB	�B	�B	��B	�GB	�B	�oB	�oB	�!B	��B	�B	�B	��B	��B	��B	�'B	�[B	�AB	��B	�UB	�B	�B	�-B	��B	�AB	�B	��B	�WB	�6B	�/B	��B	�B	�!B	�B	�'B	�B	��B	�/B	�B	�B	�B	��B	�2B	�ZB	�B	�ZB	�B	��B	�}B	�B	�=B	�UB	�!B	�B	�wB	�B	�B	�B	�)B	��B	�QB	��B	�B	�B	�B	�B	�B	��B	�B	�9B	�B	��B	��B	�	B	��B	��B	��B	�B	��B	�/B	�CB	�WB	�kB	��B	�B	�XB	��B	�RB	�XB	�B	��B	�B	�kB	�eB	�yB	�_B	�B	�_B	�>B	�B	�B	��B	�6B	�kB	�)B	�B	��B	�B	�IB	�B	�B	�B	�;B	��B	�B	�B	�hB	�B	��B	��B	��B	��B	��B	�tB	�B	�zB	�?B	�zB	��B	�2B	�B	��B	��B	�>B	�$B	��B	�*B	��B	�0B	��B	�PB	�<B	��B	��B	�qB	�B	��B	��B	��B	��B
 OB
 �B
�B
�B
HB
�B
	�B
1B
�B
gB
�B
aB
aB
�B
%B
�B
�B
�B
�B
DB
DB
�B
�B
�B
�B
B
�B
FB
�B
gB
�B
B
�B
B

B
$B
�B
sB
B
�B
+B
�B
�B
1B
�B
�B
�B
�B
�B
7B
�B
�B
�B
	B
�B
�B
B
�B
#B
�B
�B
�B
�B
�B
�B
B
OB
�B
�B
�B
�B
�B
VB
VB
�B
 B
�B
 vB
 �B
 �B
!B
!bB
 �B
!HB
!HB
!�B
"B
!�B
"�B
"4B
#B
"�B
#�B
#�B
$B
$�B
$@B
$�B
$�B
$�B
%,B
%�B
%�B
&�B
'B
'8B
'�B
'8B
(
B
(sB
(�B
(�B
)B
)*B
)�B
)�B
*B
)�B
)yB
)yB
+B
*eB
,B
+�B
+�B
+�B
,"B
,"B
,WB
,�B
-)B
-)B
-wB
-�B
.�B
.�B
/�B
0;B
0�B
1'B
1AB
1�B
0�B
0;B
0;B
0;B
0;B
0;B
2-B
2|B
2GB
2GB
2�B
3MB
49B
3�B
4nB
5tB
4�B
6`B
6�B
6�B
6�B
6�B
72B
6�B
72B
8lB
8B
7�B
8lB
9$B
:B
;JB
:�B
;0B
;�B
;�B
<PB
;�B
;�B
;�B
;B
;dB
<PB
<PB
<�B
=<B
>BB
=�B
=�B
?B
>�B
>]B
>�B
>�B
?HB
>�B
?HB
?�B
@ B
@OB
@�B
@�B
A B
A;B
AUB
A�B
A�B
BB
BAB
B[B
B�B
B�B
B�B
B�B
CGB
B�B
CGB
C{B
C�B
C�B
C�B
D3B
DB
DMB
EB
D�B
E9B
D�B
EmB
D�B
E�B
EmB
E�B
E�B
F%B
F?B
F%B
F�B
G�B
G+B
HB
G�B
G�B
H�B
H�B
H�B
I7B
I7B
I�B
J#B
IlB
I�B
J�B
I�B
J#B
J�B
JrB
KB
K^B
LB
LJB
K�B
LdB
LdB
M6B
M�B
N�B
N�B
N�B
OB
O�B
OvB
O�B
O�B
O�B
P.B
PbB
P�B
P}B
QNB
QNB
P�B
QNB
Q4B
Q�B
R�B
R:B
R�B
S&B
R�B
S�B
S&B
S�B
S�B
TB
T{B
T�B
T�B
UB
UgB
UgB
U�B
U�B
VmB
VSB
VSB
V�B
VmB
V�B
V�B
V�B
W�B
WYB
W�B
X+B
X_B
XyB
X�B
YKB
X�B
X�B
YeB
YB
Y�B
ZB
YKB
Z7B
ZB
Z�B
[#B
[#B
[qB
[WB
[�B
[�B
\CB
\�B
\xB
]~B
]�B
]/B
]dB
\�B
]�B
]dB
^5B
]�B
^OB
^�B
^�B
^�B
^�B
_�B
_pB
`�B
`vB
`�B
_�B
`BB
`\B
`�B
`�B
`B
`BB
`�B
`�B
`�B
`�B
aHB
aHB
a�B
aB
aB
a|B
a�B
a�B
a�B
b�B
bNB
b�B
b�B
b�B
b�B
c B
b�B
cB
cnB
dZB
d&B
c�B
d@B
d�B
d@B
d�B
e,B
d�B
ezB
d�B
f2B
fB
e�B
f2B
fB
fLB
f2B
ffB
f�B
f�B
f�B
f�B
g8B
gB
g�B
g�B
g8B
gmB
g�B
h�B
h
B
h$B
h$B
h�B
h�B
iDB
i�B
i�B
i�B
i�B
j0B
i�B
i�B
jB
j�B
j�B
j�B
jeB
kkB
k�B
k�B
k�B
lB
k�B
l"B
k�B
l�B
mCB
lqB
m)B
mCB
n/B
n/B
n�B
nB
n}B
n�B
o�B
oOB
oOB
oOB
oiB
pB
o5B
o�B
o�B
oiB
o�B
o5B
pB
p�B
p!B
poB
p�B
poB
p�B
q�B
q[B
q�B
q�B
raB
rGB
rB
r�B
rB
s�B
shB
s�B
s�B
s�B
tTB
uB
t�B
u?B
u�B
u�B
uB
utB
utB
uB
u?B
u�B
uZB
u�B
v�B
v`B
v�B
wfB
v�B
vzB
wfB
w�B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
xB
w�B
xB
xRB
xB
xB
xRB
x�B
xRB
x�B
x�B
y	B
x�B
y�B
y$B
y>B
y�B
zB
zB
z^B
z�B
{B
{JB
{dB
{dB
{0B
{�B
{�B
|6B
|B
|�B
|�B
|�B
|�B
|�B
}VB
}qB
}�B
}�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104920  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173822  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173823  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173823                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023830  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023830  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                